open Rresult

let ( <.> ) f g x = f (g x)

(* XXX(dinosaure): (c) Hannes Menhert, this code is [tls_mirage.ml]
 * with the possibility to define your [+'a io]. *)

module type FLOW = sig
  type t

  type error

  type +'a io

  val read :
    t -> bytes -> int -> int -> ([ `End | `Len of int ], error) result io

  val fully_write : t -> string -> int -> int -> (unit, error) result io

  val close : t -> unit io

  val bind : 'a io -> ('a -> 'b io) -> 'b io

  val map : ('a -> 'b) -> 'a io -> 'b io

  val return : 'a -> 'a io
end

module Make (Flow : FLOW) = struct
  type error =
    | Alert of Tls.Packet.alert_type
    | Failure of Tls.Engine.failure
    | Flow_error of Flow.error
    | Closed

  let ( >>= ) = Flow.bind

  let ( >>| ) x f = Flow.map f x

  let return = Flow.return

  type t = {
    socket : Flow.t;
    mutable state : [ `Active of Tls.Engine.state | `Eof | `Error of error ];
    mutable linger : Cstruct.t list;
  }

  let fully_write socket ({ Cstruct.len; _ } as cs) =
    Flow.fully_write socket (Cstruct.to_string cs) 0 len
    >>| R.reword_error (fun err -> Flow_error err)

  let read socket =
    let buf = Bytes.create 0x1000 in
    Flow.read socket buf 0 (Bytes.length buf) >>= function
    | Ok `End -> return `Eof
    | Ok (`Len len) -> return (`Data (Cstruct.of_bytes ~off:0 ~len buf))
    | Error err -> return (`Error (Flow_error err))

  let check_write flow f_res =
    ( match flow.state, f_res with
      | `Active _, Error err ->
          flow.state <- `Error err ; Flow.close flow.socket
      | _ -> return () ) >>| fun () ->
    match f_res with
    | Ok ()   -> Ok ()
    | Error e -> Error e

  let read_react flow =

    let handle tls buf =
      match Tls.Engine.handle_tls tls buf with
      | Ok (res, `Response resp, `Data data) ->
          flow.state <- ( match res with
            | `Ok tls      -> `Active tls
            | `Eof         -> `Eof
            | `Alert alert -> `Error (Alert alert)) ;
          ( match resp with
            | None -> return (Ok ())
            | Some buf -> fully_write flow.socket buf >>= check_write flow) >>= fun _ ->
          ( match res with
            | `Ok _ -> return ()
            | _ -> Flow.close flow.socket) >>= fun () ->
          return @@ `Ok data
      | Error (fail, `Response resp) ->
          let r = `Error (Failure fail) in
          flow.state <- r ;
          fully_write flow.socket resp |> fun _ -> Flow.close flow.socket >>= fun () -> return r
    in
    match flow.state with
    | `Eof | `Error _ as e -> return e
    | `Active _            ->
      read flow.socket >>=
      function
      | `Eof | `Error _ as e -> flow.state <- e ; return e
      | `Data buf            -> match flow.state with
        | `Active tls          -> handle tls buf
        | `Eof | `Error _ as e -> return e

  let rec read flow =
    match flow.linger with
    | [] ->
      ( read_react flow >>= function
          | `Ok None       -> read flow
          | `Ok (Some buf) -> return (Result.Ok (`Data buf))
          | `Eof           -> return (Result.Ok `Eof)
          | `Error e       -> return (Result.Error e))
    | bufs ->
      flow.linger <- [] ;
      return @@ Ok (`Data (Cstruct.concat @@ List.rev bufs))

  let writev flow bufs =
    match flow.state with
    | `Eof -> return (Result.Error Closed)
    | `Error err -> return (Result.Error err)
    | `Active tls ->
        match Tls.Engine.send_application_data tls bufs with
        | Some (tls, answer) ->
            flow.state <- `Active tls ;
            fully_write flow.socket answer >>= check_write flow
        | None ->
            (* "Impossible" due to handshake draining. *)
            assert false

  let write flow cs = writev flow [ cs ]

  let close flow =
    match flow.state with
    | `Active tls ->
      flow.state <- `Eof ;
      let _, buf = Tls.Engine.send_close_notify tls in
      fully_write flow.socket buf >>= fun _ -> return ()
    | _ -> return ()

  let rec drain_handshake flow =
    match flow.state with
    | `Active tls when not (Tls.Engine.handshake_in_progress tls) ->
        return (Ok flow)
    | _ -> (
        read_react flow >>= function
        | `Ok (Some mbuf) ->
            flow.linger <- mbuf :: flow.linger ;
            drain_handshake flow
        | `Ok None -> drain_handshake flow
        | `Error err -> return (Result.Error err)
        | `Eof -> return (Result.Error Closed))

  let init_client cfg socket =
    let tls, init = Tls.Engine.client cfg in
    let flow = { socket; state = `Active tls; linger = [] } in
    fully_write socket init >>= fun _ -> drain_handshake flow

  let init_server cfg socket =
    let flow =
      { socket; state = `Active (Tls.Engine.server cfg); linger = [] } in
    drain_handshake flow
end
