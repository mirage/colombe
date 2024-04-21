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
    mutable state : [ `Active of Tls.Engine.state
                    | `Read_closed of Tls.Engine.state
                    | `Write_closed of Tls.Engine.state
                    | `Closed
                    | `Error of error ];
    mutable linger : Cstruct.t list;
  }

  let fully_write socket ({ Cstruct.len; _ } as cs) =
    Flow.fully_write socket (Cstruct.to_string cs) 0 len

  let read socket =
    let buf = Bytes.create 0x1000 in
    Flow.read socket buf 0 (Bytes.length buf) >>| function
    | Ok `End -> Ok `Eof
    | Ok (`Len len) -> Ok (`Data (Cstruct.of_bytes buf ~off:0 ~len))
    | Error _ as err -> err

  let half_close state mode = match state, mode with
    | `Active tls, `read -> `Read_closed tls
    | `Active tls, `write -> `Write_closed tls
    | `Active _, `read_write -> `Closed
    | `Read_closed tls, `read -> `Read_closed tls
    | `Read_closed _, (`write | `read_write) -> `Closed
    | `Write_closed tls, `write -> `Write_closed tls
    | `Write_closed _, (`read | `read_write) -> `Closed
    | (`Closed | `Error _) as e, (`read | `write | `read_write) -> e

  let inject_state tls = function
    | `Active _ -> `Active tls
    | `Read_closed _ -> `Read_closed tls
    | `Write_closed _ -> `Write_closed tls
    | (`Closed | `Error _) as e -> e

  let write_flow flow buf =
    fully_write flow.socket buf >>= function
    | Ok _ as o -> return o
    | Error e ->
      flow.state <- `Error (Flow_error e);
      return (Error (Flow_error e))

  let read_react flow =

    let handle tls buf =
      match Tls.Engine.handle_tls tls buf with
      | Ok (state, eof, `Response resp, `Data data) ->
          let state = inject_state state flow.state in
          let state = Option.(value ~default:state (map (fun `Eof -> half_close state `read) eof)) in
          flow.state <- state;
          ( match resp with
            | None -> return (Ok ())
            | Some buf -> write_flow flow buf) >>= fun _ ->
          return @@ `Ok data
      | Error (fail, `Response resp) ->
          let r = `Error (Failure fail) in
          flow.state <- r ;
          fully_write flow.socket resp >>= fun _ -> return r
    in
    match flow.state with
    | `Error _ as e -> return e
    | `Read_closed _ | `Closed -> return `Eof
    | `Active _ | `Write_closed _ ->
      read flow.socket >>= function
      | Error e -> flow.state <- `Error (Flow_error e); return (`Error (Flow_error e))
      | Ok `Eof ->
        flow.state <- half_close flow.state `read;
        return `Eof
      | Ok `Data buf -> match flow.state with
        | `Active tls | `Write_closed tls -> handle tls buf
        | `Read_closed _ | `Closed -> return `Eof
        | `Error _ as e -> return e

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
    | `Closed | `Write_closed _ -> return (Result.Error Closed)
    | `Error err -> return (Result.Error err)
    | `Active tls | `Read_closed tls ->
        match Tls.Engine.send_application_data tls bufs with
        | Some (tls, answer) ->
            flow.state <- `Active tls ;
            write_flow flow answer
        | None ->
            (* "Impossible" due to handshake draining. *)
            assert false

  let write flow cs = writev flow [ cs ]

  let close flow =
    ( match flow.state with
    | `Active tls | `Read_closed tls ->
      let tls, buf = Tls.Engine.send_close_notify tls in
      flow.state <- inject_state tls flow.state;
      flow.state <- `Closed;
      fully_write flow.socket buf >>= fun _ -> return ()
    | `Write_closed _ ->
      flow.state <- `Closed;
      return ()
    | _ -> return () ) >>= fun () ->
    Flow.close flow.socket

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
