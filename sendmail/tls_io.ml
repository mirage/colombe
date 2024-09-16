[@@@ocamlformat "disable"]

(* XXX(dinosaure): (c) Hannes Menhert, this code is [tls_mirage.ml]
 * with the possibility to define your [+'a io]. *)

module type FLOW = sig
  type +'a io

  type t and error = private [> `Closed ]

  val read : t -> ?off:int -> ?len:int -> bytes -> ([ `End | `Len of int ], error) result io
  val write : t -> ?off:int -> ?len:int -> string -> (unit, error) result io
  val close : t -> unit io
  val bind : 'a io -> ('a -> 'b io) -> 'b io
  val map : ('a -> 'b) -> 'a io -> 'b io
  val return : 'a -> 'a io
  val pp_error : error Fmt.t
end

module Make (Flow : FLOW) = struct
  let ( >>= ) = Flow.bind and ( >|= ) x f = Flow.map f x

  type error  = [ `Tls_alert   of Tls.Packet.alert_type
                | `Tls_failure of Tls.Engine.failure
                | `Flow of Flow.error
                | `Closed ]

  let pp_error ppf = function
    | `Tls_failure f -> Tls.Engine.pp_failure ppf f
    | `Tls_alert a   -> Fmt.string ppf @@ Tls.Packet.alert_type_to_string a
    | `Flow e        -> Flow.pp_error ppf e

  type t = {
    role           : [ `Server | `Client ] ;
    flow           : Flow.t ;
    mutable state  : [ `Active of Tls.Engine.state
                     | `Read_closed of Tls.Engine.state
                     | `Write_closed of Tls.Engine.state
                     | `Closed
                     | `Error of error ] ;
    mutable linger : string list ;
  }

  let half_close state mode =
    match state, mode with
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

  let tls_alert a = `Error (`Tls_alert a)
  let tls_fail f  = `Error (`Tls_failure f)

  let write_flow flow buf =
    Flow.write flow.flow buf >>= function
    | Ok _ as o -> Flow.return o
    | Error `Closed ->
      flow.state <- half_close flow.state `write;
      Flow.return (Error (`Flow `Closed))
    | Error e ->
      flow.state <- `Error (`Flow e);
      Flow.return (Error (`Flow e))

  let read_react flow =
    let handle tls buf =
      match Tls.Engine.handle_tls tls buf with
      | Ok (state, eof, `Response resp, `Data data) ->
        let state = inject_state state flow.state in
        let state = Option.(value ~default:state (map (fun `Eof -> half_close state `read) eof)) in
        flow.state <- state;
        ( match resp with
          | None     -> Flow.return (Ok ())
          | Some buf -> write_flow flow buf) >>= fun _ ->
        Flow.return (`Ok data)
      | Error (fail, `Response resp) ->
        let reason = match fail with
          | `Alert a -> tls_alert a
          | f -> tls_fail f
        in
        flow.state <- reason ;
        Flow.write flow.flow resp >>= fun _ ->
        Flow.return reason
    in
    match flow.state with
    | `Error _ as e -> Flow.return e
    | `Read_closed _ | `Closed -> Flow.return `Eof
    | `Active _ | `Write_closed _ ->
      let buf = Bytes.create 0x800 in
      Flow.read flow.flow buf >>= function
      | Error e ->
        flow.state <- `Error (`Flow e);
        Flow.return (`Error (`Flow e))
      | Ok `End ->
        flow.state <- half_close flow.state `read;
        Flow.return `Eof
      | Ok `Len len -> match flow.state with
        | `Active tls | `Write_closed tls -> handle tls (Bytes.sub_string buf 0 len)
        | `Read_closed _ | `Closed -> Flow.return `Eof
        | `Error _ as e -> Flow.return e

  let rec read flow =
    match flow.linger with
    | [] ->
      ( read_react flow >>= function
          | `Ok None       -> read flow
          | `Ok (Some buf) -> Flow.return (Ok (`Data buf))
          | `Eof           -> Flow.return (Ok `Eof)
          | `Error e       -> Flow.return (Error e ))
    | bufs ->
      flow.linger <- [] ;
      let str = String.concat "" (List.rev bufs) in
      Flow.return (Ok (`Data str))

  let writev flow bufs =
    match flow.state with
    | `Closed | `Write_closed _ -> Flow.return (Error `Closed)
    | `Error e -> Flow.return (Error (e :> error))
    | `Active tls | `Read_closed tls ->
        match Tls.Engine.send_application_data tls bufs with
        | Some (tls, answer) ->
            flow.state <- `Active tls ;
            write_flow flow answer
        | None ->
            (* "Impossible" due to handshake draining. *)
            assert false

  let write flow buf = writev flow [buf]

  (*
   * XXX bad XXX
   * This is a point that should particularly be protected from concurrent r/w.
   * Doing this before a `t` is returned is safe; redoing it during rekeying is
   * not, as the API client already sees the `t` and can mistakenly interleave
   * writes while this is in progress.
   * *)
  let rec drain_handshake flow =
    match flow.state with
    | `Active tls when not (Tls.Engine.handshake_in_progress tls) ->
        Flow.return (Ok flow)
    | _ ->
      (* read_react re-throws *)
        read_react flow >>= function
        | `Ok mbuf ->
            flow.linger <- Option.(to_list mbuf) @ flow.linger ;
            drain_handshake flow
        | `Error e -> Flow.return (Error (e :> error))
        | `Eof     -> Flow.return (Error `Closed)

  let underlying flow = flow.flow

  (*
  let reneg ?authenticator ?acceptable_cas ?cert ?(drop = true) flow =
    match flow.state with
    | `Closed | `Write_closed _ | `Read_closed _ -> Lwt.return @@ Error `Closed
    | `Error e    -> Lwt.return @@ Error (e :> wr_or_msg)
    | `Active tls ->
        match Tls.Engine.reneg ?authenticator ?acceptable_cas ?cert tls with
        | None             -> Lwt.return (Error (`Msg "Renegotiation already in progress"))
        | Some (tls', buf) ->
            if drop then flow.linger <- [] ;
            flow.state <- `Active tls' ;
            write_flow flow buf >>= fun _ ->
            drain_handshake flow >|= function
            | Ok _    -> Ok ()
            | Error e -> Error (e :> wr_or_msg)

  let key_update ?request flow =
    match flow.state with
    | `Closed | `Write_closed _ -> Lwt.return @@ Error `Closed
    | `Error e    -> Lwt.return @@ Error (e :> wr_or_msg)
    | `Active tls | `Read_closed tls ->
      match Tls.Engine.key_update ?request tls with
      | Error _ -> Lwt.return (Error (`Msg "Key update failed"))
      | Ok (tls', buf) ->
        flow.state <- `Active tls' ;
        write_flow flow buf >|= function
        | Ok _ as o -> o
        | Error e   -> Error (e :> wr_or_msg)
  *)

  let close flow =
    (match flow.state with
     | `Active tls | `Read_closed tls ->
       let tls, buf = Tls.Engine.send_close_notify tls in
       flow.state <- inject_state tls flow.state;
       flow.state <- `Closed;
       write_flow flow buf >|= fun _ ->
       ()
     | `Write_closed _ ->
       flow.state <- `Closed;
       Flow.return ()
     | _ -> Flow.return ()) >>= fun () ->
    Flow.close flow.flow

  let shutdown flow mode =
    match flow.state with
    | `Active tls | `Read_closed tls | `Write_closed tls ->
      let tls, buf =
        match flow.state, mode with
        | (`Active tls | `Read_closed tls), (`write | `read_write) ->
          let tls, buf = Tls.Engine.send_close_notify tls in
          tls, Some buf
        | _, _ -> tls, None
      in
      flow.state <- inject_state tls (half_close flow.state mode);
      (* as outlined above, this may fail since the TCP flow may already be (half-)closed *)
      Option.fold
        ~none:(Flow.return ())
        ~some:(fun b -> write_flow flow b >|= fun _ -> ())
        buf >>= fun () ->
      (match flow.state with
       | `Closed -> Flow.close flow.flow
       | _ -> Flow.return ())
    | `Error _ | `Closed ->
      Flow.close flow.flow

  let client_of_flow conf ?host flow =
    let conf' = match host with
      | None      -> conf
      | Some host -> Tls.Config.peer conf host
    in
    let (tls, init) = Tls.Engine.client conf' in
    let tls_flow = {
      role   = `Client ;
      flow   = flow ;
      state  = `Active tls ;
      linger = [] ;
    } in
    write_flow tls_flow init >>= fun _ ->
    drain_handshake tls_flow

  let server_of_flow conf flow =
    let tls_flow = {
      role   = `Server ;
      flow   = flow ;
      state  = `Active (Tls.Engine.server conf) ;
      linger = [] ;
    } in
    drain_handshake tls_flow

  let epoch flow =
    match flow.state with
    | `Closed | `Error _ -> Error ()
    | `Active tls | `Read_closed tls | `Write_closed tls -> Tls.Engine.epoch tls
end
