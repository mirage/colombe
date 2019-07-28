module Client = struct
  let src = Logs.Src.create "starttls" ~doc:"logs starttls's events"
  let hxd_config = Hxd.O.default
  module Log = (val Logs.src_log src : Logs.LOG)

  type t =
    { q : q
    ; fiber : fiber }
  and 'a state =
    | Initialization : Tls.Engine.state -> handshake state
    | Send_handshake : Tls.Engine.state -> handshake state
    | Wait_handshake : Tls.Engine.state -> wait state
    | Send : Tls.Engine.state -> send state
    | Wait : Tls.Engine.state -> wait state
    | Close : Tls.Engine.state -> close state
    | Send_failure : Tls.Engine.failure * Tls.Engine.state -> send state
  and q = V : 'a * 'a state -> q
  and handshake = Cstruct.t
  and send = Cstruct.t (* Cstruct_cap? *)
  and wait = unit and close = unit
  and fiber = Fiber : ('s, 'error) Colombe.State.process -> fiber

  type Colombe.Rfc1869.error +=
    | Unexpected_arguments
    | Unexpected_application_data
    | Unexpected_payload
    | Unexpected_SMTP_response of { code : int; txts : string list }
    | End_of_stream

  type error = Colombe.Rfc1869.error

  let pp_error ppf = function
    | Unexpected_arguments -> Fmt.string ppf "Unexpected_arguments"
    | Unexpected_application_data -> Fmt.string ppf "Unexpected_application_data"
    | Unexpected_payload -> Fmt.string ppf "Unexpected_payload"
    | Unexpected_SMTP_response { code; txts; }->
      Fmt.pf ppf "(Unexpected_SMTP_response (@[<1>code: %d,@ txts= @[<hov>%a@]@]))"
        code Fmt.(Dump.list string) txts
    | End_of_stream -> Fmt.string ppf "End_of_stream"
    | err -> Colombe.Rfc1869.pp_error ppf err

  let ehlo t args =
    if args <> ""
    then Error Unexpected_arguments
    else Ok t

  let encode t = match t.q with
    | V (_, Initialization _) ->
      Log.app (fun m -> m "Send STARTTLS") ;
      Colombe.Rfc1869.Request { verb= "STARTTLS"; args= [] }
    | V (handshake, Send_handshake _) ->
      Log.app (fun m -> m "Send TLS handshake") ;
      let buf = Cstruct.to_bytes handshake in
      Colombe.Rfc1869.Payload { buf; off= 0; len= Bytes.length buf }
    | V (send, Send _) ->
      Log.app (fun m -> m "Send application data") ;
      let buf = Cstruct.to_bytes send in
      Colombe.Rfc1869.Payload { buf; off= 0; len= Bytes.length buf }
    | V (send, Send_failure (failure, _)) ->
      Log.err (fun m -> m "Send TLS failure (%s)" (Tls.Engine.string_of_failure failure)) ;
      let buf = Cstruct.to_bytes send in
      Colombe.Rfc1869.Payload { buf; off= 0; len= Bytes.length buf }
    | V (_, Wait_handshake _) -> assert false
    | V (_, Wait _) -> assert false
    | V (_, Close _) -> assert false

  let handle t = match t.q with
    | V (_, Wait_handshake state) ->
      assert (Tls.Engine.handshake_in_progress state = false) ;
      assert (Tls.Engine.can_handle_appdata state) ;

      (* XXX(dinosaure): should not be assertions but [if]. *)

      let Fiber fiber = t.fiber in

      ( match fiber with
        | Colombe.State.Read _ | Return _ | Error _ ->
          failwith "Inner process of STARTTLS flow MUST start with a Write operation"
        | Colombe.State.Write { buffer; off; len; k= _; } ->
          Log.app (fun m -> m "Fiber start with: @[<hov>%a@]" (Hxd_string.pp hxd_config) (String.sub buffer off len)) ;

          match Tls.Engine.send_application_data state [ Cstruct.of_string buffer ~off ~len ] with
          | Some (state, send) -> { t with q= V (send, Send state) }
          | None -> t (* XXX(dinosaure): [None] is an error? *) )
    | V (_, Send_handshake state) ->
      (* XXX(dinosaure): hmmhmm, if we look into [`q5] of [Sendmail_tls], we
         possible reach end of handshake even if we just sended TLS-data. In
         this case, [handle] should update internal state as [Wait_handshake]
         does. It's an undefined behavior. *)
      { t with q= V ((), Wait_handshake state) }
    | V (_, Send state) ->
      let Fiber fiber = t.fiber in

      let fiber = match fiber with
        | Colombe.State.Write { len; k; _ } ->
          Log.app (fun m -> m "%d byte(s) consumed on fiber" len) ;
          k len (* XXX(dinosaure): this is on top of this assumption:
                   [ocaml-tls] consumes entirely the fiber. *)
        | _ -> fiber in
      let q = match fiber with
        | Colombe.State.Read _ ->
          Log.app (fun m -> m "Fiber wants to read") ;
          V ((), Wait state)
        | Write { buffer; off; len; k= _ } ->
          Log.app (fun m -> m "Fiber wants to write: @[<hov>%a@]" (Hxd_string.pp hxd_config) (String.sub buffer off len)) ;
          ( match Tls.Engine.send_application_data state [ Cstruct.of_string buffer ~off ~len ] with
            | Some (state, send) -> V (send, Send state)
            | None -> V ((), Wait state) (* TODO! *) )
        | Return _ | Error _ ->
          Log.info (fun m -> m "Got Return or Error state from fiber.\n%!" ) ;
          (* XXX(dinosaure): any [Return] or [Error] wants to notify the server
             to close the connection. *)
          V ((), Close state) in
      { fiber= Fiber fiber; q }
    | _ -> t

  let action t = match t.q with
    | V (_, Initialization _) -> Some (Colombe.Rfc1869.Recv_code 220)
    | V (send, Send_handshake _) ->
      let buf = Cstruct.to_bytes send in
      Some Colombe.Rfc1869.(Send (Payload { buf; off= 0; len= Bytes.length buf; }))
    | V (send, Send _) ->
      let buf = Cstruct.to_bytes send in
      Some Colombe.Rfc1869.(Send (Payload { buf; off= 0; len= Bytes.length buf; }))
    | V (send, Send_failure _) ->
      let buf = Cstruct.to_bytes send in
      Some Colombe.Rfc1869.(Send (Payload { buf; off= 0; len= Bytes.length buf; }))
    | V (_, Wait_handshake _) ->
      Some Colombe.Rfc1869.Waiting_payload
    | V (_, Wait _) ->
      Some Colombe.Rfc1869.Waiting_payload
    | V (_, Close _) -> None

  let handle_handshake t ~buf ~off ~len state =
    match Tls.Engine.handle_tls state (Cstruct.of_bytes buf ~off ~len) with
    | `Ok (_, _, `Data (Some _)) -> Error Unexpected_application_data
    | `Ok (`Ok state, `Response None, _) ->
      if Tls.Engine.can_handle_appdata state && Tls.Engine.handshake_in_progress state = false
      then Ok (handle { t with q= V ((), Wait_handshake state) }) (* here, a dragoon ... *)
      else Ok { t with q= V ((), Wait_handshake state) }
    | `Ok (`Ok state, `Response (Some send), _) ->
      Ok { t with q= V (send, Send_handshake state) }
    | `Ok (`Eof, _, _) -> Error End_of_stream
    | `Ok (`Alert alert, _, _) ->
      Log.err (fun m -> m "Retrieve an alert: %s" (Tls.Packet.alert_type_to_string alert)) ;
      let state, send = Tls.Engine.send_close_notify state in
      Ok { t with q= V (send, Send state) }
    (* XXX(dinosaure): check this branch! *)
    | `Fail (failure, `Response send) ->
      Ok { t with q= V (send, Send_failure (failure, state)) }

  [@@@warning "-27"]

  let handle_tls t ~buf ~off ~len state =
    match Tls.Engine.handle_tls state (Cstruct.of_bytes buf ~off ~len) with
    | `Ok (`Ok state, `Response None, `Data (Some data)) ->
      Log.app (fun m -> m "Receive from the server: @[<hov>%a@]" (Hxd_string.pp hxd_config) (Cstruct.to_string data)) ;

      let Fiber fiber = t.fiber in

      let rec go data = function
        | Colombe.State.Read { buffer; off; len; k; } ->
          let len = min len (Cstruct.len data) in
          Cstruct.blit_to_bytes data 0 buffer off len ;
          go (Cstruct.shift data len) (k len)
        | Write { buffer; off; len; k= _; } as fiber ->
          Log.app (fun m -> m "Fiber wants to write: @[<hov>%a@]" (Hxd_string.pp hxd_config) (String.sub buffer off len)) ;

          ( match Tls.Engine.send_application_data state [ Cstruct.of_string ~off ~len buffer ] with
            | Some (state, send) ->
              Ok { fiber= Fiber fiber; q= V (send, Send state); }
            | None -> assert false )
        | Return _ as fiber ->
          Log.app (fun m -> m "Notify to close the process") ;

          let state, send = Tls.Engine.send_close_notify state in
          Ok { fiber= Fiber fiber; q= V (send, Send state) }
        | Error _ as fiber ->
          (* XXX(dinosaure): [fiber] should take care to [QUIT] properly.
             [STARTTLS] should not introspect [fiber] first, then [QUIT] it
             outside the scope of the already negociated TLS flow. *)
          Log.err (fun m -> m "Fiber returns an error, notify to close the process") ;
          let state, send = Tls.Engine.send_close_notify state in
          Ok { fiber= Fiber fiber; q= V (send, Send state) } in
      go data fiber

    | `Ok (`Ok state, `Response (Some send), `Data None) ->
      Ok { t with q= V (send, Send state) }
    | `Ok (`Ok state, `Response (Some send), `Data (Some data)) ->
      let Fiber fiber = t.fiber in

      let rec go data = function
        | Colombe.State.Read { buffer; off; len; k; } ->
          let len = min len (Cstruct.len data) in
          Cstruct.blit_to_bytes data 0 buffer off len ;
          go (Cstruct.shift data len) (k len)
        | (Write _ | Return _ | Error _) as fiber ->
          Ok { fiber= Fiber fiber; q= V (send, Send state) } in
      go data fiber

    | `Ok (`Ok state, `Response None, `Data None) ->
      Ok { t with q= V ((), Wait state) }
    | `Ok (`Eof, _, _) -> Error End_of_stream
    | `Ok (`Alert alert, _, _) ->
      Log.err (fun m -> m "Retrieve an alert: %s" (Tls.Packet.alert_type_to_string alert)) ;
      let state, send = Tls.Engine.send_close_notify state in
      Ok { t with q= V (send, Send state) }
    (* XXX(dinosaure): check this branch! *)
    | `Fail (failure, `Response send) ->
      Ok { t with q= V (send, Send_failure (failure, state)) }

  let decode resp t = match resp, t.q with
    | Colombe.Rfc1869.Response { code= 220; _ }, V (handshake, Initialization state) ->
      Ok { t with q= V (handshake, Send_handshake state) }
    | Payload { buf; off; len; }, V (_, Send_handshake state) ->
      Log.app (fun m -> m "Receive TLS handshake (client sended handshake)") ;
      handle_handshake t ~buf ~off ~len state
    | Payload { buf; off; len; }, V (_, Wait_handshake state) ->
      Log.app (fun m -> m "Receive TLS handshake (client expected handshake)") ;
      handle_handshake t ~buf ~off ~len state
    | Payload { buf; off; len; }, V (_, Send state) ->
      handle_tls t ~buf ~off ~len state
    | Payload { buf; off; len; }, V (_, Wait state) ->
      handle_tls t ~buf ~off ~len state
    | Response _, V (handshake, Initialization state) -> assert false (* server sended an other SMTP code *)
    | Response { code; txts; }, _ -> Error (Unexpected_SMTP_response { code; txts; })
    | Payload _, V (_, Initialization _) -> Error Unexpected_payload
    | Payload _, V (_, Close _) -> Error Unexpected_payload
    | Payload { buf; off; len; }, V (_, Send_failure (_, state)) ->
      Ok { t with q= V ((), Close state) } (* XXX(dinosaure): need to check! *)

  let mail_from _t _mail_from = []
  let rcpt_to _t _rcpt_to = []
end

type state = Client.t
type fiber = Client.fiber

let description : Colombe.Rfc1869.description =
  { name= "STARTTLS"
  ; elho= "STARTTLS"
  ; verb= [ "STARTTLS" ] }

let extension = Colombe.Rfc1869.inj (module Client)

module Extension = (val extension)
let inj v = Extension.T v

let fiber fiber = Client.Fiber fiber

let make fiber ?domain config =
  let config = match domain with
    | None -> config
    | Some domain -> Tls.Config.peer config (Domain_name.to_string domain) in
  let state, handshake = Tls.Engine.client config in
  { Client.q= V (handshake, Initialization state); fiber }
