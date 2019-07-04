module Client = struct
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
  and q = V : 'a * 'a state -> q
  and handshake = Cstruct.t
  and send = Cstruct.t (* Cstruct_cap? *)
  and wait = unit and close = unit
  and fiber = Fiber : ('s, 'error) Colombe.State.process -> fiber

  type error =
    | Unexpected_arguments
    | Unexpected_application_data

  let pp_error ppf = function
    | Unexpected_arguments -> Fmt.string ppf "Unexpected_arguments"
    | Unexpected_application_data -> Fmt.string ppf "Unexpected_application_data"

  let ehlo t args =
    if args <> ""
    then Error Unexpected_arguments
    else Ok t

  let encode t = match t.q with
    | V (_, Initialization _) ->
      Fmt.epr ">>> STARTTLS\n%!" ;
      Colombe.Rfc1869.Request { verb= "STARTTLS"; args= [] }
    | V (handshake, Send_handshake _) ->
      Fmt.epr ">>> Send handshake.\n%!" ;
      let buf = Cstruct.to_bytes handshake in
      Colombe.Rfc1869.Payload { buf; off= 0; len= Bytes.length buf }
    | V (send, Send _) ->
      Fmt.epr ">>> Send application data\n%!" ;
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
          Fmt.epr "Start fiber with: %S\n%!" (Bytes.sub_string buffer off len) ;

          match Tls.Engine.send_application_data state [ Cstruct.of_bytes buffer ~off ~len ] with
          | Some (state, send) -> { t with q= V (send, Send state) }
          | None -> t (* XXX(dinosaure): [None] is an error? *) )
    | V (_, Send_handshake state) ->
      (* XXX(dinosaure): hmmhmm, if we look into [`q5] of [Sendmail_tls], we
         possible reach end of handshake even if we just sended TLS-data. In this case, [handle] should update
         internal state as [Wait_handshake] does. *)
      { t with q= V ((), Wait_handshake state) }
    | V (_, Send state) ->
      let Fiber fiber = t.fiber in

      let fiber = match fiber with
        | Colombe.State.Write { len; k; _ } -> Fmt.epr "Consumed %d byte(s) of fiber\n%!" len ; k len
        | _ -> fiber in
      let q = match fiber with
        | Colombe.State.Read _ ->
          Fmt.epr "Fiber wants to read\n%!" ;
          V ((), Wait state)
        | Colombe.State.Write { buffer; off; len; k= _ } ->
          Fmt.epr "Fiber wants to write %S\n%!" (Bytes.sub_string buffer off len) ;
          V (Cstruct.of_bytes ~off ~len buffer, Send state)
        | Colombe.State.Return _ ->
          V ((), Close state)
        | Error _ -> assert false in
      { fiber= Fiber fiber; q }
    | _ -> t

  type response = PP_220

  let action t = match t.q with
    | V (_, Initialization _) -> Some (Colombe.Rfc1869.Recv_code 220)
    | V (send, Send_handshake _) ->
      let buf = Cstruct.to_bytes send in
      Some Colombe.Rfc1869.(Send (Payload { buf; off= 0; len= Bytes.length buf; }))
    | V (send, Send _) ->
      let buf = Cstruct.to_bytes send in
      Some Colombe.Rfc1869.(Send (Payload { buf; off= 0; len= Bytes.length buf; }))
    | V (_, Wait_handshake _) ->
      Fmt.epr "starttls> waiting.\n%!" ;
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
      Fmt.epr "tls> Send handshake.\n%!" ;
      Ok { t with q= V (send, Send_handshake state) }
    | `Ok (`Eof, _, _) | `Ok (`Alert _, _, _) | `Fail _ -> assert false (* TODO *)

  [@@@warning "-27"]

  let handle_tls t ~buf ~off ~len state =
    match Tls.Engine.handle_tls state (Cstruct.of_bytes buf ~off ~len) with
    | `Ok (`Ok state, `Response None, `Data (Some data)) ->
      Fmt.epr "Recv %S\n%!" (Cstruct.to_string data) ;

      let Fiber fiber = t.fiber in

      let rec go data = function
        | Colombe.State.Read { buffer; off; len; k; } ->
          let len = min len (Cstruct.len data) in
          Cstruct.blit_to_bytes data 0 buffer off len ;
          go (Cstruct.shift data len) (k len)
        | Write { buffer; off; len; k= _; } as fiber ->
          Fmt.epr "Send fiber %S\n%!" (Bytes.sub_string buffer off len) ;

          ( match Tls.Engine.send_application_data state [ Cstruct.of_bytes ~off ~len buffer ] with
            | Some (state, send) ->
              Ok { fiber= Fiber fiber; q= V (send, Send state); }
            | None -> assert false )
        | Return _ as fiber ->
          Fmt.epr "Notify close\n%!" ;

          let state, send = Tls.Engine.send_close_notify state in
          Ok { fiber= Fiber fiber; q= V (send, Send state) }
        | Error _ -> assert false in
      go data fiber

    | `Ok (`Ok state, `Response (Some send), `Data None) -> assert false
    | `Ok (`Ok state, `Response (Some send), `Data (Some data)) -> assert false
    | `Ok (`Ok state, `Response None, `Data None) -> assert false
    | `Ok (`Eof, _, _) -> Fmt.epr "EOF dealed\n%!" ; assert false
    | `Ok (`Alert _, _, _) -> Fmt.epr "Alert reached\n%!" ; assert false
    | `Fail (failure, `Response send) ->
      Fmt.epr "Failure reached: %s\n%!" (Tls.Engine.string_of_failure failure) ;
      assert false

  let decode resp t = match resp, t.q with
    | Colombe.Rfc1869.Response { code= 220; _ }, V (handshake, Initialization state) ->
      Ok { t with q= V (handshake, Send_handshake state) }
    | Colombe.Rfc1869.Payload { buf; off; len; }, V (_, Send_handshake state) ->
      Fmt.epr "<<< Recv handshake (after send).\n%!" ;
      handle_handshake t ~buf ~off ~len state
    | Colombe.Rfc1869.Payload { buf; off; len; }, V (_, Wait_handshake state) ->
      Fmt.epr "<<< Recv handshake (after waiting).\n%!" ;
      handle_handshake t ~buf ~off ~len state
    | Colombe.Rfc1869.Payload { buf; off; len; }, V (_, Send state) ->
      handle_tls t ~buf ~off ~len state
    | Colombe.Rfc1869.Payload { buf; off; len; }, V (_, Wait state) ->
      handle_tls t ~buf ~off ~len state
    | _, _ -> assert false

  let mail_from _t _mail_from = []
  let rcpt_to _t _rcpt_to = []
end

type state = Client.t

let description : Colombe.Rfc1869.description =
  { name= "STARTTLS"
  ; elho= "STARTTLS"
  ; verb= [ "STARTTLS" ] }

let extension = Colombe.Rfc1869.inj (description, (module Client))

module Ext = (val extension)
let ctor v = Ext.T v

let fiber fiber = Client.Fiber fiber

let make fiber ?domain config =
  let config = match domain with
    | None -> config
    | Some domain -> Tls.Config.peer config (Domain_name.to_string domain) in
  let state, handshake = Tls.Engine.client config in
  let state = { Client.q= V (handshake, Initialization state); fiber } in
  Ext.T state
