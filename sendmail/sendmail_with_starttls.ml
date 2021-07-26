open Colombe.Sigs
open Colombe.State
open Colombe

let ( <.> ) f g x = f (g x)

module Context_with_tls = struct
  type t = { context : Context.t; mutable tls : Tls.Engine.state option }

  type encoder = t

  type decoder = t

  let pp ppf t =
    Fmt.pf ppf "{ @[<hov>context= @[<hov>%a@];@ tls= #state@] }" Context.pp
      t.context

  let encoder x = x

  let decoder x = x

  let make () = { context = Context.make (); tls = None }

  let tls { tls; _ } = match tls with Some _ -> true | _ -> false
end

let src =
  Logs.Src.create "sendmail-with-tls" ~doc:"logs sendmail's event with TLS"

module Log = (val Logs.src_log src : Logs.LOG)

module type VALUE = sig
  type 'x send

  type 'x recv

  type error

  val pp_error : error Fmt.t

  val encode_without_tls :
    Encoder.encoder -> 'x send -> 'x -> (unit, [> `Protocol of error ]) t

  val decode_without_tls :
    Decoder.decoder -> 'x recv -> ('x, [> `Protocol of error ]) t
end

module Value = struct
  type helo = Domain.t

  type mail_from = Reverse_path.t * (string * string option) list

  type rcpt_to = Forward_path.t * (string * string option) list

  type auth = Sendmail.mechanism

  type pp_220 = string list

  type pp_221 = string list

  type pp_250 = string list

  type tp_354 = string list

  type code = int * string list

  type error =
    [ Request.Encoder.error
    | Reply.Decoder.error
    | `Unexpected_response of int * string list ]

  let pp_error ppf = function
    | #Request.Encoder.error as err -> Request.Encoder.pp_error ppf err
    | #Reply.Decoder.error as err -> Reply.Decoder.pp_error ppf err
    | `Unexpected_response (code, txts) ->
        Fmt.pf ppf "Unexpected response %3d: %a" code
          Fmt.(Dump.list string)
          txts

  type 'x send =
    | Helo : helo send
    | Mail_from : mail_from send
    | Rcpt_to : rcpt_to send
    | Data : unit send
    | Dot : unit send
    | Quit : unit send
    | Auth : auth send
    | Payload : string send
    | Starttls : unit send

  type 'x recv =
    | PP_220 : pp_220 recv
    | PP_221 : pp_221 recv
    | PP_250 : pp_250 recv
    | TP_354 : tp_354 recv
    | Code : code recv

  let pp_witness : type a. a recv Fmt.t =
   fun ppf -> function
    | PP_220 -> Fmt.pf ppf "PP-220"
    | PP_221 -> Fmt.pf ppf "PP-221"
    | PP_250 -> Fmt.pf ppf "PP-250"
    | TP_354 -> Fmt.pf ppf "TP-354"
    | Code -> Fmt.pf ppf "<code>"

  let encode :
      type a. Encoder.encoder -> a send -> a -> (unit, [> Encoder.error ]) t =
   fun encoder w v ->
    let fiber : a send -> [> Encoder.error ] Encoder.state = function
      | Payload ->
          let k encoder =
            Encoder.write v encoder ;
            Encoder.write "\r\n" encoder ;
            Encoder.flush (fun _ -> Encoder.Done) encoder in
          Encoder.safe k encoder
      | Helo -> Request.Encoder.request (`Hello v) encoder
      | Mail_from -> Request.Encoder.request (`Mail v) encoder
      | Rcpt_to -> Request.Encoder.request (`Recipient v) encoder
      | Data -> Request.Encoder.request `Data encoder
      | Dot -> Request.Encoder.request `Data_end encoder
      | Quit -> Request.Encoder.request `Quit encoder
      | Starttls -> Request.Encoder.request (`Verb ("STARTTLS", [])) encoder
      | Auth ->
      match v with
      | PLAIN -> Request.Encoder.request (`Verb ("AUTH", [ "PLAIN" ])) encoder
    in
    let rec go = function
      | Encoder.Done -> Return ()
      | Encoder.Write { continue; buffer; off; len } ->
          Write { k = go <.> continue; buffer; off; len }
      | Encoder.Error err -> Error err in
    (go <.> fiber) w

  let decode : type a. Decoder.decoder -> a recv -> (a, [> Decoder.error ]) t =
   fun decoder w ->
    let k : Reply.t -> (a, [> Decoder.error ]) t =
     fun v ->
      match (w, v) with
      | PP_220, `PP_220 txts -> Return txts
      | PP_221, `PP_221 txts -> Return txts
      | PP_250, `PP_250 txts -> Return txts
      | TP_354, `TP_354 txts -> Return txts
      | Code, `Other v -> Return v
      | Code, `PN_501 txts -> Return (501, txts)
      | Code, `PN_504 txts -> Return (504, txts)
      | Code, `PP_250 txts -> Return (250, txts)
      | _, _ ->
          Log.err (fun m ->
              m "Unexpected valid value: witness:%a value:%a" pp_witness w
                Reply.pp v) ;
          let code = Reply.code v in
          let txts = Reply.lines v in
          Error (`Unexpected_response (code, txts)) in
    let rec go = function
      | Decoder.Done v -> k v
      | Decoder.Read { buffer; off; len; continue } ->
          Read { k = go <.> continue; buffer; off; len }
      | Decoder.Error { error; _ } -> Error error in
    go (Reply.Decoder.response decoder)
end

module Value_without_tls = struct
  include Value

  let encode_without_tls ctx w v =
    let rec go = function
      | Error err -> Error (`Protocol err)
      | Read { k; buffer; off; len } -> Read { k = go <.> k; buffer; off; len }
      | Write { k; buffer; off; len } ->
          Write { k = go <.> k; buffer; off; len }
      | Return v -> Return v in
    go (encode ctx w v)

  let decode_without_tls ctx w =
    let rec go = function
      | Error err -> Error (`Protocol err)
      | Read { k; buffer; off; len } -> Read { k = go <.> k; buffer; off; len }
      | Write { k; buffer; off; len } ->
          Write { k = go <.> k; buffer; off; len }
      | Return v -> Return v in
    go (decode ctx w)
end

module type S = sig
  type 'x send

  type 'x recv

  module Value : sig
    type error
  end

  type error =
    [ `Protocol of Value.error
    | `Tls_alert of Tls.Packet.alert_type
    | `Tls_failure of Tls.Engine.failure ]

  val pp_error : error Fmt.t

  type encoder

  type decoder

  val starttls_as_client :
    encoder -> Tls.Config.client -> (unit, [> error ]) State.t

  val starttls_as_server :
    decoder -> Tls.Config.server -> (unit, [> error ]) State.t

  val close : encoder -> (unit, [> error ]) State.t

  val encode : encoder -> 'a send -> 'a -> (unit, [> error ]) State.t

  val decode : decoder -> 'a recv -> ('a, [> error ]) State.t
end

module Make_with_tls (Value : VALUE) :
  S
    with type 'x send = 'x Value.send
     and type 'x recv = 'x Value.recv
     and type encoder = Context_with_tls.encoder
     and type decoder = Context_with_tls.decoder
     and type Value.error = Value.error = struct
  type error =
    [ `Protocol of Value.error
    | `Tls_alert of Tls.Packet.alert_type
    | `Tls_failure of Tls.Engine.failure ]

  type encoder = Context_with_tls.t

  type decoder = Context_with_tls.t

  let pp_error ppf = function
    | `Protocol v -> Value.pp_error ppf v
    | `Tls_alert alert ->
        Fmt.pf ppf "TLS alert: %s" (Tls.Packet.alert_type_to_string alert)
    | `Tls_failure err ->
        Fmt.pf ppf "TLS failure: %s"
          (Tls.Packet.alert_type_to_string (Tls.Engine.alert_of_failure err))

  type 'x send = 'x Value.send

  type 'x recv = 'x Value.recv

  let only_write_tls = function
    | `Response (Some raw) ->
        let rec k raw n =
          let raw = Cstruct.shift raw n in

          (* XXX(dinosaure): write until raw is non-empty. *)
          if Cstruct.len raw = 0
          then Return ()
          else
            Write
              {
                k = k raw;
                buffer = Cstruct.to_string raw;
                off = 0;
                len = Cstruct.len raw;
              } in
        Write
          {
            k = k raw;
            buffer = Cstruct.to_string raw;
            off = 0;
            len = Cstruct.len raw;
          }
    | `Response None -> Return ()

  let rec go_to_failure failure = function
    | Read _ | Return _ | Error _ -> Error (`Tls_failure failure)
    | Write { k; buffer; off; len } ->
        Write { k = go_to_failure failure <.> k; buffer; off; len }

  let rec go_to_alert alert = function
    | Return _ | Error _ -> Error (`Tls_alert alert)
    | Write { k; buffer; off; len } ->
        Write { k = go_to_alert alert <.> k; buffer; off; len }
    | Read { k; buffer; off; len } ->
        Read { k = go_to_alert alert <.> k; buffer; off; len }

  let handle_handshake ctx state k_fiber =
    let buffer_with_tls = Bytes.create 0x1000 in

    let rec fiber_write state = function
      | `Response (Some raw) ->
          let rec k raw n =
            let raw = Cstruct.shift raw n in

            (* XXX(dinosaure): write until raw is non-empty. *)
            if Cstruct.len raw = 0
            then
              if Tls.Engine.handshake_in_progress state
              then
                Read
                  {
                    k = k_handshake state;
                    buffer = buffer_with_tls;
                    off = 0;
                    len = 0x1000;
                  }
              else k_fiber state
            else
              Write
                {
                  k = k raw;
                  buffer = Cstruct.to_string raw;
                  off = 0;
                  len = Cstruct.len raw;
                } in
          Write
            {
              k = k raw;
              buffer = Cstruct.to_string raw;
              off = 0;
              len = Cstruct.len raw;
            }
      | `Response None -> k_fiber state
    and fiber_read state resp = function
      | `Data (Some raw) ->
          Log.debug (fun m -> m "~> %S" (Cstruct.to_string raw)) ;
          (* XXX(dinosaure): should never occur while handshake! *)
          let buffer =
            ctx.Context_with_tls.context.Context.decoder.Decoder.buffer in
          let max = ctx.Context_with_tls.context.Context.decoder.Decoder.max in
          let len = min (Bytes.length buffer - max) (Cstruct.len raw) in
          if len < Cstruct.len raw
          then Fmt.failwith "Read buffer is full and TLS handshake is not done" ;
          (* TODO: this case is when, while handshake, we receive much more data
             that what we can store. We can not consume them because handshake is
             not done. But to be clear, this case should __never__ appear. *)
          Cstruct.blit_to_bytes raw 0 buffer max len ;
          ctx.Context_with_tls.context.Context.decoder.Decoder.max <- max + len ;

          if Tls.Engine.handshake_in_progress state
          then
            Read
              {
                k = k_handshake state;
                buffer = buffer_with_tls;
                off = 0;
                len = 0x1000;
              }
          else k_fiber state
      | `Data None -> fiber_write state resp
    and k_handshake state len =
      let raw = Cstruct.of_bytes buffer_with_tls ~off:0 ~len in

      match Tls.Engine.handle_tls state raw with
      | Ok (`Ok state, resp, data) -> fiber_read state resp data
      | Ok (`Eof, _resp, _data) ->
          Fmt.failwith "Reach End-of-stream while handshake"
      | Error (failure, `Response resp) ->
          (go_to_failure failure <.> only_write_tls) (`Response (Some resp))
      | Ok (`Alert alert, resp, _data) ->
          (go_to_alert alert <.> only_write_tls) resp in

    Read
      { k = k_handshake state; buffer = buffer_with_tls; off = 0; len = 0x1000 }

  let rec go_with_tls ctx fiber delayed_data =
    match (ctx.Context_with_tls.tls, fiber, Cstruct.len delayed_data) with
    | Some state, fiber, _ when not (Tls.Engine.can_handle_appdata state) ->
        let k state =
          ctx.tls <- Some state ;
          go_with_tls ctx fiber delayed_data in
        handle_handshake ctx state k
    | Some state, Write { k; buffer; off; len }, _ -> (
        let raw = Cstruct.of_string ~off ~len buffer in
        Log.debug (fun m -> m "<= %S" (Cstruct.to_string raw)) ;
        match Tls.Engine.send_application_data state [ raw ] with
        | Some (state, raw) ->
            let k n =
              ctx.tls <- Some state ;
              go_with_tls ctx (k n) delayed_data in
            Write
              {
                k;
                buffer = Cstruct.to_string raw;
                off = 0;
                len = Cstruct.len raw;
              }
        | None -> k len)
    | ( Some state,
        Read
          {
            k = k_without_tls;
            buffer = buffer_without_tls;
            off = off_0;
            len = len_0;
          },
        0 ) ->
        let buffer_with_tls = Bytes.make 0x1000 '\000' in

        let rec fiber_read = function
          | Some raw ->
              let len = min (Cstruct.len raw) len_0 in
              Log.debug (fun m -> m "=> %S" (Cstruct.to_string raw)) ;
              Cstruct.blit_to_bytes raw 0 buffer_without_tls off_0 len ;
              go_with_tls ctx (k_without_tls len) (Cstruct.shift raw len)
          | None ->
              (* Even if data is empty, eof was not reached. *)
              Read
                { k; buffer = Bytes.make 0x1000 '\000'; off = 0; len = 0x1000 }
        and fiber_write data = function
          | Some raw ->
              let rec k raw n =
                let raw = Cstruct.shift raw n in

                (* XXX(dinosaure): write until raw is non-empty. *)
                if Cstruct.len raw = 0
                then fiber_read data
                else
                  Write
                    {
                      k = k raw;
                      buffer = Cstruct.to_string raw;
                      off = 0;
                      len = Cstruct.len raw;
                    } in
              Write
                {
                  k = k raw;
                  buffer = Cstruct.to_string raw;
                  off = 0;
                  len = Cstruct.len raw;
                }
          | None -> fiber_read data
        and k n =
          let raw = Cstruct.of_bytes ~off:0 ~len:n buffer_with_tls in

          match Tls.Engine.handle_tls state raw with
          | Ok (`Ok state, `Response None, `Data data) ->
              ctx.tls <- Some state ;
              fiber_read data
          | Ok (`Ok state, `Response resp, `Data data) ->
              ctx.tls <- Some state ;
              fiber_write data resp
          | Ok (`Eof, `Response resp, `Data data) ->
              ctx.tls <- None ;
              let rec go_to_eof = function
                | Read { k; _ } -> k 0 (* emit end-of-stream *)
                | Write { k; buffer; off; len } ->
                    Write { k = go_to_eof <.> k; buffer; off; len }
                | v -> v in
              go_to_eof (fiber_write data resp)
          | Error (failure, `Response resp) ->
              (go_to_failure failure <.> only_write_tls) (`Response (Some resp))
          | Ok (`Alert alert, `Response resp, `Data _) ->
              (go_to_alert alert <.> only_write_tls) (`Response resp) in

        Read
          {
            k;
            buffer = buffer_with_tls;
            off = 0;
            len = Bytes.length buffer_with_tls;
          }
    | ( Some _,
        Read { k; buffer = buffer_without_tls; off = off_0; len = len_0 },
        delayed_len ) ->
        let len = min delayed_len len_0 in
        Cstruct.blit_to_bytes delayed_data 0 buffer_without_tls off_0 len ;
        go_with_tls ctx (k len) (Cstruct.shift delayed_data len)
    | None, (Read _ as fiber), 0 -> fiber
    | None, Read { k; buffer; off; len }, delayed_len ->
        let len = min delayed_len len in
        Cstruct.blit_to_bytes delayed_data 0 buffer off len ;
        go_with_tls ctx (k len) (Cstruct.shift delayed_data len)
    | None, fiber, _ -> fiber
    | _, fiber, _ -> fiber

  let starttls_as_client ctx config =
    let state, raw = Tls.Engine.client config in
    let rec k raw len =
      let raw = Cstruct.shift raw len in
      if Cstruct.len raw = 0
      then
        handle_handshake ctx state (fun state ->
            ctx.tls <- Some state ;
            Return ())
      else
        Write
          {
            k = k raw;
            buffer = Cstruct.to_string raw;
            off = 0;
            len = Cstruct.len raw;
          } in

    (* XXX(dinosaure): clean decoder. *)
    Log.debug (fun m -> m "Clean internal buffer to start TLS.") ;
    let buffer = ctx.Context_with_tls.context.Context.decoder.Decoder.buffer in
    Bytes.fill buffer 0 (Bytes.length buffer) '\000' ;
    ctx.Context_with_tls.context.Context.decoder.Decoder.pos <- 0 ;
    ctx.Context_with_tls.context.Context.decoder.Decoder.max <- 0 ;

    Log.debug (fun m -> m "Start TLS.") ;
    Write
      {
        k = k raw;
        buffer = Cstruct.to_string raw;
        off = 0;
        len = Cstruct.len raw;
      }

  let starttls_as_server ctx config =
    let state = Tls.Engine.server config in

    (* XXX(dinosaure): clean decoder. *)
    Log.debug (fun m -> m "Clean internal buffer to start TLS.") ;
    let buffer = ctx.Context_with_tls.context.Context.decoder.Decoder.buffer in
    Bytes.fill buffer 0 (Bytes.length buffer) '\000' ;
    ctx.Context_with_tls.context.Context.decoder.Decoder.pos <- 0 ;
    ctx.Context_with_tls.context.Context.decoder.Decoder.max <- 0 ;

    Log.debug (fun m -> m "Start TLS.") ;
    handle_handshake ctx state (fun state ->
        ctx.tls <- Some state ;
        Return ())

  let close ctx =
    match ctx.Context_with_tls.tls with
    | Some state ->
        let state, raw = Tls.Engine.send_close_notify state in
        ctx.tls <- Some state ;
        let rec loop len =
          let raw = Cstruct.shift raw len in
          if Cstruct.len raw = 0
          then Return ()
          else
            Write
              {
                k = loop;
                buffer = Cstruct.to_string raw;
                off = 0;
                len = Cstruct.len raw;
              } in
        Write
          {
            k = loop;
            buffer = Cstruct.to_string raw;
            off = 0;
            len = Cstruct.len raw;
          }
    | None -> Return ()

  let encode : type a. encoder -> a send -> a -> (unit, [> error ]) t =
   fun ctx w v ->
    let fiber =
      Value.encode_without_tls ctx.Context_with_tls.context.Context.encoder w v
    in
    go_with_tls ctx fiber Cstruct.empty

  let decode : type a. decoder -> a recv -> (a, [> error ]) t =
   fun ctx w ->
    let decoder = ctx.Context_with_tls.context.Context.decoder in
    let fiber = Value.decode_without_tls decoder w in

    (* XXX(dinosaure): [decoder] can already contains something.
       TODO(dinosaure): [?relax] as an argument? *)
    if Decoder.at_least_one_line ~relax:true decoder
    then fiber
    else go_with_tls ctx fiber Cstruct.empty

  module Value = struct
    type error = Value.error
  end
end

module Value_with_tls = Make_with_tls (Value_without_tls)
module Monad = State.Scheduler (Context_with_tls) (Value_with_tls)

let properly_quit_and_fail ctx err =
  let open Monad in
  let* _txts = send ctx Value.Quit () >>= fun () -> recv ctx Value.PP_221 in
  Error err

let auth ctx mechanism info =
  let open Monad in
  match info with
  | None -> return `Anonymous
  | Some (username, password) ->
  match mechanism with
  | Sendmail.PLAIN -> (
      let* code, txts =
        send ctx Value.Auth mechanism >>= fun () -> recv ctx Value.Code
      in
      match code with
      | 504 -> properly_quit_and_fail ctx `Unsupported_mechanism
      | 538 -> properly_quit_and_fail ctx `Encryption_required
      | 534 -> properly_quit_and_fail ctx `Weak_mechanism
      | 334 -> (
          let* () =
            match txts with
            | [] ->
                let payload =
                  Base64.encode_exn (Fmt.strf "\000%s\000%s" username password)
                in
                send ctx Value.Payload payload
            | x :: _ ->
            match Base64.decode x with
            | Ok x ->
                let payload =
                  Base64.encode_exn
                    (Fmt.strf "%s\000%s\000%s" x username password) in
                send ctx Value.Payload payload
            | Error _ ->
                Log.warn (fun m ->
                    m "The server send an invalid base64 value: %S" x) ;
                let payload =
                  Base64.encode_exn (Fmt.strf "\000%s\000%s" username password)
                in
                send ctx Value.Payload payload
          in
          recv ctx Value.Code >>= function
          | 235, _txts -> return `Authenticated
          | 501, _txts -> properly_quit_and_fail ctx `Authentication_rejected
          | 535, _txts -> properly_quit_and_fail ctx `Authentication_failed
          | code, txts ->
              Error (`Tls (`Protocol (`Unexpected_response (code, txts)))))
      | code -> Error (`Tls (`Protocol (`Unexpected_response (code, txts)))))

type domain = Sendmail.domain

type reverse_path = Sendmail.reverse_path

type forward_path = Sendmail.forward_path

type authentication = Sendmail.authentication

type mechanism = Sendmail.mechanism

type ('a, 's) stream = unit -> ('a option, 's) io

type error =
  [ `Tls of
    [ `Protocol of Value.error
    | `Tls_alert of Tls.Packet.alert_type
    | `Tls_failure of Tls.Engine.failure ]
  | `Protocol of
    [ `Protocol of Value.error
    | `Tls_alert of Tls.Packet.alert_type
    | `Tls_failure of Tls.Engine.failure ]
  | `Unsupported_mechanism
  | `Encryption_required
  | `Weak_mechanism
  | `Authentication_rejected
  | `Authentication_failed
  | `Authentication_required
  | `STARTTLS_unavailable ]

let pp_error ppf = function
  | `Protocol err | `Tls err -> Value_with_tls.pp_error ppf err
  | `Unsupported_mechanism -> Fmt.pf ppf "Unsupported mechanism"
  | `Encryption_required -> Fmt.pf ppf "Encryption required"
  | `Weak_mechanism -> Fmt.pf ppf "Weak mechanism"
  | `Authentication_rejected -> Fmt.pf ppf "Authentication rejected"
  | `Authentication_failed -> Fmt.pf ppf "Authentication failed"
  | `Authentication_required -> Fmt.pf ppf "Authentication required"
  | `STARTTLS_unavailable -> Fmt.pf ppf "STARTTLS unavailable"

let has_8bit_mime_transport_extension = List.exists (( = ) "8BITMIME")

let has_starttls = List.exists (( = ) "STARTTLS")

(* XXX(dinosaure): [m0] IS [Sendmail.m0] + [STARTTLS], we should functorize it over
   a common interface. *)

let m0 ctx config ?authentication ~domain sender recipients =
  let open Monad in
  recv ctx Value.PP_220 >>= fun _txts ->
  let* txts = send ctx Value.Helo domain >>= fun () -> recv ctx Value.PP_250 in
  let has_starttls = has_starttls txts in

  if not has_starttls
  then Error `STARTTLS_unavailable
  else
    let* _txts =
      send ctx Value.Starttls () >>= fun () -> recv ctx Value.PP_220
    in
    Value_with_tls.starttls_as_client ctx config
    |> reword_error (fun err -> `Tls err)
    >>= fun () ->
    let* txts =
      send ctx Value.Helo domain >>= fun () -> recv ctx Value.PP_250
    in
    let has_8bit_mime_transport_extension =
      has_8bit_mime_transport_extension txts in
    (match authentication with
    | Some a ->
        auth ctx a.Sendmail.mechanism
          (Some (a.Sendmail.username, a.Sendmail.password))
    | None -> return `Anonymous)
    >>= fun _status ->
    let parameters =
      if has_8bit_mime_transport_extension
      then [ ("BODY", Some "8BITMIME") ]
      else [] in
    let* code, txts =
      send ctx Value.Mail_from (sender, parameters) >>= fun () ->
      recv ctx Value.Code
    in
    let rec go = function
      | [] ->
          send ctx Value.Data () >>= fun () ->
          recv ctx Value.TP_354 >>= fun _txts -> return ()
      | x :: r ->
          send ctx Value.Rcpt_to (x, []) >>= fun () ->
          recv ctx Value.PP_250 >>= fun _txts -> go r in
    match code with
    | 250 -> go recipients
    | 530 -> properly_quit_and_fail ctx `Authentication_required
    | _ -> Error (`Tls (`Protocol (`Unexpected_response (code, txts))))

let m1 ctx =
  let open Monad in
  let* _txts = send ctx Value.Dot () >>= fun () -> recv ctx Value.PP_250 in
  let* _txts = send ctx Value.Quit () >>= fun () -> recv ctx Value.PP_221 in
  return ()

let run :
    type s flow.
    s impl ->
    (flow, s) rdwr ->
    flow ->
    ('a, 'err) t ->
    (('a, 'err) result, s) io =
 fun { bind; return } rdwr flow m ->
  let ( >>= ) = bind in

  let rec go = function
    | Read { buffer; off; len; k } ->
        rdwr.rd flow buffer off len >>= fun len -> go (k len)
    | Write { buffer; off; len; k } ->
        rdwr.wr flow buffer off len >>= fun () -> go (k len)
    | Return v -> return (Ok v)
    | Error err -> return (Error err : ('a, 'err) result) in
  go m

let _dot = Cstruct.of_string "."

let sendmail { bind; return } rdwr flow ctx mail =
  let ( >>= ) = bind in

  match ctx.Context_with_tls.tls with
  | None ->
      let rec go = function
        | Some (buf, off, len) -> rdwr.wr flow buf off len >>= mail >>= go
        | None -> return () in
      mail () >>= go
  | Some state ->
      let rec go state =
        mail () >>= function
        | None -> return state
        | Some (buf, off, len) -> (
            let raw = Cstruct.of_string buf ~off ~len in
            let raw =
              if len >= 1 && buf.[off] = '.' then [ _dot; raw ] else [ raw ]
            in
            match Tls.Engine.send_application_data state raw with
            | Some (state, raw) ->
                let buf = Cstruct.to_string raw in
                rdwr.wr flow buf 0 (Cstruct.len raw) >>= fun () -> go state
            | None -> go state) in
      go state >>= fun state ->
      ctx.tls <- Some state ;
      return ()

let sendmail ({ bind; return } as impl) rdwr flow context config ?authentication
    ~domain sender recipients mail : ((unit, error) result, 's) io =
  let ( >>- ) = bind in
  let ( >>= ) x f =
    x >>- function Ok v -> f v | Error _ as err -> return err in

  let m0 = m0 context config ~domain ?authentication sender recipients in
  run impl rdwr flow m0 >>= fun () ->
  (* assert that context is empty. *)
  sendmail impl rdwr flow context mail >>- fun () ->
  let m1 = m1 context in
  run impl rdwr flow m1
