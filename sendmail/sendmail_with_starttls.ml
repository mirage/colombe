open Rresult
open Colombe.Sigs
open Colombe.State
open Colombe

let ( <.> ) f g x = f (g x)

let src =
  Logs.Src.create "sendmail-with-tls" ~doc:"logs sendmail's event with TLS"

module Log = (val Logs.src_log src : Logs.LOG)

module type VALUE = sig
  type 'x send
  type 'x recv

  type error =
    [ Request.Encoder.error
    | Reply.Decoder.error
    | `Unexpected_response of int * string list
    | `Invalid_base64_value of string
    | `Invalid_login_challenge of string ]

  val pp_error : error Fmt.t

  val encode_without_tls :
    Encoder.encoder -> 'x send -> 'x -> (unit, [> error ]) t

  val decode_without_tls : Decoder.decoder -> 'x recv -> ('x, [> error ]) t
end

module Value = struct
  type helo = Domain.t
  type mail_from = Reverse_path.t * (string * string option) list
  type rcpt_to = Forward_path.t * (string * string option) list
  type auth = Sendmail.mechanism
  type pp_220 = string list
  type pp_221 = string list
  type pp_250 = string list
  type tp_334 = string
  type tp_354 = string list
  type code = int * string list

  type error =
    [ Request.Encoder.error
    | Reply.Decoder.error
    | `Unexpected_response of int * string list
    | `Invalid_base64_value of string
    | `Invalid_login_challenge of string ]

  let pp_error ppf = function
    | #Request.Encoder.error as err -> Request.Encoder.pp_error ppf err
    | #Reply.Decoder.error as err -> Reply.Decoder.pp_error ppf err
    | `Unexpected_response (code, txts) ->
        Fmt.pf ppf "Unexpected response %3d: %a" code
          Fmt.(Dump.list string)
          txts
    | `Invalid_base64_value str -> Fmt.pf ppf "Invalid Base64 value: %S" str
    | `Invalid_login_challenge str ->
        Fmt.pf ppf "Invalid login challenge: %S" str

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
    | TP_334 : tp_334 recv
    | TP_354 : tp_354 recv
    | Code : code recv
    | Response_or : 'a recv -> ('a, code) result recv

  let rec pp_witness : type a. a recv Fmt.t =
   fun ppf -> function
    | PP_220 -> Fmt.pf ppf "PP-220"
    | PP_221 -> Fmt.pf ppf "PP-221"
    | PP_250 -> Fmt.pf ppf "PP-250"
    | TP_334 -> Fmt.pf ppf "TP-334"
    | TP_354 -> Fmt.pf ppf "TP-354"
    | Code -> Fmt.pf ppf "<code>"
    | Response_or v -> pp_witness ppf v

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
      | LOGIN -> Request.Encoder.request (`Verb ("AUTH", [ "LOGIN" ])) encoder
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
      | TP_334, `Other (334, txts) -> (
          match Base64.decode (String.concat "" txts) with
          | Ok payload -> Return payload
          | Error _ -> Error (`Invalid_base64_value (String.concat "" txts)))
      | TP_354, `TP_354 txts -> Return txts
      | Code, `Other v -> Return v
      | Code, `PN_501 txts -> Return (501, txts)
      | Code, `PN_504 txts -> Return (504, txts)
      | Code, `PP_250 txts -> Return (250, txts)
      | Response_or w, v -> (
          match (w, v) with
          | PP_220, `PP_220 txts -> Return (Ok txts)
          | PP_221, `PP_221 txts -> Return (Ok txts)
          | PP_250, `PP_250 txts -> Return (Ok txts)
          | TP_334, `Other (334, txts) -> (
              match Base64.decode (String.concat "" txts) with
              | Ok payload -> Return (Ok payload)
              | Error _ -> Error (`Invalid_base64_value (String.concat "" txts))
              )
          | TP_354, `TP_354 txts -> Return (Ok txts)
          | _, v ->
              let code = Reply.code v in
              let txts = Reply.lines v in
              Return (Error (code, txts)))
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

module Flow = struct
  type t = unit
  type error = [ `Closed ]
  type +'a io = { run : 'r. ('a -> ('r, error) State.t) -> ('r, error) State.t }

  let bind : 'a io -> ('a -> 'b io) -> 'b io =
   fun { run = t } f -> { run = (fun k -> t (fun x -> (f x).run k)) }

  let return x = { run = (fun k -> k x) }
  let map f t = bind t (fun x -> return (f x))
  let pp_error _ppf `Closed = assert false

  let write () ?(off = 0) ?len buffer =
    let len =
      match len with None -> String.length buffer - off | Some len -> len in
    {
      run =
        (fun k1 ->
          let rec k0 off len len' =
            if len - len' = 0
            then k1 (Ok ())
            else
              let off = off + len' and len = len - len' in
              Write { buffer; off; len; k = k0 off len } in
          Write { buffer; off; len; k = k0 off len });
    }

  let read () ?(off = 0) ?len buffer =
    let len =
      match len with None -> Bytes.length buffer - off | Some len -> len in
    {
      run =
        (fun k1 ->
          let k0 = function
            | `End -> k1 (Ok `End)
            | `Len len -> k1 (Ok (`Len len)) in
          Read { buffer; off; len; k = k0 });
    }

  let close () = { run = (fun k -> k ()) }

  let rec join : (('a, 'err) result, error) State.t -> ('a, 'err) State.t =
    function
    | Write { k; buffer; off; len } ->
        Write { k = join <.> k; buffer; off; len }
    | Read { k; buffer; off; len } -> Read { k = join <.> k; buffer; off; len }
    | Return (Ok v) -> Return v
    | Return (Error err) -> Error err
    | Error `Closed -> assert false
end

module StartTLS = Tls_io.Make (Flow)

module Context_with_tls = struct
  type t = {
    context : Context.t;
    queue : (char, Bigarray.int8_unsigned_elt) Ke.Rke.t;
    mutable tls : StartTLS.t option;
  }

  type encoder = t
  type decoder = t

  let pp ppf t =
    Fmt.pf ppf "{ @[<hov>context= @[<hov>%a@];@ tls= #state@] }" Context.pp
      t.context

  let encoder x = x
  let decoder x = x
  let queue_ex_nihilo () = Ke.Rke.create ~capacity:0x1000 Bigarray.char

  let make ?encoder ?decoder ?(queue = queue_ex_nihilo) () =
    let queue = queue () in
    Ke.Rke.clear queue ;
    { context = Context.make ?encoder ?decoder (); queue; tls = None }

  let tls { tls; _ } = match tls with Some _ -> true | _ -> false
end

module Value_without_tls = struct
  include Value

  let encode_without_tls ctx w v =
    let rec go = function
      | Error err -> Error err
      | Read { k; buffer; off; len } -> Read { k = go <.> k; buffer; off; len }
      | Write { k; buffer; off; len } ->
          Write { k = go <.> k; buffer; off; len }
      | Return v -> Return v in
    go (encode ctx w v)

  let decode_without_tls ctx w =
    let rec go = function
      | Error err -> Error err
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
    type error =
      [ Request.Encoder.error
      | Reply.Decoder.error
      | `Unexpected_response of int * string list
      | `Invalid_base64_value of string
      | `Invalid_login_challenge of string ]
  end

  type error =
    [ Value.error
    | `Tls_alert of Tls.Packet.alert_type
    | `Tls_failure of Tls.Engine.failure
    | `Tls_closed ]

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

module Make_with_tls (Value : VALUE) = struct
  type error =
    [ Request.Encoder.error
    | Reply.Decoder.error
    | `Unexpected_response of int * string list
    | `Invalid_base64_value of string
    | `Invalid_login_challenge of string
    | `Tls_alert of Tls.Packet.alert_type
    | `Tls_failure of Tls.Engine.failure
    | `Tls_closed ]

  type encoder = Context_with_tls.t
  type decoder = Context_with_tls.t

  let pp_error ppf = function
    | #Value.error as v -> Value.pp_error ppf v
    | `Tls_alert alert ->
        Fmt.pf ppf "TLS alert: %s" (Tls.Packet.alert_type_to_string alert)
    | `Tls_failure err ->
        Fmt.pf ppf "TLS failure: %s"
          (Tls.Packet.alert_type_to_string
             (snd (Tls.Engine.alert_of_failure err)))
    | `Tls_closed -> Fmt.string ppf "TLS closed by peer"

  type 'x send = 'x Value.send
  type 'x recv = 'x Value.recv

  let rec pipe :
      type r.
      _ ->
      _ ->
      (r, [> error ]) State.t ->
      _ ->
      ((r, [> error ]) result, Flow.error) State.t =
   fun tls queue fiber -> function
    | Ok `Eof -> (
        match fiber with
        | Read { k; _ } -> k `End |> State.to_result
        | Write { k; buffer; off; len } -> (
            let str = String.sub buffer off len in
            let { Flow.run } = StartTLS.write tls str in
            run @@ function
            | Ok () -> k len |> State.to_result
            | Error (`Tls_alert alert) -> Return (Error (`Tls_alert alert))
            | Error (`Tls_failure failure) ->
                Return (Error (`Tls_failure failure))
            | Error (`Flow `Closed) -> assert false
            | Error `Closed -> Return (Error `Tls_closed))
        | Return v -> Return (Ok v)
        | Error err -> Return (Error err))
    | Error (`Tls_alert alert) -> Return (Error (`Tls_alert alert))
    | Error (`Tls_failure failure) -> Return (Error (`Tls_failure failure))
    | Error (`Flow `Closed) -> assert false
    | Error `Closed -> Return (Error `Tls_closed)
    | Ok (`Data cs) -> (
        let blit src src_off dst dst_off len =
          Bigstringaf.blit_from_string src ~src_off dst ~dst_off ~len in
        Ke.Rke.N.push queue ~blit ~length:String.length cs ;

        match fiber with
        | Read { buffer; off; len; k } -> (
            let blit src src_off dst dst_off len =
              Bigstringaf.blit_to_bytes src ~src_off dst ~dst_off ~len in
            let len' = min (Ke.Rke.length queue) len in
            if len' > 0
            then (
              Ke.Rke.N.keep_exn queue ~blit ~length:Bytes.length ~off ~len:len'
                buffer ;
              Ke.Rke.N.shift_exn queue len') ;
            match k (`Len len') with
            | Read _ as fiber ->
                let { Flow.run } = StartTLS.read tls in
                run (pipe tls queue fiber)
            | fiber -> State.to_result fiber)
        | Write { k; buffer; off; len } -> (
            let str = String.sub buffer off len in
            let { Flow.run } = StartTLS.write tls str in
            run @@ function
            | Ok () -> pipe tls queue (k len) (Ok (`Data ""))
            | Error (`Tls_alert alert) -> Return (Error (`Tls_alert alert))
            | Error (`Tls_failure failure) ->
                Return (Error (`Tls_failure failure))
            | Error (`Flow `Closed) -> assert false
            | Error `Closed -> Return (Error `Tls_closed))
        | Return v -> Return (Ok v)
        | Error err -> Return (Error err))

  let encode : type a. encoder -> a send -> a -> (unit, [> error ]) t =
   fun ctx w v ->
    match ctx.tls with
    | None -> Value.encode_without_tls ctx.context.encoder w v
    | Some tls ->
        let fiber = Value.encode_without_tls ctx.context.encoder w v in
        pipe tls ctx.queue fiber (Ok (`Data "")) |> Flow.join

  let decode : type a. decoder -> a recv -> (a, [> error ]) t =
   fun ctx w ->
    match ctx.tls with
    | None -> Value.decode_without_tls ctx.context.decoder w
    | Some tls ->
        let fiber = Value.decode_without_tls ctx.context.decoder w in
        pipe tls ctx.queue fiber (Ok (`Data "")) |> Flow.join

  let starttls_as_client (ctx : Context_with_tls.t) cfg =
    let { Flow.run } = StartTLS.client_of_flow cfg () in
    (run @@ function
     | Ok tls ->
         ctx.tls <- Some tls ;
         Return (Ok ())
     | Error (`Tls_alert alert) -> Return (Error (`Tls_alert alert))
     | Error (`Tls_failure failure) -> Return (Error (`Tls_failure failure))
     | Error (`Flow `Closed) -> assert false
     | Error `Closed -> Return (Error `Tls_closed))
    |> Flow.join

  let starttls_as_server (ctx : Context_with_tls.t) cfg =
    let { Flow.run } = StartTLS.server_of_flow cfg () in
    (run @@ function
     | Ok tls ->
         ctx.tls <- Some tls ;
         Return (Ok ())
     | Error (`Tls_alert alert) -> Return (Error (`Tls_alert alert))
     | Error (`Tls_failure failure) -> Return (Error (`Tls_failure failure))
     | Error (`Flow `Closed) -> assert false
     | Error `Closed -> Return (Error `Tls_closed))
    |> Flow.join

  let close (ctx : Context_with_tls.t) =
    match ctx.tls with
    | None -> Return ()
    | Some tls ->
        let { Flow.run } = StartTLS.close tls in
        ( run @@ fun () ->
          ctx.tls <- None ;
          Return (Ok ()) )
        |> Flow.join

  module Value = struct
    type error = Value.error
  end
end

module Value_with_tls = Make_with_tls (Value_without_tls)
module Monad = State.Scheduler (Context_with_tls) (Value_with_tls)

let properly_quit_and_fail ctx err =
  let open Monad in
  reword_error
    (fun _ -> err)
    (let* _txts = send ctx Value.Quit () >>= fun () -> recv ctx Value.PP_221 in
     fail err)

let username_challenge_or_quit ctx txts =
  let open Monad in
  match Base64.decode (String.concat "" txts) with
  | Ok "User Name" | Ok "Username:" -> return ()
  | Ok challenge ->
      properly_quit_and_fail ctx
        (`Protocol (`Invalid_login_challenge challenge))
  | Error _ ->
      properly_quit_and_fail ctx
        (`Protocol (`Invalid_base64_value (String.concat "" txts)))

let auth ctx mechanism info =
  let open Monad in
  match info with
  | None -> return `Anonymous
  | Some (username, password) ->
  match mechanism with
  | Sendmail.LOGIN -> (
      let* code, txts =
        send ctx Value.Auth mechanism >>= fun () -> recv ctx Value.Code in
      match code with
      | 504 -> properly_quit_and_fail ctx `Unsupported_mechanism
      | 538 -> properly_quit_and_fail ctx `Encryption_required
      | 534 -> properly_quit_and_fail ctx `Weak_mechanism
      | 334 -> (
          username_challenge_or_quit ctx txts >>= fun () ->
          let payload = Base64.encode_string ~pad:true username in
          send ctx Value.Payload payload >>= fun () ->
          recv ctx (Value.Response_or Value.TP_334) >>= function
          | Ok "Password" | Ok "Password:" -> (
              let payload = Base64.encode_string ~pad:true password in
              send ctx Value.Payload payload >>= fun () ->
              recv ctx Value.Code >>= function
              | 235, _txts -> return `Authenticated
              | 501, _txts ->
                  properly_quit_and_fail ctx `Authentication_rejected
              | 535, _txts -> properly_quit_and_fail ctx `Authentication_failed
              | code, txts ->
                  fail (`Protocol (`Unexpected_response (code, txts))))
          | Ok challenge ->
              properly_quit_and_fail ctx
                (`Protocol (`Invalid_login_challenge challenge))
          | Error (_code, _txts) ->
              properly_quit_and_fail ctx `Authentication_failed)
      | code -> fail (`Protocol (`Unexpected_response (code, txts))))
  | Sendmail.PLAIN -> (
      let* code, txts =
        send ctx Value.Auth mechanism >>= fun () -> recv ctx Value.Code in
      match code with
      | 504 -> properly_quit_and_fail ctx `Unsupported_mechanism
      | 538 -> properly_quit_and_fail ctx `Encryption_required
      | 534 -> properly_quit_and_fail ctx `Weak_mechanism
      | 334 -> (
          let* () =
            match txts with
            | [] ->
                let payload =
                  Base64.encode_exn (Fmt.str "\000%s\000%s" username password)
                in
                send ctx Value.Payload payload
            | x :: _ ->
            match Base64.decode x with
            | Ok x ->
                let payload =
                  Base64.encode_exn
                    (Fmt.str "%s\000%s\000%s" x username password) in
                send ctx Value.Payload payload
            | Error _ ->
                Log.warn (fun m ->
                    m "The server send an invalid base64 value: %S" x) ;
                let payload =
                  Base64.encode_exn (Fmt.str "\000%s\000%s" username password)
                in
                send ctx Value.Payload payload in
          recv ctx Value.Code >>= function
          | 235, _txts -> return `Authenticated
          | 501, _txts -> properly_quit_and_fail ctx `Authentication_rejected
          | 535, _txts -> properly_quit_and_fail ctx `Authentication_failed
          | code, txts -> Error (`Protocol (`Unexpected_response (code, txts))))
      | _ -> Error (`Protocol (`Unexpected_response (code, txts))))

type domain = Sendmail.domain
type reverse_path = Sendmail.reverse_path
type forward_path = Sendmail.forward_path
type authentication = Sendmail.authentication
type mechanism = Sendmail.mechanism
type ('a, 's) stream = unit -> ('a option, 's) io

type error =
  [ `Protocol of
    [ Value.error
    | `Tls_alert of Tls.Packet.alert_type
    | `Tls_failure of Tls.Engine.failure
    | `Tls_closed ]
  | `Unsupported_mechanism
  | `Encryption_required
  | `Weak_mechanism
  | `Authentication_rejected
  | `Authentication_failed
  | `Authentication_required
  | `STARTTLS_unavailable ]

let pp_error ppf = function
  | `Protocol err -> Value_with_tls.pp_error ppf err
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
  then properly_quit_and_fail ctx `STARTTLS_unavailable
  else
    let* _txts =
      send ctx Value.Starttls () >>= fun () -> recv ctx Value.PP_220 in
    Value_with_tls.starttls_as_client ctx config
    |> reword_error (fun err -> `Protocol err)
    >>= fun () ->
    let* txts = send ctx Value.Helo domain >>= fun () -> recv ctx Value.PP_250 in
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
      recv ctx Value.Code in
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
    | _ -> Error (`Protocol (`Unexpected_response (code, txts)))

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

let _dot = "."

let sendmail ({ bind; return } as state) rdwr flow ctx mail =
  let ( >>= ) = bind in

  match ctx.Context_with_tls.tls with
  | None ->
      let rec go = function
        | Some (buf, off, len) -> rdwr.wr flow buf off len >>= mail >>= go
        | None -> return (Ok ()) in
      mail () >>= go
  | Some tls ->
      let rec go () =
        mail () >>= function
        | None -> return (Ok ())
        | Some (buf, off, len) -> (
            let raw = String.sub buf off len in
            let raw =
              if len >= 1 && buf.[off] = '.' then [ _dot; raw ] else [ raw ]
            in
            let { Flow.run = run' } = StartTLS.writev tls raw in
            let m =
              (run' @@ function
               | Ok () -> Return (Ok ())
               | Error (`Tls_alert alert) ->
                   Return (Error (`Protocol (`Tls_alert alert)))
               | Error (`Tls_failure failure) ->
                   Return (Error (`Protocol (`Tls_failure failure)))
               | Error (`Flow `Closed) -> assert false
               | Error `Closed -> Return (Error (`Protocol `Tls_closed)))
              |> Flow.join in
            run state rdwr flow m >>= function
            | Ok () -> go ()
            | Error _ as err -> return err) in
      go ()

let sendmail ({ bind; return } as impl) rdwr flow context config ?authentication
    ~domain sender recipients mail : ((unit, error) result, 's) io =
  let ( >>- ) = bind in
  let ( >>= ) x f =
    x >>- function Ok v -> f v | Error _ as err -> return err in

  let m0 = m0 context config ~domain ?authentication sender recipients in
  run impl rdwr flow m0 >>= fun () ->
  (* assert that context is empty. *)
  sendmail impl rdwr flow context mail >>= fun () ->
  let m1 = m1 context in
  run impl rdwr flow m1
