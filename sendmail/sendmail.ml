open Colombe.Sigs
open Colombe.State
open Colombe

let ( <.> ) f g = fun x -> f (g x)

module Value = struct
  type helo = Domain.t
  type mail_from = Reverse_path.t * (string * string option) list
  type rcpt_to = Forward_path.t * (string * string option) list
  type auth = PLAIN

  type pp_220 = string list
  type pp_221 = string list
  type pp_250 = string list
  type tp_354 = string list
  type code = int * string list

  type error =
    [ Request.Encoder.error
    | Reply.Decoder.error
    | `Unexpected_response of (int * string list) ]

  let pp_error ppf = function
    | #Request.Encoder.error as err -> Request.Encoder.pp_error ppf err
    | #Reply.Decoder.error as err -> Reply.Decoder.pp_error ppf err
    | `Unexpected_response (code, txts) ->
      Fmt.pf ppf "Unexpected response %3d: %a"
        code Fmt.(Dump.list string) txts

  type encoder = Encoder.encoder
  type decoder = Decoder.decoder

  type 'x send =
    | Helo : helo send
    | Mail_from : mail_from send
    | Rcpt_to : rcpt_to send
    | Data : unit send
    | Dot : unit send
    | Quit : unit send
    | Auth : auth send
    | Payload : string send

  type 'x recv =
    | PP_220 : pp_220 recv
    | PP_221 : pp_221 recv
    | PP_250 : pp_250 recv
    | TP_354 : tp_354 recv
    | Code : code recv

  let encode
    : type a. encoder -> a send -> a -> (unit, [> Encoder.error ]) t
    = fun encoder w v ->
      let fiber : a send -> [> Encoder.error ] Encoder.state = function
        | Payload   -> Request.Encoder.request (`Payload v) encoder
        | Helo      -> Request.Encoder.request (`Hello v) encoder
        | Mail_from -> Request.Encoder.request (`Mail v) encoder
        | Rcpt_to   -> Request.Encoder.request (`Recipient v) encoder
        | Data      -> Request.Encoder.request `Data encoder
        | Dot       -> Request.Encoder.request `Data_end encoder
        | Quit      -> Request.Encoder.request `Quit encoder
        | Auth -> match v with
          | PLAIN   -> Request.Encoder.request (`Verb ("AUTH", [ "PLAIN" ])) encoder in
      let rec go = function
        | Encoder.Done -> Return ()
        | Encoder.Write { continue; buffer; off; len; } ->
          Write { k= go <.> continue; buffer; off; len; }
        | Encoder.Error err -> Error err in
      (go <.> fiber) w

  let decode
    : type a. decoder -> a recv -> (a, [> Decoder.error ]) t
    = fun decoder w ->
      let k : Reply.t -> (a, [> Decoder.error ]) t = fun v -> match w, v with
      | PP_220, `PP_220 txts -> Return txts
      | PP_221, `PP_221 txts -> Return txts
      | PP_250, `PP_250 txts -> Return txts
      | TP_354, `TP_354 txts -> Return txts
      | Code, `Other v -> Return v
      | Code, `PN_501 txts -> Return (501, txts)
      | Code, `PN_504 txts -> Return (504, txts)
      | Code, `PP_250 txts -> Return (250, txts)
      | _, v ->
        let code = Reply.code v in
        let txts = Reply.lines v in
        Error (`Unexpected_response (code, txts)) in
    let rec go = function
      | Decoder.Done v -> k v
      | Decoder.Read { buffer; off; len; continue; } ->
        Read { k= go <.> continue; buffer; off; len; }
      | Decoder.Error { error; _ } -> Error error in
    go (Reply.Decoder.response decoder)
end

let src = Logs.Src.create "sendmail" ~doc:"logs sendmail's event"
module Log = (val Logs.src_log src : Logs.LOG)
module Monad = State.Scheduler(State.Context)(Value)

let properly_quit_and_fail ctx err =
  let open Monad in
  let* _txts = send ctx Value.Quit () >>= fun () -> recv ctx Value.PP_221 in
  fail err

let auth ctx mechanism info =
  let open Monad in
  match info with
  | None -> return `Anonymous
  | Some (username, password) ->
    match mechanism with
    | Value.PLAIN ->
      let* code, txts = send ctx Value.Auth mechanism >>= fun () -> recv ctx Value.Code in
      match code with
      | 504 -> properly_quit_and_fail ctx `Unsupported_mechanism
      | 538 -> properly_quit_and_fail ctx `Encryption_required
      | 534 -> properly_quit_and_fail ctx `Weak_mechanism
      | 334 ->
        let* () = match txts with
          | [] ->
            let payload = Base64.encode_exn (Fmt.strf "\000%s\000%s" username password) in
            send ctx Value.Payload payload
          | x :: _ ->
            let x = Base64.decode_exn x in
            let payload = Base64.encode_exn (Fmt.strf "%s\000%s\000%s" x username password) in
            send ctx Value.Payload payload in
        ( recv ctx Value.Code >>= function
            | (235, _txts) -> return `Authenticated
            | (501, _txts) -> properly_quit_and_fail ctx `Authentication_rejected
            | (535, _txts) -> properly_quit_and_fail ctx `Authentication_failed
            | (code, txts) -> fail (`Protocol (`Unexpected_response (code, txts))) )
      | code -> fail (`Protocol (`Unexpected_response (code, txts)))

type domain = Domain.t
type reverse_path = Reverse_path.t
type forward_path = Forward_path.t

type authentication =
  { username : string
  ; password : string
  ; mechanism : Value.auth }

type mechanism = Value.auth = PLAIN
type ('a, 's) stream = unit -> ('a option, 's) io

type error =
  [ `Protocol of Value.error
  | `Unsupported_mechanism
  | `Encryption_required
  | `Weak_mechanism
  | `Authentication_rejected
  | `Authentication_failed
  | `Authentication_required ]

let pp_error ppf = function
  | `Protocol err -> Value.pp_error ppf err
  | `Unsupported_mechanism ->
    Fmt.pf ppf "Unsupported mechanism"
  | `Encryption_required ->
    Fmt.pf ppf "Encryption required"
  | `Weak_mechanism ->
    Fmt.pf ppf "Weak mechanism"
  | `Authentication_rejected -> Fmt.pf ppf "Authentication rejected"
  | `Authentication_failed -> Fmt.pf ppf "Authentication failed"
  | `Authentication_required -> Fmt.pf ppf "Authentication required"

let has_8bit_mime_transport_extension =
  List.exists ((=) "8BITMIME")

let pp_status ppf = function
  | `Anonymous -> Fmt.string ppf "<anonymous>"
  | `Authenticated -> Fmt.string ppf "<authenticated>"

let m0 ctx ?authentication ~domain sender recipients =
  let open Monad in
  recv ctx Value.PP_220 >>= fun _txts ->
  let* txts = send ctx Value.Helo domain >>= fun () -> recv ctx Value.PP_250 in
  let has_8bit_mime_transport_extension = has_8bit_mime_transport_extension txts in
  Log.debug (fun m -> m "8BITMIME extension: %b" has_8bit_mime_transport_extension) ;
  ( match authentication with
    | Some a -> auth ctx a.mechanism (Some (a.username, a.password))
    | None -> return `Anonymous ) >>= fun status ->
  Log.debug (fun m -> m "Auth status: %a" pp_status status) ;
  let parameters =
    if has_8bit_mime_transport_extension
    then [ "BODY", Some "8BITMIME" ]
    else [] in
  let* code, txts = send ctx Value.Mail_from (sender, parameters) >>= fun () -> recv ctx Value.Code in
  let rec go = function
    | [] ->
      send ctx Value.Data () >>= fun () ->
      recv ctx Value.TP_354 >>= fun _txts ->
      return ()
    | x :: r ->
      send ctx Value.Rcpt_to (x, []) >>= fun () ->
      recv ctx Value.PP_250 >>= fun _txts ->
      go r in
  match code with
  | 250 -> go recipients
  | 530 -> properly_quit_and_fail ctx `Authentication_required
  | _ -> fail (`Protocol (`Unexpected_response (code, txts)))

let m1 ctx =
  let open Monad in
  let* _txts = send ctx Value.Dot () >>= fun () -> recv ctx Value.PP_250 in
  let* _txts = send ctx Value.Quit () >>= fun () -> recv ctx Value.PP_221 in
  return ()

let run
  : type s flow. s impl -> (flow, s) rdwr -> flow -> ('a, 'err) t -> (('a, 'err) result, s) io
  = fun { bind; return; } rdwr flow m ->
    let ( >>= ) = bind in

    let rec go = function
      | Read { buffer; off; len; k; } ->
        rdwr.rd flow buffer off len >>= fun len ->
        Log.debug (fun m -> m "[rd]>>> %S" (Bytes.sub_string buffer off len)) ;
        go (k len)
      | Write { buffer; off; len; k; } ->
        Log.debug (fun m -> m "[wr]>>> %S" (String.sub buffer off len)) ;
        rdwr.wr flow buffer off len >>= fun () ->
        go (k len)
      | Return v -> return (Ok v)
      | Error err -> return (Error err : ('a, 'err) result) in
    go m

let sendmail ({ bind; return; } as impl) rdwr flow context ?authentication ~domain sender recipients mail =
  let ( >>- ) = bind in
  let ( >>= ) x f = x >>- function
    | Ok v -> f v
    | Error _ as err -> return err in

  let m0 = m0 context ~domain ?authentication sender recipients in
  run impl rdwr flow m0 >>= fun () ->
  (* assert that context is empty. *)
  let rec go = function
    | Some (buf, off, len) ->
       if len >= 1 && buf.[off] = '.'
       then ( rdwr.wr flow "." 0 1 >>- fun () -> rdwr.wr flow buf off len >>- mail >>- go )
       else rdwr.wr flow buf off len >>- mail >>- go
    | None -> return () in
  Log.debug (fun m -> m "Start to send email") ;
  mail () >>- go >>- fun () ->
  let m1 = m1 context in run impl rdwr flow m1
