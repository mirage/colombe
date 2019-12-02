open Colombe.Sigs
open Colombe.State
open Colombe

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
    [ Request.Decoder.error
    | Reply.Decoder.error
    | `Unexpected_response of (int * string list)
    | `Unsupported_mechanism
    | `Encryption_required
    | `Weak_mechanism  ]
  
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
    : type a. Encoder.encoder -> a send -> a -> [> Encoder.error ] Encoder.state
    = fun encoder w v -> match w with
      | Payload -> Encoder.safe (fun encoder -> Encoder.write v encoder ; Encoder.Done) encoder
    | Helo -> Request.Encoder.request (`Hello v) encoder
    | Mail_from -> Request.Encoder.request (`Mail v) encoder
    | Rcpt_to -> Request.Encoder.request (`Recipient v) encoder
    | Data -> Request.Encoder.request `Data encoder
    | Dot -> Request.Encoder.request `Data_end encoder
    | Quit -> Request.Encoder.request `Quit encoder
    | Auth -> match v with
      | PLAIN -> Request.Encoder.request (`Verb ("AUTH", [ "PLAIN" ])) encoder

  let decode
    : type a. Decoder.decoder -> a recv -> (a, [> Decoder.error ]) Decoder.state
    = fun decoder w ->
      let k : Reply.t -> (a, [> Decoder.error ]) Decoder.state = fun v -> match w, v with
      | PP_220, `PP_220 txts -> Decoder.Done txts
      | PP_221, `PP_221 txts -> Decoder.Done txts
      | PP_250, `PP_250 txts -> Decoder.Done txts
      | TP_354, `TP_354 txts -> Decoder.Done txts
      | Code, `Other v -> Decoder.Done v
      | _, _ -> assert false in
    let rec go = function
      | Decoder.Done v -> k v
      | Decoder.Read { buffer; off; len; continue; } ->
        let continue n = go (continue n) in
        Decoder.Read { buffer; off; len; continue; }
      | Decoder.Error _ as err -> err in
    go (Reply.Decoder.response decoder)
end

module Monad = State.Scheduler(Value)

let auth ctx mechanism info =
  let open Monad in
  match info with
  | None -> return `Anonymous
  | Some (username, password) ->
    match mechanism with
    | Value.PLAIN ->
      let* code, txts = send ctx Value.Auth mechanism >>= fun () -> recv ctx Value.Code in
      match code with
      | 504 -> fail `Unsupported_mechanism
      | 538 -> fail `Encryption_required
      | 534 -> fail `Weak_mechanism
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
            | (code, txts) -> fail (`Unexpected_response (code, txts)) )
      | code -> fail (`Unexpected_response (code, txts))

type domain = Domain.t
type reverse_path = Reverse_path.t
type forward_path = Forward_path.t

type authentication =
  { username : string
  ; password : string
  ; mechanism : Value.auth }

type mechanism = Value.auth = PLAIN
type ('a, 's) stream = unit -> ('a option, 's) io

type error = Value.error

let m0 ctx ?authentication ~domain sender recipients =
  let open Monad in
  recv ctx Value.PP_220 >>= fun _txts ->
  let* _txts = send ctx Value.Helo domain >>= fun () -> recv ctx Value.PP_250 in
  ( match authentication with
    | Some a -> auth ctx a.mechanism (Some (a.username, a.password))
    | None -> return `Anonymous ) >>= fun _status ->
  let* _txts = send ctx Value.Mail_from (sender, []) >>= fun () -> recv ctx Value.PP_250 in
  let rec go = function
    | [] ->
      send ctx Value.Data () >>= fun () ->
      recv ctx Value.TP_354 >>= fun _txts ->
      return ()
    | x :: r ->
      send ctx Value.Rcpt_to (x, []) >>= fun () ->
      recv ctx Value.PP_250 >>= fun _txts ->
      go r in
  go recipients

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
        go (k len)
      | Write { buffer; off; len; k; } ->
        rdwr.wr flow buffer off len >>= fun () ->
        go (k len)
      | Return v -> return (Ok v)
      | Error err -> return (Error err : ('a, 'err) result) in
    go m

let sendmail ({ bind; return; } as impl) rdwr flow context ~domain ?authentication sender recipients mail =
  let ( >>- ) = bind in
  let ( >>= ) x f = x >>- function
    | Ok v -> f v
    | Error _ as err -> return err in

  let m0 = m0 context ~domain ?authentication sender recipients in
  run impl rdwr flow m0 >>= fun () ->
  (* assert that context is empty. *)
  let rec go = function
    | Some (buf, off, len) ->
      rdwr.wr flow buf off len >>- mail >>- go
    | None -> return () in
  mail () >>- go >>- fun () ->
  let m1 = m1 context in run impl rdwr flow m1
