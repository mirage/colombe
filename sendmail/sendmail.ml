[@@@warning "-37"]

open Colombe.Sigs
open Colombe.State
open Colombe

module Option = struct
  let map f = function
    | Some x -> Some (f x)
    | None -> None

  let bind f = function
    | Some x -> f x
    | None -> None
end

type helo = Domain.t
type mail_from = Reverse_path.t * (string * string option) list
type rcpt_to = Forward_path.t * (string * string option) list
type pp_220 = string list
type pp_221 = string list
type pp_235 = string list
type pp_250 = string list
type tp_354 = string list
type tp_334 = string list
type pn_501 = string list
type pn_530 = string list
type pn_550 = string list
type pn_554 = string list
type auth = Rfc1869.t

type 'x protocol =
  | Helo : helo protocol
  | Mail_from : mail_from protocol
  | Rcpt_to : rcpt_to protocol
  | Data : unit protocol
  | Dot : unit protocol
  | Quit : unit protocol
  | Auth : auth protocol
  | PP_220 : pp_220 protocol
  | PP_221 : pp_221 protocol
  | PP_235 : pp_235 protocol
  | PP_250 : pp_250 protocol
  | TP_354 : tp_354 protocol
  | TP_334 : tp_334 protocol
  | PN_501 : pn_501 protocol
  | PN_530 : pn_530 protocol
  | PN_550 : pn_550 protocol
  | PN_554 : pn_554 protocol

module Send_mail_p = struct
  type 'x t = 'x protocol =
    | Helo : helo t
    | Mail_from : mail_from t
    | Rcpt_to : rcpt_to t
    | Data : unit t
    | Dot : unit t
    | Quit : unit t
    | Auth : auth t
    | PP_220 : pp_220 t
    | PP_221 : pp_221 t
    | PP_235 : pp_235 t
    | PP_250 : pp_250 t
    | TP_354 : tp_354 t
    | TP_334 : tp_334 t
    | PN_501 : pn_501 t (* illegal syntax in a sender or recipient *)
    | PN_530 : pn_530 t (* authentication error *)
    | PN_550 : pn_550 t (* no rights *)
    | PN_554 : pn_554 t (* invalid domain *)

  let pp : type x. x t Fmt.t = fun ppf -> function
    | Helo -> Fmt.string ppf "EHLO"
    | Mail_from -> Fmt.string ppf "MAIL FROM"
    | Rcpt_to -> Fmt.string ppf "RCPT TO"
    | Data -> Fmt.string ppf "DATA"
    | Dot -> Fmt.string ppf "DOT"
    | Quit -> Fmt.string ppf "QUIT"
    | PP_220 -> Fmt.string ppf "PP-220"
    | PP_221 -> Fmt.string ppf "PP-221"
    | PP_235 -> Fmt.string ppf "PP-235"
    | PP_250 -> Fmt.string ppf "PP-250"
    | TP_334 -> Fmt.string ppf "TP-334"
    | TP_354 -> Fmt.string ppf "TP-354"
    | PN_501 -> Fmt.string ppf "PN-501"
    | PN_530 -> Fmt.string ppf "PN-530"
    | PN_550 -> Fmt.string ppf "PN-550"
    | PN_554 -> Fmt.string ppf "PN-554"
    | Auth -> Fmt.string ppf "Auth"

  let is_request (type a) (w : a t) : bool = match w with
    | Helo -> true | Mail_from -> true | Rcpt_to -> true | Data -> true | Dot -> true | Quit -> true
    | _ -> false

  type error =
    | Decoder of Decoder.error
    | Encoder of Encoder.error
    | Unexpected_request : 'x t * Request.t -> error
    | Unexpected_reply : 'x t * Reply.t -> error
    | Auth_error | No_auth
    | Invalid_state

  let pp_error ppf = function
    | Decoder err -> Decoder.pp_error ppf err
    | Encoder err -> Encoder.pp_error ppf err
    | Unexpected_request (w, v) -> Fmt.pf ppf "(Unexpected_request expect:%a,@ received:@[<hov>%a@])" pp w Request.pp v
    | Unexpected_reply (w, v) -> Fmt.pf ppf "(Unexpected_reply expect:%a,@ received:@[<hov>%a@])" pp w Reply.pp v
    | Auth_error -> Fmt.string ppf "Auth_error"
    | No_auth -> Fmt.string ppf "No_auth"
    | Invalid_state -> Fmt.pf ppf "Invalid_state"

  let uncast
    : type o. o t -> o -> (Request.t, Reply.t) either
    = fun w v -> match w, v with
      | Helo, domain -> L (`Hello domain)
      | Mail_from, reverse_path -> L (`Mail reverse_path)
      | Rcpt_to, forward_path -> L (`Recipient forward_path)
      | Data, () -> L `Data
      | Dot, () -> L `Data_end
      | Quit, () -> L `Quit
      | Auth, auth -> L (`Extension auth)
      | PP_220, txts -> R (`PP_220 txts)
      | PP_221, txts -> R (`PP_221 txts)
      | PP_235, txts -> R (`Other (235, txts))
      | PP_250, txts -> R (`PP_250 txts)
      | TP_354, txts -> R (`TP_354 txts)
      | TP_334, txts -> R (`Other (334, txts))
      | PN_501, txts -> R (`PN_501 txts)
      | PN_530, txts -> R (`Other (530, txts))
      | PN_550, txts -> R (`PN_550 txts)
      | PN_554, txts -> R (`PN_554 txts)

  let encode
    : type o. o t * o -> (ctx -> ('s, error) process) -> ctx -> ('s, error) process
    = fun (w, v) k ctx ->
    let rec go = function
      | Encoder.Ok -> k ctx
      | Encoder.Write { buffer; off; len; continue; } ->
        let continue n = go (continue n) in
        Write { buffer; off; len; k= continue; }
      | Encoder.Error err -> Error (Encoder err) in
    let res = match uncast w v with
      | L request -> Request.Encoder.request request ctx.encoder
      | R reply -> Reply.Encoder.response reply ctx.encoder in
    go res

  let cast
    : type x. (ctx -> x -> ('s, error) process) -> ctx -> x t -> [ Request.t | Reply.t ] -> ('s, error) process
    = fun k ctx w v -> match w, v with
    | Helo, `Hello domain -> k ctx domain
    | Mail_from, `Mail from -> k ctx from
    | Rcpt_to, `Recipient t -> k ctx t
    | Data, `Data -> k ctx ()
    | Dot, `Data_end -> k ctx ()
    | Quit, `Quit -> k ctx ()
    | PP_220, `PP_220 txts -> k ctx txts
    | PP_221, `PP_221 txts -> k ctx txts
    | PP_235, `Other (235, txts) -> k ctx txts
    | PP_250, `PP_250 txts -> k ctx txts
    | TP_354, `TP_354 txts -> k ctx txts
    | TP_334, `Other (334, txts) -> k ctx txts
    | PN_501, `PN_501 txts -> k ctx txts
    | PN_530, `Other (530, txts) -> k ctx txts
    | PN_550, `PN_550 txts -> k ctx txts
    | PN_554, `PN_554 txts -> k ctx txts
    | _, (#Request.t as v) -> Error (Unexpected_request (w, v))
    | _, (#Reply.t as v) -> Error (Unexpected_reply (w, v))

  let encode_raw
    : (string * int * int) -> (ctx -> int -> ('s, error) process) -> ctx -> ('s, error) process
    = fun (buf, off, len) k ctx ->
      let rec go = function
        | Encoder.Write { buffer; off; len; continue; } ->
          let k n = go (continue n) in
          Write { buffer; off; len; k; }
        | Encoder.Ok ->
          Encoder.blit ~buf ~off ~len ctx.encoder ;
          k ctx len
        | Encoder.Error err -> Error (Encoder err) in
      go (Encoder.flush (fun _ -> Ok) ctx.encoder)

  let decode_raw
    : (bytes * int * int) -> (ctx -> int -> ('s, error) process) -> ctx -> ('s, error) process
    = fun (buf, off, len) k ctx ->
      let buf', off', len' = Decoder.peek_while_eol ctx.decoder in
      let res = (min : int -> int -> int) len len' in
      Bytes.blit buf' off' buf off res ;
      k ctx res

  let decode
    : type i. i t -> (ctx -> i -> ('s, error) process) -> ctx -> ('s, error) process
    = fun w k ctx ->
    let rec go = function
      | Decoder.Ok v -> cast k ctx w v
      | Decoder.Read { buffer; off; len; continue; } ->
        let continue n = go (continue n) in
        Read { buffer; off; len; k= continue; }
      | Decoder.Error { error; _ }  -> Error (Decoder error) in
    match is_request w with
    | true -> go (Request.Decoder.request ctx.decoder :> [ Request.t | Reply.t ] Decoder.state)
    | false -> go (Reply.Decoder.response ctx.decoder :> [ Request.t | Reply.t ] Decoder.state)
end

type 'a stream = unit -> 'a option

module Send_mail_s = struct
  type 's t =
    { q: [ `q0 | `q1 | `q2 | `q3 | `q4 | `q5 | `q6 | `q7 | `q8 | `q9
         | `q10 | `q11 | `q12 | `q13 | `q14 | `q15 | `q16 ]
    ; domain : Domain.t
    ; from : from
    ; recipients : recipient list
    ; mail : (string * int * int) stream
    ; auth : Rfc1869.t option
    ; logger : Rfc1869.t option
    ; encoding : Rfc1869.t }
  and from = Reverse_path.t
  and recipient = Forward_path.t
end

module State = State.Make(Send_mail_s)(Send_mail_p)

let to_response : type a. a Send_mail_p.t -> a -> Rfc1869.decode = fun k txts -> match k with
  | Send_mail_p.TP_334 -> Rfc1869.Response { code= 334; txts; }
  | Send_mail_p.PP_250 -> Rfc1869.Response { code= 250; txts; }
  | Send_mail_p.PP_235 -> Rfc1869.Response { code= 235; txts; }
  | Send_mail_p.PN_501 -> Rfc1869.Response { code= 501; txts; }
  | _ -> Fmt.failwith "By duality, [to_response] should never receive something else than TP-334, PP-250 and PP-235"

let ok x y = Ok (x, y)
let ( $ ) f x = f x

let rec transition (q:'s Send_mail_s.t) (e:State.event) =
  let open State in
  ( match q.logger with
    | None -> ()
    | Some t ->
      let Rfc1869.V (v, (module Codes), _) = Rfc1869.prj t in
      match e with
      | Recv (PP_250, txts) ->
        ignore @@ Codes.decode (Rfc1869.Response { code= 250; txts }) v
      | Recv (PP_220, txts) ->
        ignore @@ Codes.decode (Rfc1869.Response { code= 220; txts }) v
      | Recv (PP_221, txts) ->
        ignore @@ Codes.decode (Rfc1869.Response { code= 221; txts }) v
      | Recv (PP_235, txts) ->
        ignore @@ Codes.decode (Rfc1869.Response { code= 235; txts }) v
      | _ -> () ) ;

    match q.q, e with
    | `q14, Recv (PP_250, _ehlo :: exts) ->
      let exts = List.map
          (fun ext -> match Astring.String.cut ~sep:" " ext with
             | Some (ext, args) -> (ext, args)
             | None -> (ext, ""))
          exts in
      ( let q = match q.logger, List.assoc_opt Enhanced_status_codes.description.elho exts with
            | Some t, Some args ->
              let Rfc1869.V (v, (module Codes), inj) = Rfc1869.prj t in
              ( match Codes.ehlo v args with
                | Ok v -> { q with logger= Some (inj v) }
                | Error _ -> q )
            | Some _, None | None, Some _ | None, None -> { q with logger= None } in

        let q = match List.assoc_opt Mime.description.elho exts with
          | Some _ -> q | None -> { q with encoding= Mime.inj Mime.none } in

        match q.auth, List.assoc_opt Auth.description.elho exts with
        | Some _, None -> Rresult.R.error (Send_mail_p.No_auth, q)
        | None, Some _ | None, None ->
          (* try without authentication *)
          transition { q with q= `q2 } e
        | Some auth, Some args ->
          let Rfc1869.V (auth, (module Auth), inj) = Rfc1869.prj auth in
          match Auth.ehlo auth args with
          | Ok auth ->
            ok $ send Auth (inj auth) $ { q with q= `q15; auth= Some (inj auth); }
          | Error _ -> Rresult.R.error (Send_mail_p.Auth_error, q) )
    | `q15, Send Auth ->
      ( match q.auth with
        | None -> Error (Send_mail_p.Invalid_state, q)
        | Some ext ->
          let Rfc1869.V (auth, (module Auth), inj) = Rfc1869.prj ext in
          match Auth.action auth with
          | Some (Rfc1869.Recv_code 235) -> ok $ recv Send_mail_p.PP_235 $ { q with q= `q16; }
          | Some (Rfc1869.Recv_code 250) -> ok $ recv Send_mail_p.PP_250 $ { q with q= `q16; }
          | Some (Rfc1869.Recv_code 334) -> ok $ recv Send_mail_p.TP_334 $ { q with q= `q16; }
          | Some (Rfc1869.Recv_code _) ->
            transition { q with q= `q2; auth= Some (inj auth) } (Recv (PP_250, []))
          | Some (Rfc1869.Send _) ->
            transition { q with q= `q2; auth= Some (inj auth) } (Recv (PP_250, []))
          | Some _ | None -> transition { q with q= `q2; auth= Some (inj auth) } (Recv (PP_250, [])) (* TODO: or error? *) )
    | `q16, Recv (k, v) ->
      ( match q.auth with
        | None -> Error (Send_mail_p.Invalid_state, q)
        | Some ext ->
          match Rfc1869.eq ext (module Auth.Extension) with
          | None -> Fmt.failwith "Expected AUTH extension"
          | Some auth ->
            match Auth.Client.decode (to_response k v) auth with
            | Ok auth ->
              if Auth.is_authenticated auth
              then transition { q with q= `q2; auth= Some (Auth.inj auth); } (Recv (PP_250, []))
              else ok $ send Auth (Auth.inj auth) $ { q with q= `q15; auth= Some (Auth.inj auth); }
            | Error _ -> Error (Send_mail_p.Auth_error, q) )
    | `q2, Recv (PP_250, _txts) ->
      let Rfc1869.V (v, (module Ext), _inj) = Colombe.Rfc1869.prj q.encoding in
      (* XXX(dinosaure): extraction of 8BITMIME extension. *)

      ( match Option.bind (fun ext -> Rfc1869.eq ext (module Auth.Extension)) q.auth with
        | None ->
          ok $ send Mail_from (q.from, Ext.mail_from v q.from) $ { q with q= `q3 }
        | Some auth when Auth.is_authenticated auth ->
          ok $ send Mail_from (q.from, Ext.mail_from v q.from) $ { q with q= `q3 }
        | Some _ -> Error (Send_mail_p.Invalid_state, q) )
    | `q4, Recv (PP_250, _txts) ->
      ( match q.recipients with
        | []                     -> ok $ send Data ()         $ { q with q= `q5 }
        | x :: r                 -> ok $ send Rcpt_to (x, []) $ { q with q= `q6; recipients= r } )
    | `q7, Recv (TP_354, _txts) ->
      ( match q.mail () with
        | None                   -> ok $ send Dot ()          $ { q with q= `q8 }
        | Some (buf, off, len)   -> ok $ write ~buf ~off ~len $ { q with q= `q9 } )
    | `q9, Write _ ->
      ( match q.mail () with
        | None                   -> ok $ send Dot ()          $ { q with q= `q8 }
        | Some (buf, off, len)   -> ok $ write ~buf ~off ~len $ { q with q= `q9 } )
    | `q0, Recv (PP_220, _txts)  -> ok $ send Helo q.domain   $ { q with q= `q1 }
    | `q1, Send Helo             -> ok $ recv PP_250          $ { q with q= `q14 }
    | `q3, Send Mail_from        -> ok $ recv PP_250          $ { q with q= `q4 }
    | `q8, Send Dot              -> ok $ recv PP_250          $ { q with q= `q10 }
    | `q6, Send Rcpt_to          -> ok $ recv PP_250          $ { q with q= `q4 } (* loop *)
    | `q5, Send Data             -> ok $ recv TP_354          $ { q with q= `q7 }
    | `q10, Recv (PP_250, _txts) -> ok $ send Quit ()         $ { q with q= `q11 }
    | `q11, Send Quit            -> ok $ recv PP_221          $ { q with q= `q12 }
    | `q12, Recv (PP_221, _txts) -> ok $ Close                $ { q with q= `q13 }
    | `q13, Close                -> ok $ Close                $ { q with q= `q13 }
    | _, _                       -> Error (Send_mail_p.Invalid_state, q)

type error = Send_mail_p.error

let pp_error = Send_mail_p.pp_error

type 'x state = 'x Send_mail_s.t
type 'x t = 'x State.t

let make_state ?logger ?(encoding= Mime.bit8) ~domain ~from ~recipients auth mail =
  let logger = match logger with
    | Some src -> let module Ext = (val (Enhanced_status_codes.extension src)) in Some (Ext.T false)
    | None -> None in
  let encoding = Mime.inj encoding in
  let auth = Option.map Auth.inj auth in
  { Send_mail_s.q= `q0; domain; from; recipients; mail; auth; logger; encoding; }

let make state =
  State.make ~init:state transition

let run
  : type s flow. s impl -> (flow, s) rdwr -> flow -> 'x t -> ctx -> (('x state, error) result, s) io
  = fun impl rdwr flow state ctx ->
    let ( >>= ) = impl.bind in
    let return = impl.return in

    (* XXX(dinosaure): [ctx] has side-effects, it's _safe_ to re-use it to quit
       properly. *)
    let properly_quit err =
      let rec go = function
        | Read { buffer; off; len; k; } ->
          rdwr.rd flow buffer off len >>= fun len ->
          go (k len)
        | Write { buffer; off; len; k; } ->
          rdwr.wr flow buffer off len >>= fun () -> go (k len)
        | Return () -> return (Rresult.R.error err)
        | Error err -> return (Rresult.R.error err) in
        let fiber0 = Send_mail_p.decode Send_mail_p.PP_221 (fun _ _ -> Return ()) in
        let fiber1 = Send_mail_p.encode (Send_mail_p.Quit, ()) fiber0 in
        go (fiber1 ctx) in
    let rec go = function
      | Read { buffer; off; len; k; } ->
        rdwr.rd flow buffer off len >>= fun len ->
        go (k len)
      | Write { buffer; off; len; k; } ->
        rdwr.wr flow buffer off len >>= fun () -> go (k len)
      | Return v -> return (Ok v)
      | Error (Send_mail_p.Unexpected_reply (Send_mail_p.PP_235, `PN_501 _)) ->
        properly_quit Send_mail_p.Auth_error
      | Error Send_mail_p.Auth_error ->
        properly_quit Send_mail_p.Auth_error
      | Error err ->
        return (Rresult.R.error err) in
    let rec pp_220 = function
      | Read { buffer; off; len; k; } ->
        rdwr.rd flow buffer off len >>= fun len ->
        pp_220 (k len)
      | Write { buffer; off; len; k; } ->
        rdwr.wr flow buffer off len >>= fun () -> pp_220 (k len)
      | Return txts ->
        go (State.run state ctx (State.Recv (PP_220, txts)))
      | Error err -> return (Rresult.R.error err) in
    pp_220 Send_mail_p.(decode PP_220 (fun _ v -> Return v) ctx)
