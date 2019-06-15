open Colombe.Sigs
open Colombe.State
open Colombe

module Send_mail_p = struct
  type helo = Domain.t
  type mail_from = Reverse_path.t * (string * string option) list
  type rcpt_to = Forward_path.t * (string * string option) list
  type pp_220 = string list
  type pp_221 = string list
  type pp_235 = string list
  type pp_250 = string list
  type tp_354 = string list
  type tp_334 = string list
  type auth = Rfc1869.t

  type 'x t =
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

  let is_request (type a) (w : a t) : bool = match w with
    | Helo -> true | Mail_from -> true | Rcpt_to -> true | Data -> true | Dot -> true | Quit -> true
    | _ -> false

  type error =
    | Decoder of Decoder.error
    | Encoder of Encoder.error
    | Unexpected_request of Request.t
    | Unexpected_reply of Reply.t
    | Invalid_state

  let pp_error ppf = function
    | Decoder err -> Decoder.pp_error ppf err
    | Encoder err -> Encoder.pp_error ppf err
    | Unexpected_request request -> Fmt.pf ppf "(Unexpected_request @[<hov>%a@])" Request.pp request
    | Unexpected_reply reply -> Fmt.pf ppf "(Unexpected_reply @[<hov>%a@])" Reply.pp reply
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

  let encode
    : type o. o t * o -> (ctx -> ('s, error) process) -> ctx -> ('s, error) process
    = fun (w, v) k ctx ->
    let rec go = function
      | Encoder.Ok -> k ctx
      | Encoder.Write { buffer; off; len; continue; } ->
        let continue n = go (continue n) in
        Wr { buffer; off; len; k= continue; }
      | Encoder.Error err -> Er (Encoder err) in
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
    | _, (#Request.t as v) -> Er (Unexpected_request v)
    | _, (#Reply.t as v) -> Er (Unexpected_reply v)

  let encode_raw
    : (bytes * int * int) -> (ctx -> int -> ('s, error) process) -> ctx -> ('s, error) process
    = fun (buf, off, len) k ctx ->
      Encoder.write (Bytes.sub_string buf off len) ctx.encoder ;
      k ctx len

  let decode
    : type i. i t -> (ctx -> i -> ('s, error) process) -> ctx -> ('s, error) process
    = fun w k ctx ->
    let rec go = function
      | Decoder.Ok v -> cast k ctx w v
      | Decoder.Read { buffer; off; len; continue; } ->
        let continue n = go (continue n) in
        Rd { buffer; off; len; k= continue; }
      | Decoder.Error { error; _ }  -> Er (Decoder error) in
    match is_request w with
    | true -> go (Request.Decoder.request ctx.decoder :> [ Request.t | Reply.t ] Decoder.state)
    | false -> go (Reply.Decoder.response ctx.decoder :> [ Request.t | Reply.t ] Decoder.state)
end

type 'a stream = unit -> 'a option

module Send_mail_s = struct
  type 's t =
    { q: [ `q0 | `q1 | `q2 | `q3 | `q4 | `q5 | `q6 | `q7 | `q8 | `q9
         | `q10 | `q11 | `q12 | `q13 | `q14 | `q15 | `q16 | `q17 | `q18 ]
    ; domain : Domain.t
    ; from : from
    ; recipients : recipient list
    ; mail : (bytes * int * int) stream
    ; auth : Auth.authenticator option }
  and from = Reverse_path.t
  and recipient = Forward_path.t
end

module State = State.Make(Send_mail_s)(Send_mail_p)

let recv_from_auth auth = match Auth.Client.expect auth with
  | 334 -> Send_mail_p.TP_334
  | 235 -> Send_mail_p.PP_235
  | 250 -> Send_mail_p.PP_250
  | _ -> assert false

let rec transition
  : 's Send_mail_s.t -> State.event -> (State.action * 's Send_mail_s.t, Send_mail_p.error * 's Send_mail_s.t) result
  = fun q e -> let open State in match q.q, e with
  | `q0, Recv (PP_220, txts) ->
    List.iter (Fmt.pr "220> %s\n%!") txts ;
    Ok (Send (Helo, q.domain), { q with q= `q1 })
  | `q1, Send Helo ->
    Ok (Recv PP_250, { q with q= `q14 })
  | `q14, Recv (PP_250, txts) ->
    ( match q.auth with
      | Some auth ->
        let module Ext = (val Auth.client) in
        let auth = Auth.Client.decode (250, txts) auth in
        let ext = Ext.T auth in
        Ok (Send (Auth, ext), { q with q= `q15; auth= Some auth })
      | None -> transition { q with q= `q2 } e )
  | `q15, Send Auth ->
    ( match q.auth with
      | Some auth ->
        let recv = recv_from_auth auth in
        Ok (Recv recv, { q with q= `q16; })
      | None -> Error (Send_mail_p.Invalid_state, q) )
  | `q16, Recv (TP_334, txts) ->
    ( match q.auth with
      | Some auth ->
        let module Ext = (val Auth.client) in
        let auth = Auth.Client.decode (334, txts) auth in
        let ext = Ext.T auth in
        Ok (Send (Auth, ext), { q with q= `q17; auth= Some auth })
      | None -> Error (Send_mail_p.Invalid_state, q) )
  | `q17, Send Auth ->
    ( match q.auth with
      | Some auth ->
        let recv = recv_from_auth auth in
        Ok (Recv recv, { q with q= `q18; })
      | None -> Error (Send_mail_p.Invalid_state, q) )
  | `q18, Recv (PP_235, txts) ->
    ( match q.auth with
      | Some auth ->
        let module Ext = (val Auth.client) in
        let auth = Auth.Client.decode (235, txts) auth in
        transition { q with q= `q2; auth= Some auth } (Recv (PP_250, []))
      | None -> Error (Send_mail_p.Invalid_state, q) )
  | `q2, Recv (PP_250, txts) ->
    ( match q.auth with
      | None ->
        List.iter (Fmt.pr "225> %s\n%!") txts ;
        Ok (Send (Mail_from, (q.from, [])), { q with q= `q3 })
      | Some auth when Auth.Client.is_authenticated auth ->
        Ok (Send (Mail_from, (q.from, [])), { q with q= `q3 })
      | Some _ -> Error (Send_mail_p.Invalid_state, q) )
  | `q3, Send Mail_from ->
    Ok (Recv PP_250, { q with q= `q4 })
  | `q4, Recv (PP_250, txts) ->
    List.iter (Fmt.pr "250> %s\n%!") txts ;
    ( match q.recipients with
      | [] -> Ok (Send (Data, ()), { q with q= `q5 })
      | x :: r ->
        Ok (Send (Rcpt_to, (x, [])), { q with q= `q6; recipients= r }))
  | `q6, Send Rcpt_to ->
    Ok (Recv PP_250, { q with q= `q4 }) (* loop *)
  | `q5, Send Data ->
    Ok (Recv TP_354, { q with q= `q7 })
  | `q7, Recv (TP_354, txts) ->
    List.iter (Fmt.pr "354> %s\n%!") txts ;
    ( match q.mail () with
      | None -> Ok (Send (Dot, ()), { q with q= `q8 })
      | Some (buf, off, len) ->
        Ok (Wr { buf; off; len; }, { q with q= `q9 }) )
  | `q8, Send Dot ->
    Ok (Recv PP_250, { q with q= `q10 })
  | `q9, Wr w ->
    Fmt.pr "Wrote %d byte(s).\n%!" w ;
    ( match q.mail () with
      | None -> Ok (Send (Dot, ()), { q with q= `q8 })
      | Some (buf, off, len) ->
        Ok (Wr { buf; off; len; }, { q with q= `q9 }) )
  | `q10, Recv (PP_250, txts) ->
    List.iter (Fmt.pr "250> %s\n%!") txts ;
    Ok (Send (Quit, ()), { q with q= `q11 })
  | `q11, Send Quit ->
    Ok (Recv PP_221, { q with q= `q12 })
  | `q12, Recv (PP_221, txts) ->
    List.iter (Fmt.pr "221> %s\n%!") txts ;
    Ok (Close, { q with q= `q13 })
  | `q13, Close -> Ok (Close, { q with q= `q13 })
  | _, _ -> Error (Send_mail_p.Invalid_state, q)

type error = Send_mail_p.error

let pp_error = Send_mail_p.pp_error

type 'x state = 'x Send_mail_s.t
type 'x t = 'x State.t

let make_state ~domain ~from ~recipients auth mail =
  { Send_mail_s.q= `q0; domain; from; recipients; mail; auth; }

let make state =
  State.make ~i:state transition

let run
  : type s flow. s impl -> (flow, s) rdwr -> flow -> 'x t -> ctx -> (('x state, error) result, s) io
  = fun impl rdwr flow state ctx ->
    let ( >>= ) = impl.bind in
    let return = impl.return in

    let rec go = function
      | Rd { buffer; off; len; k; } ->
        rdwr.rd flow buffer off len >>= fun len ->
        Fmt.epr "<<< %S.\n%!" (Bytes.sub_string buffer off len) ;
        go (k len)
      | Wr { buffer; off; len; k; } ->
        Fmt.epr ">>> %S.\n%!" (Bytes.sub_string buffer off len) ;
        rdwr.wr flow buffer off len >>= fun () -> go (k len)
      | Rt v -> return (Ok v)
      | Er err -> return (Error err) in
    let rec pp_220 = function
      | Rd { buffer; off; len; k; } ->
        rdwr.rd flow buffer off len >>= fun len ->
        Fmt.epr "<<< %S.\n%!" (Bytes.sub_string buffer off len) ;
        pp_220 (k len)
      | Wr { buffer; off; len; k; } ->
        Fmt.epr ">>> %S.\n%!" (Bytes.sub_string buffer off len) ;
        rdwr.wr flow buffer off len >>= fun () -> pp_220 (k len)
      | Rt txts ->
        go (State.run state ctx (State.Recv (PP_220, txts)))
      | Er err -> return (Error err) in
    pp_220 Send_mail_p.(decode PP_220 (fun _ v -> Rt v) ctx)
