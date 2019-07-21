open Colombe.Sigs
open Colombe.State
open Colombe

module Send_mail_tls_p = struct
  type helo = Domain.t
  type starttls = Rfc1869.t
  type quit = unit
  type pp_220 = string list
  type pp_250 = string list

  type 'x t =
    | Helo : helo t
    | Starttls : starttls t
    | Quit : quit t
    | PP_220 : pp_220 t
    | PP_250 : pp_250 t

  let pp : type x. x t Fmt.t = fun ppf -> function
    | Helo -> Fmt.string ppf "EHLO"
    | Starttls -> Fmt.string ppf "STARTTLS"
    | Quit -> Fmt.string ppf "QUIT"
    | PP_220 -> Fmt.string ppf "PP-220"
    | PP_250 -> Fmt.string ppf "PP-250"

  let is_request (type a) (w : a t) : bool = match w with
    | Helo -> true | Starttls -> true | Quit -> true
    | _ -> false

  type error =
    | Decoder of Decoder.error
    | Encoder of Encoder.error
    | Unexpected_request : 'x t * Request.t -> error
    | Unexpected_reply : 'x t * Reply.t -> error
    | Invalid_state

  let pp_error ppf = function
    | Decoder err -> Decoder.pp_error ppf err
    | Encoder err -> Encoder.pp_error ppf err
    | Unexpected_request (w, v) -> Fmt.pf ppf "(Unexpected_request expect:%a,@ received:@[<hov>%a]@)" pp w Request.pp v
    | Unexpected_reply (w, v) -> Fmt.pf ppf "(Unexpected_reply expect:%a,@ received:@[<hov>%a@])" pp w Reply.pp v
    | Invalid_state -> Fmt.pf ppf "Invalid_state"

  let uncast
    : type o. o t -> o -> (Request.t, Reply.t) either
    = fun w v -> match w, v with
      | Helo, domain -> L (`Hello domain)
      | Starttls, starttls -> L (`Extension starttls)
      | Quit, () -> L `Quit
      | PP_220, txts -> R (`PP_220 txts)
      | PP_250, txts -> R (`PP_250 txts)

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
      | PP_220, `PP_220 txts -> k ctx txts
      | PP_250, `PP_250 txts -> k ctx txts
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

  let rec decode_raw
    : (bytes * int * int) -> (ctx -> int -> ('s, error) process) -> ctx -> ('s, error) process
    = fun (buf, off, len) k ctx ->
      let off', max' = ctx.decoder.Decoder.pos, ctx.decoder.Decoder.max in

      if max' - off' > 0
      then
        ( let res = (min : int -> int -> int) len (max' - off') in
          ctx.decoder.pos <- ctx.decoder.pos + res ;
          Bytes.blit ctx.decoder.Decoder.buffer off' buf off res ;
          k ctx res )
      else
        ( ctx.decoder.pos <- 0
        ; ctx.decoder.max <- 0
        ; Read { buffer= ctx.decoder.Decoder.buffer
               ; off= ctx.decoder.Decoder.pos
               ; len= Bytes.length ctx.decoder.Decoder.buffer - ctx.decoder.Decoder.pos
               ; k= (fun len -> ctx.decoder.max <- len ; decode_raw (buf, off, len) k ctx) } )

  let decode
    : type i. i t -> (ctx -> i -> ('s, error) process) -> ctx -> ('s, error) process
    = fun w k ctx ->
      let rec go = function
        | Decoder.Ok v -> cast k ctx w v
        | Decoder.Read { buffer; off; len; continue; } ->
          let continue n = go (continue n) in
          Read { buffer; off; len; k= continue; }
        | Decoder.Error { error; _ } -> Error (Decoder error) in
      match is_request w with
      | true -> go (Request.Decoder.request ctx.decoder :> [ Request.t | Reply.t ] Decoder.state)
      | false -> go (Reply.Decoder.response ctx.decoder :> [ Request.t | Reply.t ] Decoder.state)
end

module Send_mail_tls_s = struct
  type 's t =
    { q : [ `q0 | `q1 | `q2 | `q3 | `q4 | `q5 | `q6 | `q7 | `q8 ]
    ; tls : Rfc1869.t
    ; domain : Domain.t
    ; tls_buf : Bytes.t }
end

let src = Logs.Src.create "sendmail-tls" ~doc:"logs sendmail-tls's events"
module State = State.Make(Send_mail_tls_s)(Send_mail_tls_p)
module Log = (val Logs.src_log src : Logs.LOG)

let ok x y = Ok (x, y)
let ( $ ) f x = f x

let transition
  : 's Send_mail_tls_s.t -> State.event -> (State.action * 's Send_mail_tls_s.t, Send_mail_tls_p.error * 's Send_mail_tls_s.t) result
  = fun q e -> let open State in
    match q.q, e with
    | `q0, Recv (PP_220, _txts) -> ok $ send Helo q.domain $ { q with q= `q1 }
    | `q1, Send Helo            -> ok $ recv PP_250        $ { q with q= `q2 }
    | `q2, Recv (PP_250, _ehlo :: exts) ->
      let exts = List.map
          (fun ext -> match Astring.String.cut ~sep:" " ext with
             | Some (ext, args) -> (ext, args)
             | None -> (ext, ""))
          exts in
      let action, q = match List.assoc_opt Starttls.description.elho exts with
        | Some args ->
          let Rfc1869.V (starttls, (module Starttls), inj) = Rfc1869.prj q.tls in
          ( match Starttls.ehlo starttls args with
            | Ok starttls -> send Starttls (inj starttls), { q with q= `q3; tls= inj starttls }
            | Error error ->
              Log.err (fun m -> m "Retrieve an error while extension negociation: %a" Starttls.pp_error error) ;
              send Quit (), { q with q= `q8 } )
        | None ->
          Log.err (fun m -> m "STARTTLS is not available") ;
          send Quit (), { q with q= `q7 } in
      ok $ action $ q
    | `q3, Send Starttls ->
      Log.info (fun m -> m "TLS chunk sended .\n%!") ;

      let Rfc1869.V (starttls, (module Starttls), inj) = Rfc1869.prj q.tls in
      let action, q' = match Starttls.action starttls with
        | Some (Rfc1869.Recv_code 220) -> recv PP_220, `q4
        | Some Rfc1869.Waiting_payload -> read ~buf:q.tls_buf ~off:0 ~len:(Bytes.length q.tls_buf), `q4
        | Some Rfc1869.(Send _) -> send Starttls (inj starttls), `q5
        | Some (Rfc1869.Recv_code _) -> assert false
        | None -> Close, `q6 in
      ok $ action $ { q with tls= inj starttls; q= q' }
    | `q5, Send Starttls ->
      let Rfc1869.V (starttls, (module Starttls), inj) = Rfc1869.prj q.tls in
      let starttls = Starttls.handle starttls in
      let tls = inj starttls in
      let action, q' = match Starttls.action starttls with
        | Some (Rfc1869.Recv_code _) -> assert false
        | Some Rfc1869.Waiting_payload -> read ~buf:q.tls_buf ~off:0 ~len:(Bytes.length q.tls_buf), `q4
        | Some Rfc1869.(Send _) ->
          Log.info (fun m -> m "Ask FSM to send TLS chunk.\n%!") ;
          send Starttls tls, `q5 (* XXX(dinosaure): ok dragoon here, if we move
                                    to [q3], we send TLS chunk twice times. Why
                                    I make this new state? Why we not use only
                                    [q3] to send TLS chunk? Why [handle] is only
                                    on [q5]? WHY? *)
        | None -> Close, `q6 in
      ok $ action $ { q with tls; q= q' }
    | `q4, Recv (PP_220, txts) ->
      let Rfc1869.V (starttls, (module Starttls), inj) = Rfc1869.prj q.tls in

      ( match Starttls.decode (Rfc1869.Response { code= 220; txts; }) starttls with
        | Ok starttls ->
          let tls = inj starttls in ok $ send Starttls tls $ { q with tls; q= `q5 }
        | Error error ->
          Log.err (fun m -> m "Retrieve an error while decoding 220 response (@[<hov>%a@]): %a"
                      Fmt.(Dump.list string) txts
                      Starttls.pp_error error) ;
          ok $ send Quit () $ { q with q= `q8 } )
    | `q4, Read len ->
      let Rfc1869.V (starttls, (module Starttls), inj) = Rfc1869.prj q.tls in

      ( match Starttls.decode (Rfc1869.Payload { buf= q.tls_buf; off= 0; len; }) starttls with
        | Ok starttls ->
          let action, q' = match Starttls.action starttls with
            | Some Rfc1869.Waiting_payload ->
              read ~buf:q.tls_buf ~off:0 ~len:(Bytes.length q.tls_buf), `q4
            | Some Rfc1869.(Send _) ->
              Log.info (fun m -> m "Ask FSM to send TLS chunk.\n%!") ;
              send Starttls (inj starttls), `q5
            | Some (Rfc1869.Recv_code _) -> assert false (* XXX(dinosaure): should not occur at this stage! *)
            | None -> Close, `q6 in
          ok $ action $ { q with tls= inj starttls; q= q' }
        | Error error ->
          Log.err (fun m -> m "Retrieve an error while decoding payload: %a"
                      Starttls.pp_error error) ;
          ok $ send Quit () $ { q with q= `q8 } )
    | `q6, Close -> ok $ Close $ { q with q= `q6 }
    | `q7, Send Quit -> ok $ Close $ { q with q= `q6 }
    | `q8, Send Quit -> ok $ Close $ { q with q= `q6 }
    | _, _ -> Error (Send_mail_tls_p.Invalid_state, q)

type error = Send_mail_tls_p.error

let pp_error = Send_mail_tls_p.pp_error

type 'x state = 'x Send_mail_tls_s.t
type 'x t = 'x State.t

let domain_to_domain_name x =
  let x = match x with
    | Colombe.Domain.IPv4 ipv4 -> Domain_name.of_string (Ipaddr.V4.to_string ipv4) (* TODO: fuzz! *)
    | Colombe.Domain.IPv6 ipv6 -> Domain_name.of_string (Ipaddr.V6.to_string ipv6) (* TODO: fuzz! *)
    | Colombe.Domain.Extension (k, v) -> Domain_name.of_string (Fmt.strf "%s:%s" k v)
    | Colombe.Domain.Domain lst -> Domain_name.of_strings lst in
  Rresult.R.bind x Domain_name.host

let make_state ?logger ?encoding ~domain ~from ~recipients auth mail tls_config =
  let sendmail_state = Sendmail.make_state ?logger ?encoding ~domain ~from ~recipients auth mail in
  let sendmail_state = Sendmail.make sendmail_state in
  let sendmail_ctx   = Colombe.State.make_ctx () in

  let fiber = Sendmail.State.run sendmail_state sendmail_ctx (Sendmail.State.Recv (Sendmail.PP_220, [])) in
  let fiber = Starttls.fiber fiber in

  let open Rresult.R in
  domain_to_domain_name domain >>| fun valid_domain ->
  { Send_mail_tls_s.q= `q0
  ; domain
  ; tls= Starttls.inj (Starttls.make fiber ~domain:valid_domain tls_config)
  ; tls_buf= Bytes.create 4096 }

let make state =
  State.make ~init:state transition

let run
  : type s flow. s impl -> (flow, s) rdwr -> flow -> 'x t -> ctx -> (('x state, error) result, s) io
  = fun impl rdwr flow state ctx ->
    let ( >>= ) = impl.bind in
    let return = impl.return in

    let rec go = function
      | Read { buffer; off; len; k; } ->
        rdwr.rd flow buffer off len >>= fun len ->
        go (k len)
      | Write { buffer; off; len; k; } ->
        rdwr.wr flow buffer off len >>= fun () -> go (k len)
      | Return v -> return (Ok v)
      | Error err -> return (Rresult.R.error err) in
    let rec pp_220 = function
      | Read { buffer; off; len; k; } ->
        rdwr.rd flow buffer off len >>= fun len ->
        pp_220 (k len)
      | Write { buffer; off; len; k; } ->
        rdwr.wr flow buffer off len >>= fun () -> pp_220 (k len)
      | Return txts ->
        go (State.run state ctx (State.Recv (PP_220, txts)))
      | Error err -> return (Rresult.R.error err) in
    pp_220 Send_mail_tls_p.(decode PP_220 (fun _ v -> Return v) ctx)
