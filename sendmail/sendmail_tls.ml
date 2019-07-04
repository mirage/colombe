open Colombe.Sigs
open Colombe.State
open Colombe

module Send_mail_tls_p = struct
  type helo = Domain.t
  type starttls = Rfc1869.t
  type pp_220 = string list
  type pp_250 = string list

  type 'x t =
    | Helo : helo t
    | Starttls : starttls t
    | PP_220 : pp_220 t
    | PP_250 : pp_250 t

  let pp : type x. x t Fmt.t = fun ppf -> function
    | Helo -> Fmt.string ppf "EHLO"
    | Starttls -> Fmt.string ppf "STARTTLS"
    | PP_220 -> Fmt.string ppf "PP-220"
    | PP_250 -> Fmt.string ppf "PP-250"

  let is_request (type a) (w : a t) : bool = match w with
    | Helo -> true | Starttls -> true
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
    : (bytes * int * int) -> (ctx -> int -> ('s, error) process) -> ctx -> ('s, error) process
    = fun (buf, off, len) k ctx ->
      Encoder.write (Bytes.sub_string buf off len) ctx.encoder ;
      k ctx len

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
    { q : [ `q0 | `q1 | `q2 | `q3 | `q4 | `q5 | `q6 ]
    ; tls : Rfc1869.t
    ; domain : Domain.t
    ; tls_buf : Bytes.t }
end

module State = State.Make(Send_mail_tls_s)(Send_mail_tls_p)

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
      let q = match List.assoc_opt Starttls.description.elho exts with
        | Some args ->
          let Rfc1869.V (v, (_, (module Starttls)), ctor) = Rfc1869.prj q.tls in
          ( match Starttls.ehlo v args with
            | Ok v -> { q with tls= ctor v }
            | Error _ -> q (* TODO: [args] is not empty. *) )
        | None -> q (* TODO: STARTTLS is not available on the server side. *) in
      ok $ send Starttls q.tls $ { q with q= `q3 }
    | `q3, Send Starttls ->
      let Rfc1869.V (starttls, (_, (module Starttls)), ctor) = Rfc1869.prj q.tls in
      let action, q' = match Starttls.action starttls with
        | Some (Rfc1869.Recv_code 220) -> recv PP_220, `q4
        | Some Rfc1869.Waiting_payload -> read ~buf:q.tls_buf ~off:0 ~len:(Bytes.length q.tls_buf), `q4
        | Some Rfc1869.(Send _) -> send Starttls (ctor starttls), `q5
        | Some (Rfc1869.Recv_code _) -> assert false
        | None -> Close, `q6 in
      ok $ action $ { q with tls= ctor starttls; q= q' }
    | `q5, Send Starttls ->
      let Rfc1869.V (starttls, (_, (module Starttls)), ctor) = Rfc1869.prj q.tls in
      let starttls = Starttls.handle starttls in
      let tls = ctor starttls in
      let action, q' = match Starttls.action starttls with
        | Some (Rfc1869.Recv_code _) -> assert false
        | Some Rfc1869.Waiting_payload -> read ~buf:q.tls_buf ~off:0 ~len:(Bytes.length q.tls_buf), `q4
        | Some Rfc1869.(Send _) -> send Starttls tls, `q3
        | None -> Close, `q6 in
      ok $ action $ { q with tls; q= q' }
    | `q4, Recv (PP_220, txts) ->
      let Rfc1869.V (starttls, (_, (module Starttls)), ctor) = Rfc1869.prj q.tls in

      (match Starttls.decode (Rfc1869.Response { code= 220; txts; }) starttls with
       | Ok starttls ->
         let tls = ctor starttls in ok $ send Starttls tls $ { q with tls; q= `q5 }
       | Error _ -> assert false)
    | `q4, Read len ->
      let Rfc1869.V (starttls, (_, (module Starttls)), ctor) = Rfc1869.prj q.tls in

      ( match Starttls.decode (Rfc1869.Payload { buf= q.tls_buf; off= 0; len; }) starttls with
        | Ok starttls ->
          (* let starttls = Starttls.handle starttls in *)
          let action, q' = match Starttls.action starttls with
            | Some Rfc1869.Waiting_payload -> read ~buf:q.tls_buf ~off:0 ~len:(Bytes.length q.tls_buf), `q5
            | Some Rfc1869.(Send _) -> send Starttls (ctor starttls), `q5
            | Some (Rfc1869.Recv_code _) -> assert false
            | None -> Close, `q6 in
          ok $ action $ { q with tls= ctor starttls; q= q' }
        | Error _ -> assert false )
    | `q6, Close -> ok $ Close $ { q with q= `q6 }
    | _, _ -> Error (Send_mail_tls_p.Invalid_state, q)

type error = Send_mail_tls_p.error

let pp_error = Send_mail_tls_p.pp_error

type 'x state = 'x Send_mail_tls_s.t
type 'x t = 'x State.t

let domain_to_domain_name = function
  | Colombe.Domain.IPv4 ipv4 -> Domain_name.of_string_exn (Ipaddr.V4.to_string ipv4) (* TODO: fuzz! *)
  | Colombe.Domain.IPv6 ipv6 -> Domain_name.of_string_exn (Ipaddr.V6.to_string ipv6) (* TODO: fuzz! *)
  | Colombe.Domain.Extension (k, v) -> Domain_name.of_string_exn (Fmt.strf "%s:%s" k v)
  | Colombe.Domain.Domain lst -> Domain_name.of_strings_exn lst

let make_state ?logger ?encoding ~domain ~from ~recipients auth mail tls_config =
  let sendmail_state = Sendmail.make_state ?logger ?encoding ~domain ~from ~recipients auth mail in
  let sendmail_state = Sendmail.make sendmail_state in
  let sendmail_ctx   = Colombe.State.make_ctx () in

  let fiber = Sendmail.State.run sendmail_state sendmail_ctx (Sendmail.State.Recv (Sendmail.PP_220, [])) in
  let fiber = Starttls.fiber fiber in

  { Send_mail_tls_s.q= `q0
  ; domain
  ; tls= Starttls.make fiber ~domain:(domain_to_domain_name domain) tls_config
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
        Fmt.epr "rd %!" ;
        rdwr.rd flow buffer off len >>= fun len ->
        Fmt.epr "< @[<hov>%a@]\n%!" (Hxd_string.pp Hxd.O.default) (Bytes.sub_string buffer off len) ;
        go (k len)
      | Write { buffer; off; len; k; } ->
        Fmt.epr "wr %!" ;
        Fmt.epr "> @[<hov>%a@]\n%!" (Hxd_string.pp Hxd.O.default) (Bytes.sub_string buffer off len) ;
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
