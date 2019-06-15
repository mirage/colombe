module Client = struct
  type mechanism =
    | PLAIN
    | CRAM_MD5
    | DIGEST_MD5
    | LOGIN
    (* XXX(dinosaure): see https://www.iana.org/assignments/sasl-mechanisms/sasl-mechanisms.xhtml *)

  let auth : Colombe.Rfc1869.verb = Obj.magic "AUTH"

  type t =
    { mechanism : mechanism
    ; q : [ `q0 | `q1 | `q2 | `q3 ]
    ; username : string
    ; password : string }

  let encode t = match t.q, t.mechanism with
    | `q1, PLAIN -> auth, Some "PLAIN"
    | `q2, PLAIN ->
      let combined = Fmt.strf "\000%s\000%s" t.username t.password in
      Obj.magic (Base64.encode_exn ~pad:true combined, None)
    | _, _ -> assert false

  let expect t = match t.q with
    | `q0 -> 250
    | `q1 -> 334
    | `q2 -> 235
    | `q3 -> assert false

  let decode txts t = match t.q, txts with
    | `q0, (250, _txts) ->
      { t with q= `q1 }
    | `q1, (334, _txts) -> { t with q= `q2 }
    | `q2, (235, _txts) -> { t with q= `q3 }
    | _, _ -> assert false

  let is_authenticated t = t.q = `q3

  let mail_from _t _mail_from = []
  let rcpt_to _t _rcpt_to = []
end

type authenticator = Client.t

let desc : Colombe.Rfc1869.description =
  { name= "Authentication"
  ; elho= Obj.magic "AUTH"
  ; verb= [ Obj.magic "AUTH" ] }

let client = Colombe.Rfc1869.(inj (As_client (desc, (module Client))))
