(* XXX(dinosaure): RFC 4616, mechanism consists of a single message, a string of
   UTF-8 encoded Unicode characters. TODO: [SASLprep][RFC4013]/[hannesm/precis] *)

module Client = struct
  type mechanism =
    | PLAIN
    | CRAM_MD5

  let pp_mechanism ppf = function
    | PLAIN -> Fmt.string ppf "PLAIN"
    | CRAM_MD5 -> Fmt.string ppf "CRAM_MD5"

  let mechanism_to_string = Fmt.to_to_string pp_mechanism

  type error =
    | Unsupported_mechanism of mechanism (* 504 *)
    | Authentication_rejected (* 501 *)
    | Weak_mechanism of mechanism (* 534 *)
    | Authentication_failed (* 535 *)
    | Line_too_long (* 500 *)
    | Encryption_required (* 538 *)
    | Invalid_ehlo of string
    | Invalid_state

  let pp_error ppf = function
    | Unsupported_mechanism m -> Fmt.pf ppf "(Unsupported_mechanism %a)" pp_mechanism m
    | Authentication_rejected -> Fmt.string ppf "Authentication_rejected"
    | Authentication_failed -> Fmt.string ppf "Authentication_failed"
    | Weak_mechanism m -> Fmt.pf ppf "(Weak_mechanism %a)" pp_mechanism m
    | Line_too_long -> Fmt.string ppf "Line_too_long"
    | Encryption_required -> Fmt.string ppf "Encryption_required"
    | Invalid_ehlo args -> Fmt.pf ppf "(Invalid_ehlo %S)" args
    | Invalid_state -> Fmt.string ppf "Invalid_state"

  type t =
    { mechanism : mechanism
    ; q : [ `q0 | `q1_plain | `authenticated ]
    ; username : string
    ; password : string }

  let ehlo t args =
    let ms = Astring.String.cuts ~sep:" " args in
    if List.exists ((=) (mechanism_to_string t.mechanism)) ms
    then Ok t
    else Error (Unsupported_mechanism t.mechanism)

  let encode t = match t.q, t.mechanism with
    | `q0, PLAIN ->
      "AUTH", Some "PLAIN"
    | `q1_plain, PLAIN ->
      let combined = Fmt.strf "\000%s\000%s" t.username t.password in (* lol *)
      Base64.encode_exn ~pad:true combined, None
    | _, _ -> assert false

  type response = TP_334 | PP_250 | PP_235

  let expect t = match t.q with
    | `q0 -> Some TP_334
    | `q1_plain -> Some PP_235
    | `authenticated -> None

  let decode txts t = match t.q, txts with
    | `q0, (504, _txts) ->
      Error (Unsupported_mechanism t.mechanism)
    | `q0, (538, _txts) ->
      Error Encryption_required
    | `q0, (534, _txts) ->
      assert (t.mechanism = PLAIN) ;
      Error (Weak_mechanism t.mechanism)
    | `q0, (334, _txts) ->
      (* XXX(dinosaure): here, [_txts] should be empty. *)
      Ok { t with q= `q1_plain }
    | `q1_plain, (235, _txts) -> Ok { t with q= `authenticated }
    | `q1_plain, (500, _txts) ->
      Error Line_too_long
    | `q1_plain, (501, _txts) ->
      Error Authentication_rejected
    | `q1_plain, (535, _txts) ->
      Error Authentication_failed
    | _ -> Error Invalid_state

  let is_authenticated t = t.q = `authenticated

  let mail_from _t _mail_from = []
  let rcpt_to _t _rcpt_to = []
end

type authenticator = Client.t
let verb = "AUTH"

let description : Colombe.Rfc1869.description =
  { name= "Authentication"
  ; elho= "AUTH"
  ; verb= [ "AUTH" ] }

let extension = Colombe.Rfc1869.inj (description, (module Client))
