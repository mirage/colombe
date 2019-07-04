(* XXX(dinosaure): RFC 4616, mechanism consists of a single message, a string of
   UTF-8 encoded Unicode characters. TODO: [SASLprep][RFC4013]/[hannesm/precis] *)

module Client = struct
  type mechanism =
    | PLAIN

  let pp_mechanism ppf = function
    | PLAIN -> Fmt.string ppf "PLAIN"

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
      Colombe.Rfc1869.Request { verb= "AUTH"; args= [ "PLAIN" ] }
    | `q1_plain, PLAIN ->
      let combined = Fmt.strf "\000%s\000%s" t.username t.password in (* lol *)
      let buf = Base64.encode_exn ~pad:true combined in
      Colombe.Rfc1869.Payload { buf= Bytes.unsafe_of_string (buf ^"\r\n"); off= 0; len= String.length buf + 2 }
    | _, _ -> assert false

  let handle t = t

  let action t = match t.q with
    | `q0 -> Some (Colombe.Rfc1869.Recv_code 334)
    | `q1_plain -> Some (Colombe.Rfc1869.Recv_code 235)
    | `authenticated -> None

  let decode resp t = match t.q, resp with
    | `q0, Colombe.Rfc1869.Response { code= 504; _ } ->
      Error (Unsupported_mechanism t.mechanism)
    | `q0, Colombe.Rfc1869.Response { code= 538; _ } ->
      Error Encryption_required
    | `q0, Colombe.Rfc1869.Response { code= 534; _ } ->
      assert (t.mechanism = PLAIN) ;
      Error (Weak_mechanism t.mechanism)
    | `q0, Colombe.Rfc1869.Response { code= 334; _ }->
      (* XXX(dinosaure): here, [_txts] should be empty. *)
      Ok { t with q= `q1_plain }
    | `q1_plain, Colombe.Rfc1869.Response { code= 235; _ } ->
      Ok { t with q= `authenticated }
    | `q1_plain, Colombe.Rfc1869.Response { code= 500; _ } ->
      Error Line_too_long
    | `q1_plain, Colombe.Rfc1869.Response { code= 501; _ } ->
      Error Authentication_rejected
    | `q1_plain, Colombe.Rfc1869.Response { code= 535; _ } ->
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

let plain = Client.PLAIN

let make ?(mechanism= Client.PLAIN) ~username password =
  { Client.mechanism
  ; q= `q0
  ; username
  ; password }

let extension = Colombe.Rfc1869.inj (description, (module Client))
