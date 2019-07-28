(* XXX(dinosaure): RFC 4616, mechanism consists of a single message, a string of
   UTF-8 encoded Unicode characters. TODO: [SASLprep][RFC4013]/[hannesm/precis] *)

module Client = struct
  type mechanism =
    | PLAIN

  let pp_mechanism ppf = function
    | PLAIN -> Fmt.string ppf "PLAIN"

  let mechanism_to_string = Fmt.to_to_string pp_mechanism

  type Colombe.Rfc1869.error +=
    | Unsupported_mechanism of mechanism (* 504 *)
    | Authentication_rejected (* 501 *)
    | Weak_mechanism of mechanism (* 534 *)
    | Authentication_failed (* 535 *)
    | Line_too_long (* 500 *)
    | Encryption_required (* 538 *)
    | Invalid_state

  type error = Colombe.Rfc1869.error

  let pp_error ppf = function
    | Unsupported_mechanism m -> Fmt.pf ppf "(Unsupported_mechanism %a)" pp_mechanism m
    | Authentication_rejected -> Fmt.string ppf "Authentication_rejected"
    | Authentication_failed -> Fmt.string ppf "Authentication_failed"
    | Weak_mechanism m -> Fmt.pf ppf "(Weak_mechanism %a)" pp_mechanism m
    | Line_too_long -> Fmt.string ppf "Line_too_long"
    | Encryption_required -> Fmt.string ppf "Encryption_required"
    | Invalid_state -> Fmt.string ppf "Invalid_state"
    | err -> Colombe.Rfc1869.pp_error ppf err

  type t =
    { mechanism : mechanism
    ; q : [ `q0 | `q1_plain of string option | `authenticated ]
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
    | `q1_plain id, PLAIN ->
      let combined = match id with
        | Some id -> Fmt.strf "%s\000%s\000%s" id t.username t.password
        | None -> Fmt.strf "\000%s\000%s" t.username t.password in (* lol *)
      let buf = Base64.encode_exn ~pad:true combined in
      Colombe.Rfc1869.Payload { buf= Bytes.unsafe_of_string (buf ^ "\r\n"); off= 0; len= String.length buf + 2 }
    | `authenticated, PLAIN ->
      Fmt.failwith "Impossible to encode something where we are already authenticated"

  let handle t = t

  let action t = match t.q with
    | `q0 -> Some (Colombe.Rfc1869.Recv_code 334)
    | `q1_plain _ -> Some (Colombe.Rfc1869.Recv_code 235)
    | `authenticated -> None

  let decode resp t = match t.q, resp with
    | `q0, Colombe.Rfc1869.Response { code= 504; _ } ->
      Error (Unsupported_mechanism t.mechanism)
    | `q0, Colombe.Rfc1869.Response { code= 538; _ } ->
      Error Encryption_required
    | `q0, Colombe.Rfc1869.Response { code= 534; _ } ->
      assert (t.mechanism = PLAIN) ;
      Error (Weak_mechanism t.mechanism)
    | `q0, Colombe.Rfc1869.Response { code= 334; txts; }->
      let id = match txts with
        | [] -> None
        | x :: _ -> Some (Base64.decode_exn x) in
      (* XXX(dinosaure): should alert when r <> []? *)
      Ok { t with q= `q1_plain id }
    | `q1_plain _, Colombe.Rfc1869.Response { code= 235; _ } ->
      Ok { t with q= `authenticated }
    | `q1_plain _, Colombe.Rfc1869.Response { code= 500; _ } ->
      Error Line_too_long
    | `q1_plain _, Colombe.Rfc1869.Response { code= 501; _ } ->
      Error Authentication_rejected
    | `q1_plain _, Colombe.Rfc1869.Response { code= 535; _ } ->
      Error Authentication_failed
    | _ -> Error Invalid_state

  let is_authenticated t = t.q = `authenticated

  let mail_from _t _mail_from = [] (* TODO: handle [AUTH=] parameter *)
  let rcpt_to _t _rcpt_to = []
end

type authenticator = Client.t
type mechanism = Client.mechanism

let pp_mechanism ppf Client.PLAIN = Fmt.string ppf "PLAIN"

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

let is_authenticated = Client.is_authenticated

let extension = Colombe.Rfc1869.inj (module Client)
module Extension = (val extension)
let inj v = Extension.T v
