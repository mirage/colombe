module Option = struct
  let some x = Some x
  let is_none = function None -> true | Some _ -> false

  let value_exn = function
    | Some x -> x | None -> Fmt.invalid_arg "Option.value_exn"
end

type on = bool

module Client (L : Logs.LOG) = struct
  type error = Colombe.Rfc1869.error
  type t = on

  type code =
    { c : [ `Positive_completion (* INFO *)
          | `Transient_negative_completion (* WARN *)
          | `Permanent_negative_completion (* ERR *) ]
    ; s : int
    ; d : int
    ; info : string }

  let pp_error = Colombe.Rfc1869.pp_error

  let ehlo _ _ = (* assert (args = ""); *) Ok true

  let action _ = assert false
  let encode _ = assert false
  let handle _ = assert false

  let is_sp = (=) ' '
  let is_digit = function
    | '0' .. '9' -> true | _ -> false
  let ( <.> ) f g = fun x -> f (g x)

  let parser =
    let open Angstrom in
    let c =
      (char '2' >>| fun _ -> `Positive_completion)
      <|> (char '4' >>| fun _ -> `Transient_negative_completion)
      <|> (char '5' >>| fun _ -> `Permanent_negative_completion) in
    let d = satisfy is_digit >>| (int_of_string <.> String.make 1) in
    let p =
      d >>= fun x ->
      option None (d >>| Option.some) >>= fun y ->
      option None (d >>| Option.some) >>= fun z ->
      match y, z with
      | Some y, Some z -> return (x * 100 + y * 10 + z)
      | Some y, _ -> return (x * 10 + y)
      | _ -> return x in
    c >>= fun c -> char '.' *> p >>= fun s -> char '.' *> p >>= fun d ->
    skip_while is_sp *> available >>= take >>= fun info ->
    return { c; s; d; info; }

  let level_of_code code =
    if code >= 200 && code < 300 then Ok Logs.App
    else if code >= 400 && code < 500 then Ok Logs.Warning
    else if code >= 500 && code < 600 then Ok Logs.Error
    else Rresult.R.error_msgf "Bad code %3d" code

  let pp_class ppf = function
    | `Positive_completion -> Fmt.string ppf "2"
    | `Transient_negative_completion -> Fmt.string ppf "4"
    | `Permanent_negative_completion -> Fmt.string ppf "5"

  let decode resp q =
    if q then match resp with
      | Colombe.Rfc1869.Payload _ -> Ok q
      | Colombe.Rfc1869.Response { code; txts; } ->
        let parse txt = match Angstrom.parse_string parser txt with
          | Ok v -> Some v
          | Error _ -> Fmt.epr "Got (at least) an error.\n%!" ; None in
        let txts = List.map parse txts in

        match not (List.exists Option.is_none txts), level_of_code code with
        | true, Ok level ->
          let txts = List.map Option.value_exn txts in
          let pp { c; s; d; info; } =
            L.msg level @@ fun m -> m "%a.%3d.%3d: %s" pp_class c s d info in
          List.iter pp txts ; Ok q
        | _ -> Ok q
    else Ok q

  let mail_from _ _ = []
  let rcpt_to _ _ = []
end

let description : Colombe.Rfc1869.description =
  { name= "Enhanced-Status-Codes"
  ; elho= "ENHANCEDSTATUSCODES"
  ; verb= [] }

let extension (module Logs : Logs.LOG) =
  let module Ext = Client(Logs) in
  Colombe.Rfc1869.inj (module Ext)
