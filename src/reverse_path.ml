type t = Path.t option

let equal a b = match a, b with
  | Some a, Some b -> Path.equal a b
  | None, None -> true
  | _, _ -> false

let pp = Fmt.option Path.pp

module Decoder = struct
  open Angstrom

  let reverse_path = (Path.Decoder.path >>| fun t -> Some t) <|> (string "<>" *> return None)

  let esmtp_keyword =
    satisfy Domain.Decoder.(is_alpha or is_digit)
    >>= fun pre -> take_while Domain.Decoder.(is_alpha or is_digit or is_dash)
    >>| fun lst -> String.concat "" [ String.make 1 pre ; lst ]

  let esmtp_value = take_while1 (function '\033' .. '\060' | '\062' .. '\126' -> true | _ -> false)

  let esmtp_param =
    esmtp_keyword
    >>= fun key -> (option None (char '=' *> esmtp_value >>| fun x -> Some x))
    >>| fun value -> (key, value)

  let mail_parameters = esmtp_param >>= fun x -> many (char ' ' *> esmtp_param) >>| fun r -> x :: r

  let of_string x =
    let p =
      reverse_path
      >>= fun reverse_path -> option [] (char ' ' *> mail_parameters)
      >>| fun parameters -> (reverse_path, parameters) in
    match parse_string p x with
    | Ok v -> v
    | Error _ -> Fmt.invalid_arg "Invalid reverse-path: %s" x
end

module Encoder = struct
  let to_string = function
    | None -> "<>"
    | Some path -> Path.Encoder.to_string path
end
