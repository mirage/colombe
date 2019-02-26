type t = Path.t option

let equal a b = match a, b with
  | Some a, Some b -> Path.equal a b
  | None, None -> true
  | _, _ -> false

let pp = Fmt.option Path.pp

module Parser = struct
  open Angstrom

  let at_domain = char '@' *> Domain.Parser.domain
  let a_d_l = at_domain >>= fun x -> many (char ',' *> at_domain) >>| fun r -> x :: r

  let is_atext = function
    | 'a' .. 'z'
    |'A' .. 'Z'
    |'0' .. '9'
    |'!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '/' | '=' | '?'
    |'^' | '_' | '`' | '{' | '}' | '|' | '~' ->
      true
    | _ -> false

  let is_qtextSMTP = function
    | '\032' | '\033' | '\035' .. '\091' | '\093' .. '\126' -> true
    | _ -> false

  let atom = take_while1 is_atext

  let dot_string = atom >>= fun x -> many (char '.' *> atom) >>| fun r -> `Dot_string (x :: r)

  let quoted_pairSMTP =
    char '\\' *> satisfy (function '\032' .. '\126' -> true | _ -> false) >>| String.make 1

  let qcontentSMTP = quoted_pairSMTP <|> take_while1 is_qtextSMTP

  let quoted_string =
    char '"' *> many qcontentSMTP <* char '"' >>| String.concat "" >>| fun x -> `String x

  let local_part = dot_string <|> quoted_string

  let mailbox =
    local_part
    >>= fun local -> char '@' *> (Domain.Parser.domain <|> Domain.Parser.address_literal)
    >>| fun domain -> (local, domain)

  let path =
    char '<' *> option [] (a_d_l <* char ':')
    >>= fun rest -> mailbox
    >>| fun (local, domain) -> { Path.local; domain; rest; }

  let reverse_path = (path >>| fun t -> Some t) <|> (string "<>" *> return None)

  let esmtp_keyword =
    satisfy Domain.Parser.(is_alpha or is_digit)
    >>= fun pre -> take_while Domain.Parser.(is_alpha or is_digit or is_dash)
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

