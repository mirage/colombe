type t =
  | Postmaster
  | Domain of Domain.t
  | Forward_path of Path.t

let equal a b = match a, b with
  | Postmaster, Postmaster -> true
  | Domain a, Domain b -> Domain.equal a b
  | Forward_path a, Forward_path b -> Path.equal a b
  | _, _ -> false

let pp ppf = function
  | Postmaster -> Fmt.string ppf "<Postmaster>"
  | Domain domain -> Fmt.pf ppf "<Postmaster@%a>" Domain.pp domain
  | Forward_path path -> Path.pp ppf path

module Parser = struct
  open Angstrom

  let forward_path = Reverse_path.Parser.path
  let mail_parameters = Reverse_path.Parser.mail_parameters

  let of_string x =
    let p =
      (string "<Postmaster@" *> Domain.Parser.domain >>| fun domain -> Domain domain)
      <|> (string "Postmaster>" *> return Postmaster)
      <|> (forward_path >>| fun path -> Forward_path path) in
    let p = p
      >>= fun forward_path -> option [] (char ' ' *> mail_parameters)
      >>| fun parameters -> (forward_path, parameters) in
    match parse_string p x with
    | Ok v -> v
    | Error _ -> Fmt.invalid_arg "Invalid forward-path: %s" x
end

