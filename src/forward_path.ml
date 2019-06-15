type t =
  | Postmaster
  | Domain of Domain.t
  | Forward_path of Path.t

let pp ppf = function
  | Postmaster -> Fmt.string ppf "<Postmaster>"
  | Domain domain -> Fmt.pf ppf "<Postmaster@%a>" Domain.pp domain
  | Forward_path path -> Fmt.pf ppf "forward-path:%a" Path.pp path

let equal a b = match a, b with
  | Postmaster, Postmaster -> true
  | Domain a, Domain b -> Domain.equal a b
  | Forward_path a, Forward_path b -> Path.equal a b
  | _, _ -> false

module Parser = struct
  open Angstrom

  let forward_path = Path.Parser.path
  let mail_parameters = Reverse_path.Parser.mail_parameters

  let of_string x =
    let p =
      (string "<Postmaster@" *> Domain.Parser.domain <* char '>' >>| fun domain -> Domain domain)
      <|> (string "<Postmaster>" *> return Postmaster)
      <|> (forward_path >>| fun path -> Forward_path path) in
    let p = p
      >>= fun forward_path -> (option [] (char ' ' *> mail_parameters))
      >>| fun parameters -> (forward_path, parameters) in
    match parse_string p x with
    | Ok v -> v
    | Error _ -> Fmt.invalid_arg "Invalid forward-path: %s" x
end

module Encoder = struct
  let to_string = function
    | Postmaster -> "<Postmaster>"
    | Domain domain -> Fmt.strf "<Postmaster@%s>" (Domain.to_string domain)
    | Forward_path path -> Path.Encoder.to_string path
end
