type t =
  | IPv4 of Ipaddr.V4.t
  | IPv6 of Ipaddr.V6.t
  | Extension of string * string
  | Domain of string list

let equal a b = match a, b with
  | IPv4 a, IPv4 b -> Ipaddr.(compare (V4 a) (V4 b)) = 0
  | IPv6 a, IPv6 b -> Ipaddr.(compare (V6 a) (V6 b)) = 0
  | IPv4 a, IPv6 b -> Ipaddr.(compare (V4 a) (V6 b)) = 0
  | IPv6 a, IPv4 b -> Ipaddr.(compare (V6 a) (V4 b)) = 0
  | Extension (ka, va), Extension (kb, vb) ->
    String.equal ka kb && String.equal va vb
  | Domain a, Domain b ->
    (try List.for_all2 String.equal a b
     with _ -> false)
  | _, _ -> false

let pp ppf = function
  | IPv4 ipv4 -> Ipaddr.V4.pp ppf ipv4
  | IPv6 ipv6 -> Ipaddr.V6.pp ppf ipv6
  | Extension (k, v) -> Fmt.pf ppf "%s:%s" k v
  | Domain l -> Fmt.pf ppf "%a" Fmt.(list ~sep:(const string ".") string) l

module Parser = struct
  open Angstrom

  let ( or ) a b = fun x -> a x || b x
  let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
  let is_digit = function '0' .. '9' -> true | _ -> false
  let is_dash = (=) '-'

  let let_dig = satisfy (is_alpha or is_digit)

  let ldh_str =
    take_while1 (is_alpha or is_digit or is_dash)
    >>= fun res ->
    if String.get res (String.length res - 1) <> '-'
    then return res
    else fail "Invalid ldh-str token"

  let sub_domain =
    let_dig
    >>= fun pre -> option "" ldh_str
    >>| fun lst -> String.concat "" [ String.make 1 pre; lst ]

  let domain =
    sub_domain
    >>= fun x -> many (char '.' *> sub_domain)
    >>| fun r -> Domain (x :: r)

  (* From Mr. MIME. *)

  let is_dcontent = function
    | '\033' .. '\090' | '\094' .. '\126' -> true
    | _ -> false

  let ipv4_address_literal =
    Unsafe.take_while1 is_dcontent (fun buf ~off ~len ->
        let raw = Bigstringaf.substring buf ~off ~len in
        let pos = ref 0 in
        try
          let res = Ipaddr.V4.of_string_raw raw pos in
          if !pos = len then Some res else None
        with Ipaddr.Parse_error _ -> None )
    >>= function Some v -> return v | None -> fail "ipv4_address_literal"

  let ipv6_addr =
    Unsafe.take_while1 is_dcontent (fun buf ~off ~len ->
        let raw = Bigstringaf.substring buf ~off ~len in
        let pos = ref 0 in
        try
          let res = Ipaddr.V6.of_string_raw raw pos in
          if !pos = len then Some res else None
        with Ipaddr.Parse_error _ -> None )
    >>= function Some v -> return v | None -> fail "ipv6_addr"

  let ipv6_address_literal = string "IPv6:" *> ipv6_addr

  let failf fmt = Fmt.kstrf fail fmt

  let ldh_str =
    take_while1 (function
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' -> true
        | _ -> false )
    >>= fun ldh ->
    if String.unsafe_get ldh (String.length ldh - 1) = '-'
    then failf "ldh_str: %s is invalid" ldh
    else return ldh

  let general_address_literal =
    ldh_str <* char ':'
    >>= fun ldh -> take_while1 is_dcontent
    >>| fun value -> Extension (ldh, value)

  let address_literal =
    char '[' *>
    ((ipv4_address_literal >>| (fun v -> IPv4 v))
     <|> (ipv6_address_literal >>| fun v -> IPv6 v)
     <|> general_address_literal)
    <* char ']'

  let of_string x =
    match parse_string (domain <|> address_literal) x with
    | Ok v -> v
    | Error _ -> Fmt.invalid_arg "Invalid domain: %s" x
end

module Encoder = struct
  (* XXX(dinosaure): use [Fmt] was not the best idea when we will use
     [to_string] on the protocol. *)

  let to_string = function
    | IPv4 ipv4 -> Fmt.strf "[%s]" (Ipaddr.V4.to_string ipv4)
    | IPv6 ipv6 -> Fmt.strf "[IPv6:%s]" (Ipaddr.V6.to_string ipv6)
    | Extension (k, v) -> Fmt.strf "[%s:%s]" k v
    | Domain l -> Fmt.strf "%a" Fmt.(list ~sep:(const string ".") string) l
end

exception Break

let satisfy predicate x =
  let len = String.length x in
  try for i = 0 to len - 1 do if not (predicate x.[i]) then raise Break done ; true
  with Break -> false

let extension k v =
  let is_ldh = Parser.(is_alpha or is_digit or is_dash) in
  let is_dcontent = Parser.is_dcontent in
  if String.length k > 0 && satisfy is_ldh k && k.[String.length k - 1] <> '-'
     && String.length v > 0 && satisfy is_dcontent v
  then Extension (k, v)
  else Fmt.invalid_arg "Invalid key or value"

let ipv4 x = IPv4 x

let ipv6 x = IPv6 x
