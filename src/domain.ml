type t =
  | IPv4 of Ipaddr.V4.t
  | IPv6 of Ipaddr.V6.t
  | Extension of string * string
  | Domain of string list

let equal a b =
  match (a, b) with
  | IPv4 a, IPv4 b -> Ipaddr.(compare (V4 a) (V4 b)) = 0
  | IPv6 a, IPv6 b -> Ipaddr.(compare (V6 a) (V6 b)) = 0
  | IPv4 a, IPv6 b -> Ipaddr.(compare (V4 a) (V6 b)) = 0
  | IPv6 a, IPv4 b -> Ipaddr.(compare (V6 a) (V4 b)) = 0
  | Extension (ka, va), Extension (kb, vb) ->
      String.equal ka kb && String.equal va vb
  | Domain a, Domain b -> (
      try List.for_all2 String.equal a b with _ -> false)
  | _, _ -> false

let error_msgf fmt = Format.kasprintf (fun err -> Error (`Msg err)) fmt

let compare a b =
  let sup = 1 and inf = -1 in
  match (a, b) with
  | Domain a, Domain b ->
      let rec go a b =
        match (a, b) with
        | [], [] -> 0
        | a :: ar, b :: br ->
            let res =
              String.compare (String.lowercase_ascii a)
                (String.lowercase_ascii b) in
            if res = 0 then go ar br else res
        | [], _ :: _ -> inf
        | _ :: _, [] -> sup in
      go a b
  | IPv4 ipv4, IPv6 ipv6 | IPv6 ipv6, IPv4 ipv4 ->
      Ipaddr.(compare (V4 ipv4) (V6 ipv6))
  | IPv6 a, IPv6 b -> Ipaddr.V6.compare a b
  | IPv4 a, IPv4 b -> Ipaddr.V4.compare a b
  | Extension (ka, va), Extension (kb, vb) ->
      let ret = String.compare ka kb in
      if ret = 0 then String.compare va vb else ret
  | Domain _, _ -> sup
  | (IPv4 _ | IPv6 _), Domain _ -> inf
  | IPv6 _, _ -> sup
  | IPv4 _, _ -> sup
  | Extension _, (Domain _ | IPv4 _ | IPv6 _) -> inf

let pp ppf = function
  | IPv4 ipv4 -> Ipaddr.V4.pp ppf ipv4
  | IPv6 ipv6 -> Ipaddr.V6.pp ppf ipv6
  | Extension (k, v) -> Fmt.pf ppf "%s:%s" k v
  | Domain l -> Fmt.pf ppf "%a" Fmt.(list ~sep:(const string ".") string) l

module Decoder = struct
  open Angstrom

  let ( or ) a b x = a x || b x

  let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false

  let is_digit = function '0' .. '9' -> true | _ -> false

  let is_dash = ( = ) '-'

  let let_dig = satisfy (is_alpha or is_digit)

  (* XXX(dinosaure): Ldh-str = *( ALPHA / DIGIT / "-" ) Let-dig
   * and Let-dig = ALPHA / DIGIT
   *
   * This [ldh_str] does not strictly follow the definition but just
   * eats [*( ALPHA / DIGIT / "-" )] and check that the last character
   * **is not** a dash. *)
  let ldh_str =
    take_while1 (is_alpha or is_digit or is_dash) >>= fun res ->
    if String.get res (String.length res - 1) <> '-'
    then return res
    else fail "Invalid ldh-str token"

  let sub_domain =
    let_dig >>= fun pre ->
    option "" ldh_str >>| fun lst -> String.concat "" [ String.make 1 pre; lst ]

  let domain =
    sub_domain >>= fun x ->
    many (char '.' *> sub_domain) >>| fun r -> Domain (x :: r)

  (* XXX(dinosaure): from mrmime. *)

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
        with Ipaddr.Parse_error _ -> None)
    >>= function
    | Some v -> return v
    | None -> fail "ipv4_address_literal"

  let ipv6_addr =
    Unsafe.take_while1 is_dcontent (fun buf ~off ~len ->
        let raw = Bigstringaf.substring buf ~off ~len in
        let pos = ref 0 in
        try
          let res = Ipaddr.V6.of_string_raw raw pos in
          if !pos = len then Some res else None
        with Ipaddr.Parse_error _ -> None)
    >>= function
    | Some v -> return v
    | None -> fail "ipv6_addr"

  let ipv6_address_literal = string "IPv6:" *> ipv6_addr

  let failf fmt = Fmt.kstrf fail fmt

  let ldh_str =
    take_while1 (function
      | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' -> true
      | _ -> false)
    >>= fun ldh ->
    if String.unsafe_get ldh (String.length ldh - 1) = '-'
    then failf "ldh_str: %s is invalid" ldh
    else return ldh

  let general_address_literal =
    ldh_str <* char ':' >>= fun ldh ->
    take_while1 is_dcontent >>| fun value -> Extension (ldh, value)

  let address_literal =
    char '['
    *> (ipv4_address_literal
       >>| (fun v -> IPv4 v)
       <|> (ipv6_address_literal >>| fun v -> IPv6 v)
       <|> general_address_literal)
    <* char ']'

  let of_string_exn x =
    match parse_string ~consume:Consume.All (domain <|> address_literal) x with
    | Ok v -> v
    | Error _ -> Fmt.invalid_arg "Invalid domain: %s" x

  let of_string x =
    match parse_string ~consume:Consume.All (domain <|> address_literal) x with
    | Ok _ as v -> v
    | Error _ -> error_msgf "Invalid domain: %S" x
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

let of_string = Decoder.of_string

let of_string_exn = Decoder.of_string_exn

let to_string = Encoder.to_string

exception Break

let satisfy predicate x =
  let len = String.length x in
  try
    for i = 0 to len - 1 do
      if not (predicate x.[i]) then raise Break
    done ;
    true
  with Break -> false

let extension k v =
  let is_ldh = Decoder.(is_alpha or is_digit or is_dash) in
  let is_dcontent = Decoder.is_dcontent in
  if String.length k > 0
     && satisfy is_ldh k
     && k.[String.length k - 1] <> '-'
     && String.length v > 0
     && satisfy is_dcontent v
  then Ok (Extension (k, v))
  else error_msgf "Invalid key:%S or value:%S" k v

let is_atext_valid_string _ = true

type atom = string

let atom x =
  if is_atext_valid_string x
  then Ok x
  else error_msgf "atom %S does not respect standards" x

let atom_exn x =
  match atom x with Ok v -> v | Error (`Msg err) -> invalid_arg err

let a = atom_exn

module Peano = struct
  type z = Z

  type 'a s = S
end

let unsafe_domain_of_list_exn = function
  | [] -> Fmt.invalid_arg "A domain must contain at least one element"
  | domain -> Domain domain

type 'a domain =
  | ( :: ) : atom * 'a domain -> 'a Peano.s domain
  | [] : Peano.z domain

let rec coerce : type a. a Peano.s domain -> string list = function
  | [ x ] -> [ x ]
  | x :: y :: r -> List.cons x (coerce (y :: r))

let make_domain : type a. a domain -> (string list, [> `Msg of string ]) result
    = function
  | [] -> error_msgf "A domain must contain at least one element"
  | x :: r -> Ok (coerce (x :: r))

type 'a w =
  | WDomain : 'a domain w
  | WIPv4 : Ipaddr.V4.t w
  | WIPv6 : Ipaddr.V6.t w

let domain = WDomain

let ipv4 = WIPv4

let ipv6 = WIPv6

let make : type a. a w -> a -> (t, [> `Msg of string ]) result =
 fun witness v ->
  match witness with
  | WDomain ->
      let ( >>| ) x f =
        match x with Ok x -> Ok (f x) | Error err -> Error err in
      make_domain v >>| fun v -> Domain v
  | WIPv4 -> Ok (IPv4 v)
  | WIPv6 -> Ok (IPv6 v)

let v : type a. a w -> a -> t =
 fun witness v ->
  match make witness v with Ok v -> v | Error (`Msg err) -> invalid_arg err
