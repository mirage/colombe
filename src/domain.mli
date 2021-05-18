type t =
  | IPv4 of Ipaddr.V4.t
  | IPv6 of Ipaddr.V6.t
  | Extension of string * string
  | Domain of string list

val compare : t -> t -> int

val equal : t -> t -> bool

val pp : t Fmt.t

module Decoder : sig
  val ( or ) : ('a -> bool) -> ('a -> bool) -> 'a -> bool

  val is_alpha : char -> bool

  val is_digit : char -> bool

  val is_dash : char -> bool

  val is_dcontent : char -> bool

  val ipv4_address_literal : Ipaddr.V4.t Angstrom.t

  val ipv6_addr : Ipaddr.V6.t Angstrom.t

  val address_literal : t Angstrom.t

  val domain : t Angstrom.t
end

val of_string : string -> (t, [ `Msg of string ]) result

val of_string_exn : string -> t

val to_string : t -> string

val extension : string -> string -> (t, [ `Msg of string ]) result

type atom

val atom : string -> (atom, [ `Msg of string ]) result

val atom_exn : string -> atom

val a : string -> atom

module Peano : sig
  type z = Z

  type 'a s = S
end

type 'a domain =
  | ( :: ) : atom * 'a domain -> 'a Peano.s domain
  | [] : Peano.z domain

val unsafe_domain_of_list_exn : string list -> t

type 'a w

val domain : 'a domain w

val ipv4 : Ipaddr.V4.t w

val ipv6 : Ipaddr.V6.t w

val make : 'a w -> 'a -> (t, [ `Msg of string ]) result

val v : 'a w -> 'a -> t
