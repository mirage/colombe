module Domain : sig
  type t =
    | IPv4 of Ipaddr.V4.t
    | IPv6 of Ipaddr.V6.t
    | Extension of string * string
    | Domain of string list

  val of_string : string -> t
end

module Reverse_path : sig
  type path =
    { local : [ `String of string | `Dot_string of string list ]
    ; domain : Domain.t
    ; rest : Domain.t list }
  and t = path option

  val of_string : string -> (t * (string * string option) list)
end

module Forward_path : sig
  type t =
    | Postmaster
    | Domain of Domain.t
    | Forward_path of forward_path
  and forward_path = Reverse_path.path

  val of_string : string -> (t * (string * string option) list)
end

module Request : sig
  type t =
    [ `Hello of Domain.t
    | `Mail of Reverse_path.t * (string * string option) list
    | `Recipient of Forward_path.t * (string * string option) list
    | `Expand of string
    | `Data of string
    | `Help of string option
    | `Noop of string option
    | `Verify of string
    | `Reset
    | `Quit ]

  val pp : t Fmt.t
end

module Decoder : sig
  type decoder

  type error =
    | End_of_input
    | Expected_char of char
    | Unexpected_char of char
    | Expected_string of string
    | Invalid_command of string
    | Expected_eol
    | No_enough_space
    | Assert_predicate of (char -> bool)

  val pp_error : error Fmt.t

  type 'v state =
    | Ok of 'v
    | Read of { buffer : bytes
              ; off : int
              ; len : int
              ; continue : int -> 'v state }
    | Error of info
  and info = { error : error
             ; buffer : bytes
             ; committed : int }

  val decoder : unit -> decoder
  val decoder_from : string -> decoder
  val request : decoder -> Request.t state
  val of_string : string -> (Request.t, error) result
end
