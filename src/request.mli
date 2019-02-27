module Request : sig
  type t =
    [ `Hello of Domain.t
    | `Mail of Reverse_path.t * (string * string option) list
    | `Recipient of Forward_path.t * (string * string option) list
    | `Expand of string
    | `Data
    | `Help of string option
    | `Noop of string option
    | `Verify of string
    | `Reset
    | `Quit ]

  val equal : t -> t -> bool

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
    | Expected_eol_or_space
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

module Encoder : sig
  type encoder

  type error = No_enough_space

  val pp_error : error Fmt.t

  type state =
    | Write of { buffer : Bytes.t
               ; off : int
               ; len : int
               ; continue : int -> state }
    | Error of error
    | Ok

  val encoder : unit -> encoder
  val request : Request.t -> encoder -> state
  val to_string : Request.t -> (string, error) result
end
