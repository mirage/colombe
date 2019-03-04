(* Permanent positive response *)
type pos_completion =
  [ `PP_211 of string list
  | `PP_214 of string list
  | `PP_220 of string list
  | `PP_221 of string list
  | `PP_250 of string list
  | `PP_251 of string list
  | `PP_252 of string list ]

(* Temporary positive response *)
type pos_intermediate =
  [ `TP_354 of string list ]

(* Temporary negativ response *)
type transient_neg_completion =
  [ `TN_421 of string list
  | `TN_450 of string list
  | `TN_451 of string list
  | `TN_452 of string list
  | `TN_455 of string list ]

(* Permanent negativ response *)
type permanent_neg_completion =
  [ `PN_500 of string list
  | `PN_501 of string list
  | `PN_502 of string list
  | `PN_503 of string list
  | `PN_504 of string list
  | `PN_550 of string list
  | `PN_551 of string list
  | `PN_552 of string list
  | `PN_553 of string list
  | `PN_554 of string list
  | `PN_555 of string list ]

type t =
  [ pos_completion
  | pos_intermediate
  | transient_neg_completion
  | permanent_neg_completion
  | `Other of int * string list ]

val pp : t Fmt.t
val compare : t -> t -> int
val equal : t -> t -> bool
val v : int -> string list -> t

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
    | Invalid_code of int

  val pp_error : error Fmt.t

  type 'v state =
    | Ok of 'v
    | Read of { buffer : Bytes.t; off : int; len : int; continue : int -> 'v state }
    | Error of info
  and info = { error : error; buffer : Bytes.t; committed : int }

  val decoder_from : string -> decoder
  val decoder : unit -> decoder

  val response : decoder -> t state

  val of_string : string -> (t, error) result
  val of_string_raw : string -> int ref -> (t, error) result
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

  val response : t -> encoder -> state

  val to_string : t -> (string, error) result
end
