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
type pos_intermediate = [ `TP_354 of string list ]

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

val code : t -> int

val lines : t -> string list

module Decoder : sig
  type error = [ `Invalid_code of int | Decoder.error ]

  val pp_error : error Fmt.t

  val response : Decoder.decoder -> (t, [> error ]) Decoder.state

  val of_string : string -> (t, [> error ]) result

  val of_string_raw : string -> int ref -> (t, [> error ]) result
end

module Encoder : sig
  type error = Encoder.error

  val pp_error : error Fmt.t

  val response : t -> Encoder.encoder -> [> error ] Encoder.state

  val to_string : t -> (string, error) result
end
