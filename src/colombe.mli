module Reply: sig
  (* Permanent positive response *)
  type pos_completion = [
    | `PP_211 of string list
    | `PP_214 of string list
    | `PP_220 of string list
    | `PP_221 of string list
    | `PP_250 of string list
    | `PP_251 of string list
    | `PP_252 of string list
    ]

  (* Temporary positive response *)
  type pos_intermediate = [
    | `TP_354 of string list
    ]

  (* Temporary negativ response *)
  type transient_neg_completion = [
    | `TN_421 of string list
    | `TN_450 of string list
    | `TN_451 of string list
    | `TN_452 of string list
    | `TN_455 of string list
    ]

  (* Permanent negativ response *)
  type permanent_neg_completion = [
    | `PN_500 of string list
    | `PN_501 of string list
    | `PN_502 of string list
    | `PN_503 of string list
    | `PN_504 of string list
    | `PN_550 of string list
    | `PN_551 of string list
    | `PN_552 of string list
    | `PN_553 of string list
    | `PN_554 of string list
    | `PN_555 of string list
    ]

  type t = [
    | pos_completion
    | pos_intermediate
    | transient_neg_completion
    | permanent_neg_completion
    | `Other of int * string list
    ]

  val pp: Format.formatter -> t -> unit

  val eval: string -> t

  val code: t -> int

  val texts: t -> string list
end