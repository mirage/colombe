type t =
  [ `Hello of Domain.t
  | `Mail of Reverse_path.t * (string * string option) list
  | `Recipient of Forward_path.t * (string * string option) list
  | `Expand of string
  | `Data
  | `Data_end
  | `Help of string option
  | `Noop of string option
  | `Verify of string
  | `Reset
  | `Quit
  | `Extension of Rfc1869.t ]

val equal : t -> t -> bool
val pp : t Fmt.t

module Decoder : sig
  val request : Decoder.decoder -> t Decoder.state

  val of_string : string -> (t, Decoder.error) result
  val of_string_raw : string -> int ref -> (t, Decoder.error) result
end

module Encoder : sig
  val request : t -> Encoder.encoder -> Encoder.state
  val to_string : t -> (string, Encoder.error) result
end
