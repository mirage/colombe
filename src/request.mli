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
  | `Verb of string * string list
  | `Payload of string ]

val equal : t -> t -> bool

val pp : t Fmt.t

module Decoder : sig
  type error =
    [ `Invalid_command of string | `Invalid_domain of string | Decoder.error ]

  val pp_error : error Fmt.t

  val add_extension : string -> unit

  val request : ?relax:bool -> Decoder.decoder -> (t, [> error ]) Decoder.state

  val of_string : string -> (t, [> error ]) result

  val of_string_raw : string -> int ref -> (t, [> error ]) result
end

module Encoder : sig
  type error = Encoder.error

  val pp_error : error Fmt.t

  val request : t -> Encoder.encoder -> [> error ] Encoder.state

  val to_string : t -> (string, error) result
end
