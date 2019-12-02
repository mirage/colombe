type ('a, 'err) t =
  | Read of { buffer : bytes; off: int; len: int; k: int -> ('a, 'err) t }
  | Write of { buffer : string; off: int; len: int; k: int -> ('a, 'err) t }
  | Return of 'a
  | Error of 'err

type context =
  { encoder : Encoder.encoder
  ; decoder : Decoder.decoder }

val make_context : unit -> context

module type S = sig
  type 'a send
  type 'a recv

  type error

  val encode : Encoder.encoder -> 'a send -> 'a -> error Encoder.state
  val decode : Decoder.decoder -> 'a recv -> ('a, error) Decoder.state
end

module Scheduler (Value : S) : sig
  val bind : ('a, 'err) t -> f:('a -> ('b, 'err) t) -> ('b, 'err) t

  val ( let* ) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t
  val ( >>= ) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t

  val encode : context -> 'a Value.send -> 'a -> (context -> ('b, Value.error) t) -> ('b, Value.error) t
  val decode : context -> 'a Value.recv -> (context -> 'a -> ('b, Value.error) t) -> ('b, Value.error) t

  val send : context -> 'a Value.send -> 'a -> (unit, Value.error) t
  val recv : context -> 'a Value.recv -> ('a, Value.error) t

  val return : 'v -> ('v, 'err) t
  val fail : 'err -> ('v, 'err) t

  val error_msgf : ('a, Format.formatter, unit, ('b, [> Rresult.R.msg ]) t) format4 -> 'a
end
