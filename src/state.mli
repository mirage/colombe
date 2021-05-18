type ('a, 'err) t =
  | Read of { buffer : bytes; off : int; len : int; k : int -> ('a, 'err) t }
  | Write of { buffer : string; off : int; len : int; k : int -> ('a, 'err) t }
  | Return of 'a
  | Error of 'err

val reword_error : ('a -> 'b) -> ('v, 'a) t -> ('v, 'b) t

module Context : sig
  type t = { encoder : Encoder.encoder; decoder : Decoder.decoder }

  type decoder = Decoder.decoder

  type encoder = Encoder.encoder

  val pp : t Fmt.t

  val encoder : t -> encoder

  val decoder : t -> decoder

  val make : unit -> t
end

module type S = sig
  type 'a send

  type 'a recv

  type error

  type encoder

  type decoder

  val encode : encoder -> 'a send -> 'a -> (unit, error) t

  val decode : decoder -> 'a recv -> ('a, error) t
end

module type C = sig
  type t

  type encoder

  type decoder

  val pp : t Fmt.t

  val encoder : t -> encoder

  val decoder : t -> decoder
end

module Scheduler
    (Context : C)
    (Value : S
               with type encoder = Context.encoder
                and type decoder = Context.decoder) : sig
  type error = Value.error

  val bind : ('a, 'err) t -> f:('a -> ('b, 'err) t) -> ('b, 'err) t

  val ( let* ) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t

  val ( >>= ) : ('a, 'err) t -> ('a -> ('b, 'err) t) -> ('b, 'err) t

  val encode :
    Context.t ->
    'a Value.send ->
    'a ->
    (Context.t -> ('b, ([> `Protocol of error ] as 'err)) t) ->
    ('b, 'err) t

  val decode :
    Context.t ->
    'a Value.recv ->
    (Context.t -> 'a -> ('b, ([> `Protocol of error ] as 'err)) t) ->
    ('b, 'err) t

  val send :
    Context.t -> 'a Value.send -> 'a -> (unit, [> `Protocol of error ]) t

  val recv : Context.t -> 'a Value.recv -> ('a, [> `Protocol of error ]) t

  val return : 'v -> ('v, 'err) t

  val fail : 'err -> ('v, 'err) t

  val reword_error : ('err0 -> 'err1) -> ('v, 'err0) t -> ('v, 'err1) t

  val error_msgf :
    ('a, Format.formatter, unit, ('b, [> `Msg of string ]) t) format4 -> 'a
end
