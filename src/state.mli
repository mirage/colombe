type ('s, 'error) process =
  | Rd of { buffer : bytes; off: int; len: int; k: int -> ('s, 'error) process }
  | Wr of { buffer : bytes; off: int; len: int; k: int -> ('s, 'error) process }
  | Rt of 's
  | Er of 'error

type ctx =
  { encoder : Encoder.encoder
  ; decoder : Decoder.decoder }

val make_ctx : unit -> ctx

module type PROTOCOL = sig
  type 'a t

  type error

  val decode : 'i t -> (ctx -> 'i -> ('s, error) process) -> ctx -> ('s, error) process
  val encode : ('o t * 'o) -> (ctx -> ('s, error) process) -> ctx -> ('s, error) process

  val encode_raw
    : (bytes * int * int) -> (ctx -> int -> ('s, error) process) -> ctx -> ('s, error) process
end

module Make (State : Sigs.FUNCTOR) (Protocol : PROTOCOL) : sig
  type 's state = 's State.t

  type event =
    | Accept
    | Recv : 'x Protocol.t * 'x -> event
    | Send : 'x Protocol.t -> event
    | Wr of int
    | Close

  type action =
    | Send : 'x Protocol.t * 'x -> action
    | Recv : 'x Protocol.t -> action
    | Wr of { buf : bytes; off : int; len : int; }
    | Close

  type 's t

  val run : 's t -> ctx -> event -> ('s state, Protocol.error) process

  val make :
    i:'s state ->
    ('s state -> event -> (action * 's state, Protocol.error * 's state) result) ->
    's t
end
