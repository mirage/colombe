type ('s, 'error) process =
  | Read of { buffer : bytes; off: int; len: int; k: int -> ('s, 'error) process }
  | Write of { buffer : string; off: int; len: int; k: int -> ('s, 'error) process }
  | Return of 's
  | Error of 'error

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
    : (string * int * int) -> (ctx -> int -> ('s, error) process) -> ctx -> ('s, error) process
  val decode_raw
    : (bytes * int * int) -> (ctx -> int -> ('s, error) process) -> ctx -> ('s, error) process
end

module Make (State : Sigs.FUNCTOR) (Protocol : PROTOCOL) : sig
  type 's state = 's State.t

  type event =
    | Accept
    | Recv : 'x Protocol.t * 'x -> event
    | Send : 'x Protocol.t -> event
    | Write of int
    | Read of int
    | Close

  type action =
    | Send : 'x Protocol.t * 'x -> action
    | Recv : 'x Protocol.t -> action
    | Write of { buf : string; off : int; len : int; }
    | Read of { buf : bytes; off : int; len : int; }
    | Close

  type 's t
  type 's transition = 's state -> event -> (action * 's state, Protocol.error * 's state) result

  val send : 'x Protocol.t -> 'x -> action
  val recv : 'x Protocol.t -> action
  val write : buf:string -> off:int -> len:int -> action
  val read : buf:bytes -> off:int -> len:int -> action

  val run : 's t -> ctx -> event -> ('s state, Protocol.error) process
  val make : init:'s state -> 's transition -> 's t
end
