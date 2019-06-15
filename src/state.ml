type ('s, 'error) process =
  | Rd of { buffer : bytes
          ; off: int
          ; len: int
          ; k: int -> ('s, 'error) process }
  | Wr of { buffer : bytes
          ; off: int
          ; len: int
          ; k: int -> ('s, 'error) process }
  | Rt of 's
  | Er of 'error

type ctx =
  { encoder : Encoder.encoder
  ; decoder : Decoder.decoder }

let make_ctx () =
  { encoder= Encoder.encoder ()
  ; decoder= Decoder.decoder () }

module type PROTOCOL = sig
  type 'a t

  type error

  val decode : 'i t -> (ctx -> 'i -> ('s, error) process) -> ctx -> ('s, error) process
  val encode : ('o t * 'o) -> (ctx -> ('s, error) process) -> ctx -> ('s, error) process

  val encode_raw
    : (bytes * int * int) -> (ctx -> int -> ('s, error) process) -> ctx -> ('s, error) process
end

module Make (State : Sigs.FUNCTOR) (Protocol : PROTOCOL) = struct
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

  type 's t =
    { i : 's state
    ; t : 's state -> event -> (action * 's state, Protocol.error * 's state) result }

  let run t ctx e =
    let rec go ctx q e = match t.t q e with
      | Ok (Recv w, q') ->
        Protocol.decode w (fun ctx v -> go ctx q' (Recv (w, v))) ctx
      | Ok (Send (w, v), q') ->
        Protocol.encode (w, v) (fun ctx -> go ctx q' (Send w)) ctx
      | Ok (Close, q') -> Rt q'
      | Ok (Wr { buf; off; len; }, q') ->
        Protocol.encode_raw (buf, off, len)
          (fun ctx len -> go ctx q' (Wr len)) ctx
      | Error (err, _) -> Er err in
    go ctx t.i e

  let make ~i t = { i; t; }
end
