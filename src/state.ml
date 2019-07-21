type ('s, 'error) process =
  | Read of { buffer : bytes
            ; off : int
            ; len : int
            ; k : int -> ('s, 'error) process }
  | Write of { buffer : string
             ; off : int
             ; len : int
             ; k : int -> ('s, 'error) process }
  | Return of 's
  | Error of 'error

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
    : (string * int * int) ->
      (ctx -> int -> ('s, error) process) ->
      ctx -> ('s, error) process
  val decode_raw
    : (bytes * int * int) ->
      (ctx -> int -> ('s, error) process) ->
      ctx -> ('s, error) process
end

module Make (State : Sigs.FUNCTOR) (Protocol : PROTOCOL) = struct
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

  let send : 'x Protocol.t -> 'x -> action = fun k v -> Send (k, v)
  let recv : 'x Protocol.t -> action = fun v -> Recv v
  let write ~buf ~off ~len = Write { buf; off; len; }
  let read ~buf ~off ~len = Read { buf; off; len; }

  type 's t =
    { init : 's state
    ; trans : 's state -> event -> (action * 's state, Protocol.error * 's state) result }

  type 's transition = 's state -> event -> (action * 's state, Protocol.error * 's state) result

  let run t ctx e =
    let rec go ctx q e = match t.trans q e with
      | Ok (Recv w, q') ->
        Protocol.decode w (fun ctx v -> go ctx q' (Recv (w, v))) ctx
      | Ok (Send (w, v), q') ->
        Protocol.encode (w, v) (fun ctx -> go ctx q' (Send w)) ctx
      | Ok (Close, q') -> Return q'
      | Ok (Write { buf; off; len; }, q') ->
        Protocol.encode_raw (buf, off, len)
          (fun ctx len -> go ctx q' (Write len)) ctx
      | Ok (Read { buf; off; len; }, q') ->
        Protocol.decode_raw (buf, off, len)
          (fun ctx len -> go ctx q' (Read len)) ctx
      | Error (err, _) -> Error err in
    go ctx t.init e

  let make ~init trans = { init; trans; }
end
