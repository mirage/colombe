let ( <.> ) f g = fun x -> f (g x)

type ('a, 'err) t =
  | Read of { buffer : bytes
            ; off : int
            ; len : int
            ; k : int -> ('a, 'err) t }
  | Write of { buffer : string
             ; off : int
             ; len : int
             ; k : int -> ('a, 'err) t }
  | Return of 'a
  | Error of 'err

type context =
  { encoder : Encoder.encoder
  ; decoder : Decoder.decoder }

let make_context () =
  { encoder= Encoder.encoder ()
  ; decoder= Decoder.decoder () }

module type S = sig
  type 'a send
  type 'a recv

  type error

  val encode : Encoder.encoder -> 'a send -> 'a -> error Encoder.state
  val decode : Decoder.decoder -> 'a recv -> ('a, error) Decoder.state
end

module Scheduler (Value : S) = struct
  let rec go ~f m len = match m len with
    | Return v -> f v
    | Read { k; off; len; buffer; } ->
      Read { k= go ~f k; off; len; buffer; }
    | Write { k; off; len; buffer; } ->
      Write { k= go ~f k; off; len; buffer; }
    | Error _ as err -> err

  let bind
    : ('a, 'err) t -> f:('a -> ('b, 'err) t) -> ('b, 'err) t
    = fun m ~f -> match m with
    | Return v -> f v
    | Error _ as err -> err
    | Read { k; off; len; buffer; } ->
      Read { k= go ~f k; off; len; buffer; }
    | Write { k; off; len; buffer; } ->
      Write { k= go ~f k; off; len; buffer; }

  let ( let* ) m f = bind m ~f
  let ( >>= ) m f = bind m ~f

  let encode
    : type a. context -> a Value.send -> a -> (context -> ('b, Value.error) t) -> ('b, Value.error) t
    = fun ctx w x k ->
      let rec go : 'err Encoder.state -> ('a, Value.error) t = function
        | Write { continue; off; len; buffer; } ->
          let continue n = go (continue n) in
          Write { k= continue; off; len; buffer; }
        | Done -> k ctx
        | Error err -> Error err in
      (go <.> Value.encode ctx.encoder w) x

  let send : type a. context -> a Value.send -> a -> (unit, Value.error) t
    = fun ctx w x -> encode ctx w x (fun _ctx -> Return ())

  let decode
    : type a. context -> a Value.recv -> (context -> a -> ('b, Value.error) t) -> ('b, Value.error) t
    = fun ctx w k ->
      let rec go : (a, 'err) Decoder.state -> ('b, Value.error) t = function
        | Read { continue; off; len; buffer; } ->
          let continue n = go (continue n) in
          Read { k= continue; off; len; buffer; }
        | Done v -> k ctx v
        | Error { error; _ } -> Error error in
      go (Value.decode ctx.decoder w)

  let recv : type a. context -> a Value.recv -> (a, Value.error) t
    = fun ctx w -> decode ctx w (fun _ctx v -> Return v)

  let return v = Return v
  let fail error = Error error
  let error_msgf fmt = Fmt.kstrf (fun err -> Error (`Msg err)) fmt
end
