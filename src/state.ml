let ( <.> ) f g x = f (g x)

type ('a, 'err) t =
  | Read of { buffer : bytes; off : int; len : int; k : int -> ('a, 'err) t }
  | Write of { buffer : string; off : int; len : int; k : int -> ('a, 'err) t }
  | Return of 'a
  | Error of 'err

let rec reword_error : type v a b. (a -> b) -> (v, a) t -> (v, b) t =
 fun f -> function
  | Error err -> Error (f err)
  | Read { k; buffer; off; len } ->
      Read { k = reword_error f <.> k; buffer; off; len }
  | Write { k; buffer; off; len } ->
      Write { k = reword_error f <.> k; buffer; off; len }
  | Return _ as x -> x

module Context = struct
  type t = { encoder : Encoder.encoder; decoder : Decoder.decoder }

  type encoder = Encoder.encoder

  type decoder = Decoder.decoder

  let pp ppf t =
    Fmt.pf ppf "{ @[<hov>encoder= @[<hov>%a@];@ decoder= @[<hov>%a@]@] }"
      Encoder.pp t.encoder Decoder.pp t.decoder

  let make () = { encoder = Encoder.encoder (); decoder = Decoder.decoder () }

  let encoder { encoder; _ } = encoder

  let decoder { decoder; _ } = decoder
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
                and type decoder = Context.decoder) =
struct
  type error = Value.error

  let rec go ~f m len =
    match m len with
    | Return v -> f v
    | Read { k; off; len; buffer } -> Read { k = go ~f k; off; len; buffer }
    | Write { k; off; len; buffer } -> Write { k = go ~f k; off; len; buffer }
    | Error err -> Error err

  let bind : ('a, 'err) t -> f:('a -> ('b, 'err) t) -> ('b, 'err) t =
   fun m ~f ->
    match m with
    | Return v -> f v
    | Error err -> Error err
    | Read { k; off; len; buffer } -> Read { k = go ~f k; off; len; buffer }
    | Write { k; off; len; buffer } -> Write { k = go ~f k; off; len; buffer }

  let ( let* ) m f = bind m ~f

  let ( >>= ) m f = bind m ~f

  let encode :
      type a.
      Context.t ->
      a Value.send ->
      a ->
      (Context.t -> ('b, [> `Protocol of error ]) t) ->
      ('b, [> `Protocol of error ]) t =
   fun ctx w v k ->
    let rec go = function
      | Return () -> k ctx
      | Write { k; buffer; off; len } ->
          Write { k = go <.> k; buffer; off; len }
      | Read { k; buffer; off; len } -> Read { k = go <.> k; buffer; off; len }
      | Error err -> Error (`Protocol err) in
    go (Value.encode (Context.encoder ctx) w v)

  let send :
      type a.
      Context.t -> a Value.send -> a -> (unit, [> `Protocol of error ]) t =
   fun ctx w x -> encode ctx w x (fun _ctx -> Return ())

  let decode :
      type a.
      Context.t ->
      a Value.recv ->
      (Context.t -> a -> ('b, [> `Protocol of error ]) t) ->
      ('b, [> `Protocol of error ]) t =
   fun ctx w k ->
    let rec go : (a, 'err) t -> ('b, [> `Protocol of error ]) t = function
      | Read { k; buffer; off; len } -> Read { k = go <.> k; buffer; off; len }
      | Write { k; buffer; off; len } ->
          Write { k = go <.> k; buffer; off; len }
      | Return v -> k ctx v
      | Error err -> Error (`Protocol err) in
    go (Value.decode (Context.decoder ctx) w)

  let recv : type a. Context.t -> a Value.recv -> (a, [> `Protocol of error ]) t
      =
   fun ctx w -> decode ctx w (fun _ctx v -> Return v)

  let reword_error f x =
    let rec go = function
      | Read { k; buffer; off; len } -> Read { k = go <.> k; buffer; off; len }
      | Write { k; buffer; off; len } ->
          Write { k = go <.> k; buffer; off; len }
      | Return v -> Return v
      | Error err -> Error (f err) in
    go x

  let return v = Return v

  let fail error = Error error

  let error_msgf fmt = Fmt.kstrf (fun err -> Error (`Msg err)) fmt
end
