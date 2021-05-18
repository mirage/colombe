type decoder = { buffer : Bytes.t; mutable pos : int; mutable max : int }

let pp ppf { buffer; pos; max } =
  Fmt.pf ppf "%S" (Bytes.sub_string buffer pos (max - pos))

let io_buffer_size = 65536

let decoder () = { buffer = Bytes.create io_buffer_size; pos = 0; max = 0 }

let decoder_from x =
  let max = String.length x in
  let buffer = Bytes.of_string x in
  { buffer; pos = 0; max }

type error =
  [ `End_of_input
  | `Expected_char of char
  | `Unexpected_char of char
  | `Expected_string of string
  | (* | Invalid_command of string *)
    `Expected_eol
  | `Expected_eol_or_space
  | `Not_enough_space
  | `Assert_predicate of char -> bool ]
(* | Invalid_code of int *)

let pp_error ppf = function
  | `End_of_input -> Fmt.string ppf "End of input"
  | `Expected_char chr -> Fmt.pf ppf "Expected char: %02x" (Char.code chr)
  | `Unexpected_char chr -> Fmt.pf ppf "Unexpected char: %02x" (Char.code chr)
  | `Expected_string s -> Fmt.pf ppf "Expected_string: %s" s
  | `Expected_eol -> Fmt.string ppf "Expected end-of-line"
  | `Expected_eol_or_space -> Fmt.string ppf "Expected end-of-line or space"
  | `Not_enough_space -> Fmt.string ppf "Not enough space"
  | `Assert_predicate _ -> Fmt.string ppf "Assert predicate"

type 'err info = { error : 'err; buffer : Bytes.t; committed : int }

type ('v, 'err) state =
  | Done of 'v
  | Read of {
      buffer : Bytes.t;
      off : int;
      len : int;
      continue : int -> ('v, 'err) state;
    }
  | Error of 'err info

exception Leave of error info

let return (type v) (v : v) _ : (v, 'err) state = Done v

let safe :
    (decoder -> ('v, ([> error ] as 'err)) state) -> decoder -> ('v, 'err) state
    =
 fun k decoder ->
  try k decoder
  with Leave { error = #error as error; buffer; committed } ->
    Error { error = (error :> 'err); buffer; committed }

let end_of_input decoder = decoder.max

let peek_char decoder =
  if decoder.pos < end_of_input decoder
  then Some (Bytes.unsafe_get decoder.buffer decoder.pos)
  else None
(* XXX(dinosaure): in [angstrom] world, [peek_char] should try to read input
    again. However, SMTP is a line-directed protocol where we can ensure to
    have the full line at the top (with a queue) instead to have a
    systematic check (which slow-down the process). *)

let leave_with (decoder : decoder) error =
  raise (Leave { error; buffer = decoder.buffer; committed = decoder.pos })

let fail (decoder : decoder) error =
  Error { error; buffer = decoder.buffer; committed = decoder.pos }

let string str decoder =
  let idx = ref 0 in
  let len = String.length str in
  while
    decoder.pos + !idx < end_of_input decoder
    && !idx < len
    && Char.equal
         (Bytes.unsafe_get decoder.buffer (decoder.pos + !idx))
         (String.unsafe_get str !idx)
  do
    incr idx
  done ;
  if !idx = len
  then decoder.pos <- decoder.pos + len
  else leave_with decoder (`Expected_string str)

let junk_char decoder =
  if decoder.pos < end_of_input decoder
  then decoder.pos <- decoder.pos + 1
  else leave_with decoder `End_of_input

let while1 predicate decoder =
  let idx = ref decoder.pos in
  while
    !idx < end_of_input decoder
    && predicate (Bytes.unsafe_get decoder.buffer !idx)
  do
    incr idx
  done ;
  if !idx - decoder.pos = 0
  then leave_with decoder (`Assert_predicate predicate) ;
  let sub = (decoder.buffer, decoder.pos, !idx - decoder.pos) in
  (* XXX(dinosaure): avoid sub-string operation. *)
  decoder.pos <- !idx ;
  sub

let at_least_one_line ?(relax = true) decoder =
  let pos = ref decoder.pos in
  let chr = ref '\000' in
  let has_cr = ref false in
  while
    !pos < decoder.max
    &&
    (chr := Bytes.unsafe_get decoder.buffer !pos ;
     not (!chr = '\n' && (!has_cr || relax)))
  do
    has_cr := !chr = '\r' ;
    incr pos
  done ;
  !pos < decoder.max && !chr = '\n' && (!has_cr || relax)

let prompt :
    ?relax:bool ->
    (decoder -> ('v, ([> error ] as 'err)) state) ->
    decoder ->
    ('v, 'err) state =
 fun ?relax k decoder ->
  if decoder.pos > 0
  then (
    (* XXX(dinosaure): compress *)
    let rest = decoder.max - decoder.pos in
    Bytes.unsafe_blit decoder.buffer decoder.pos decoder.buffer 0 rest ;
    decoder.max <- rest ;
    decoder.pos <- 0) ;
  let rec go off =
    if off = Bytes.length decoder.buffer
    then
      Error
        {
          error = `Not_enough_space;
          buffer = decoder.buffer;
          committed = decoder.pos;
        }
    else if not (at_least_one_line ?relax { decoder with max = off })
            (* XXX(dinosaure): we make a new decoder here and we did __not__ set [decoder.max] owned by end-user,
               and this is exactly what we want. *)
    then
      Read
        {
          buffer = decoder.buffer;
          off;
          len = Bytes.length decoder.buffer - off;
          continue = (fun len -> go (off + len));
        }
    else (
      decoder.max <- off ;
      safe k decoder) in
  go decoder.max

let peek_while_eol ?(relax = false) decoder =
  let idx = ref decoder.pos in
  let chr = ref '\000' in
  let has_cr = ref false in

  while
    !idx < end_of_input decoder
    &&
    (chr := Bytes.unsafe_get decoder.buffer !idx ;
     not (!chr == '\n' && (!has_cr || relax)))
  do
    has_cr := !chr == '\r' ;
    incr idx
  done ;

  if !idx < end_of_input decoder && !chr == '\n' && (!has_cr || relax)
  then (
    assert (!idx + 1 - decoder.pos > 0) ;
    (decoder.buffer, decoder.pos, !idx + 1 - decoder.pos))
  else leave_with decoder `Expected_eol

let peek_while_eol_or_space ?(relax = false) decoder =
  let idx = ref decoder.pos in
  let chr = ref '\000' in
  let has_cr = ref false in

  while
    !idx < end_of_input decoder
    &&
    (chr := Bytes.unsafe_get decoder.buffer !idx ;
     (not (!chr = '\n' && (!has_cr || relax))) && !chr <> ' ')
  do
    has_cr := !chr = '\r' ;
    incr idx
  done ;

  if !idx < end_of_input decoder
     && ((!chr = '\n' && (!has_cr || relax)) || !chr = ' ')
  then (decoder.buffer, decoder.pos, !idx + 1 - decoder.pos)
  else leave_with decoder `Expected_eol_or_space
