type decoder =
  { buffer : Bytes.t
  ; mutable pos : int
  ; mutable max : int }

let pp ppf { buffer; pos; max; } =
  Fmt.pf ppf "%S" (Bytes.sub_string buffer pos (max - pos))

let io_buffer_size = 65536

let decoder () =
  { buffer= Bytes.create io_buffer_size
  ; pos= 0
  ; max= 0 }

let decoder_from x =
  let max = String.length x in
  let buffer = Bytes.of_string x in
  { buffer; pos= 0; max; }

type error =
  | End_of_input
  | Expected_char of char
  | Unexpected_char of char
  | Expected_string of string
  | Invalid_command of string
  | Expected_eol
  | Expected_eol_or_space
  | No_enough_space
  | Assert_predicate of (char -> bool)
  | Invalid_code of int

let pp_error ppf = function
  | End_of_input -> Fmt.string ppf "End_of_input"
  | Expected_char chr -> Fmt.pf ppf "(Expected_char %02x)" (Char.code chr)
  | Unexpected_char chr -> Fmt.pf ppf "(Unexpected_char %02x)" (Char.code chr)
  | Expected_string s -> Fmt.pf ppf "(Expected_string %s)" s
  | Invalid_command s -> Fmt.pf ppf "(Invalid_command %s)" s
  | Expected_eol -> Fmt.string ppf "Expected_eol"
  | Expected_eol_or_space -> Fmt.string ppf "Expected_eol_or_space"
  | No_enough_space -> Fmt.string ppf "No_enough_space"
  | Assert_predicate _ -> Fmt.string ppf "(Assert_predicate #predicate)"
  | Invalid_code c -> Fmt.pf ppf "(Invalid_code %d)" c

type 'v state =
  | Ok of 'v
  | Read of { buffer : Bytes.t; off : int; len : int; continue : int -> 'v state }
  | Error of info
and info = { error : error; buffer : Bytes.t; committed : int }

exception Leave of info

let return (type v) (v : v) _ : v state = Ok v

let safe k decoder : 'v state =
  try k decoder with Leave info -> Error info

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
  raise (Leave { error; buffer= decoder.buffer; committed= decoder.pos; })

let string str decoder =
  let idx = ref 0 in
  let len = String.length str in
  while decoder.pos + !idx < end_of_input decoder
        && !idx < len
        && Char.equal
          (Bytes.unsafe_get decoder.buffer (decoder.pos + !idx))
          (String.unsafe_get str !idx)
  do incr idx done ;
  if !idx = len then decoder.pos <- decoder.pos + len else leave_with decoder (Expected_string str)

let junk_char decoder =
  if decoder.pos < end_of_input decoder
  then decoder.pos <- decoder.pos + 1
  else leave_with decoder End_of_input

let while1 predicate decoder =
  let idx = ref decoder.pos in
  while !idx < end_of_input decoder
        && predicate (Bytes.unsafe_get decoder.buffer !idx)
  do incr idx done ;
  if !idx - decoder.pos = 0
  then leave_with decoder (Assert_predicate predicate) ;
  let sub = decoder.buffer, decoder.pos, !idx - decoder.pos in
  (* XXX(dinosaure): avoid sub-string operation. *)
  decoder.pos <- !idx ; sub

let at_least_one_line decoder =
  let pos = ref decoder.pos in
  let chr = ref '\000' in
  let has_cr = ref false in
  while !pos < decoder.max
        &&  ( chr := Bytes.unsafe_get decoder.buffer !pos
            ; not (!chr = '\n' && !has_cr) )
  do has_cr := !chr = '\r' ; incr pos done ;
  !pos < decoder.max
  && !chr = '\n'
  && !has_cr

let prompt k decoder =
  if decoder.pos > 0
  then (* XXX(dinosaure): compress *)
    (let rest = decoder.max - decoder.pos in
      Bytes.unsafe_blit decoder.buffer decoder.pos decoder.buffer 0 rest ;
      decoder.max <- rest ;
      decoder.pos <- 0 ) ;
  let rec go off =
    if off = Bytes.length decoder.buffer
    then Error { error= No_enough_space; buffer= decoder.buffer; committed= decoder.pos; }
    else if not (at_least_one_line { decoder with max= off })
    (* XXX(dinosaure): we make a new decoder here and we did __not__ set [decoder.max] owned by end-user,
       and this is exactly what we want. *)
    then Read { buffer= decoder.buffer
              ; off
              ; len= Bytes.length decoder.buffer - off
              ; continue= (fun len -> go (off + len)) }
    else
      ( decoder.max <- off ;
        safe k decoder ) in
  go decoder.max

let peek_while_eol decoder =
  let idx = ref decoder.pos in
  let chr = ref '\000' in
  let has_cr = ref false in

  while !idx < end_of_input decoder
        && ( chr := Bytes.unsafe_get decoder.buffer !idx
            ; not (!chr == '\n' && !has_cr) )
  do has_cr := !chr == '\r' ; incr idx done ;

  if !idx < end_of_input decoder && !chr == '\n' && !has_cr
  then ( assert (!idx + 1 - decoder.pos > 1) ; decoder.buffer, decoder.pos, !idx + 1 - decoder.pos )
  else leave_with decoder Expected_eol

let peek_while_eol_or_space decoder =
  let idx = ref decoder.pos in
  let chr = ref '\000' in
  let has_cr = ref false in

  while !idx < end_of_input decoder
        && ( chr := Bytes.unsafe_get decoder.buffer !idx
           ; not (!chr = '\n' && !has_cr) && !chr <> ' ')
  do has_cr := !chr = '\r' ; incr idx done ;

  if !idx < end_of_input decoder && ((!chr = '\n' && !has_cr) || (!chr = ' '))
  then ( decoder.buffer, decoder.pos, !idx + 1 - decoder.pos )
  else leave_with decoder Expected_eol_or_space
