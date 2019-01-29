open Angstrom

module Reply = struct
  (* Permanent positive response *)
  type pos_completion = [
    | `PP_211 of string list
    | `PP_214 of string list
    | `PP_220 of string list
    | `PP_221 of string list
    | `PP_250 of string list
    | `PP_251 of string list
    | `PP_252 of string list
    ]

  (* Temporary positive response *)
  type pos_intermediate = [
    | `TP_354 of string list
    ]

  (* Temporary negativ response *)
  type transient_neg_completion = [
    | `TN_421 of string list
    | `TN_450 of string list
    | `TN_451 of string list
    | `TN_452 of string list
    | `TN_455 of string list
    ]

  (* Permanent negativ response *)
  type permanent_neg_completion = [
    | `PN_500 of string list
    | `PN_501 of string list
    | `PN_502 of string list
    | `PN_503 of string list
    | `PN_504 of string list
    | `PN_550 of string list
    | `PN_551 of string list
    | `PN_552 of string list
    | `PN_553 of string list
    | `PN_554 of string list
    | `PN_555 of string list
    ]

  type t = [
    | pos_completion
    | pos_intermediate
    | transient_neg_completion
    | permanent_neg_completion
    | `Other of int * string list
    ]

  exception MultilineInvalidCode of int list

  let pp : Format.formatter -> t -> unit =
    fun ppf -> function
      | `PP_211 (texts) -> Fmt.pf ppf "Standard code: '211' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `PP_214 (texts) -> Fmt.pf ppf "Standard code: '214' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `PP_220 (texts) -> Fmt.pf ppf "Standard code: '220' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `PP_221 (texts) -> Fmt.pf ppf "Standard code: '221' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `PP_250 (texts) -> Fmt.pf ppf "Standard code: '250' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `PP_251 (texts) -> Fmt.pf ppf "Standard code: '251' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `PP_252 (texts) -> Fmt.pf ppf "Standard code: '252' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `TP_354 (texts) -> Fmt.pf ppf "Standard code: '354' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `TN_421 (texts) -> Fmt.pf ppf "Standard code: '421' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `TN_450 (texts) -> Fmt.pf ppf "Standard code: '450' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `TN_451 (texts) -> Fmt.pf ppf "Standard code: '451' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `TN_452 (texts) -> Fmt.pf ppf "Standard code: '452' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `TN_455 (texts) -> Fmt.pf ppf "Standard code: '455' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `PN_500 (texts) -> Fmt.pf ppf "Standard code: '500' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `PN_501 (texts) -> Fmt.pf ppf "Standard code: '501' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `PN_502 (texts) -> Fmt.pf ppf "Standard code: '502' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `PN_503 (texts) -> Fmt.pf ppf "Standard code: '503' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `PN_504 (texts) -> Fmt.pf ppf "Standard code: '504' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `PN_550 (texts) -> Fmt.pf ppf "Standard code: '550' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `PN_551 (texts) -> Fmt.pf ppf "Standard code: '551' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `PN_552 (texts) -> Fmt.pf ppf "Standard code: '552' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `PN_553 (texts) -> Fmt.pf ppf "Standard code: '553' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `PN_554 (texts) -> Fmt.pf ppf "Standard code: '554' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `PN_555 (texts) -> Fmt.pf ppf "Standard code: '555' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `Other (code, texts) -> Fmt.pf ppf "Other code: '%d' - [" code; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"

  let compare t1 t2 =
    let cmp_str_list l1 l2 = List.length l1 = List.length l2 && List.for_all (fun (s1, s2) -> String.compare s1 s2 = 0) (List.combine l1 l2) in
    match (t1, t2) with
    | (`PP_211 (texts1), `PP_211 (texts2))
    | (`PP_214 (texts1), `PP_214 (texts2))
    | (`PP_220 (texts1), `PP_220 (texts2))
    | (`PP_221 (texts1), `PP_221 (texts2))
    | (`PP_250 (texts1), `PP_250 (texts2))
    | (`PP_251 (texts1), `PP_251 (texts2))
    | (`PP_252 (texts1), `PP_252 (texts2))
    | (`TP_354 (texts1), `TP_354 (texts2))
    | (`TN_421 (texts1), `TN_421 (texts2))
    | (`TN_450 (texts1), `TN_450 (texts2))
    | (`TN_451 (texts1), `TN_451 (texts2))
    | (`TN_452 (texts1), `TN_452 (texts2))
    | (`TN_455 (texts1), `TN_455 (texts2))
    | (`PN_500 (texts1), `PN_500 (texts2))
    | (`PN_501 (texts1), `PN_501 (texts2))
    | (`PN_502 (texts1), `PN_502 (texts2))
    | (`PN_503 (texts1), `PN_503 (texts2))
    | (`PN_504 (texts1), `PN_504 (texts2))
    | (`PN_550 (texts1), `PN_550 (texts2))
    | (`PN_551 (texts1), `PN_551 (texts2))
    | (`PN_552 (texts1), `PN_552 (texts2))
    | (`PN_553 (texts1), `PN_553 (texts2))
    | (`PN_554 (texts1), `PN_554 (texts2))
    | (`PN_555 (texts1), `PN_555 (texts2)) -> cmp_str_list texts1 texts2
    | (`Other (code1, texts1), `Other (code2, texts2)) when code1 = code2 -> cmp_str_list texts1 texts2
    | _ -> false

  let is_space = function | ' ' | '\t' -> true | _ -> false
  let is_eol = function | '\r' | '\n' -> true | _ -> false
  let is_not_eol c = not (is_eol c)
  let crlf = string "\r\n" <?> "crlf" >>= fun _ -> return ()
  let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

  let v code texts =
    match code with
      | 211 -> `PP_211 (texts)
      | 214 -> `PP_214 (texts)
      | 220 -> `PP_220 (texts)
      | 221 -> `PP_221 (texts)
      | 250 -> `PP_250 (texts) 
      | 251 -> `PP_251 (texts)
      | 252 -> `PP_252 (texts)
      | 354 -> `TP_354 (texts)
      | 421 -> `TN_421 (texts)
      | 450 -> `TN_450 (texts)
      | 451 -> `TN_451 (texts)
      | 452 -> `TN_452 (texts)
      | 455 -> `TN_455 (texts)
      | 500 -> `PN_500 (texts)
      | 501 -> `PN_501 (texts)
      | 502 -> `PN_502 (texts)
      | 503 -> `PN_503 (texts)
      | 504 -> `PN_504 (texts)
      | 550 -> `PN_550 (texts)
      | 551 -> `PN_551 (texts)
      | 552 -> `PN_552 (texts)
      | 553 -> `PN_553 (texts)
      | 554 -> `PN_554 (texts)
      | 555 -> `PN_555 (texts)
      | _ -> `Other (code, texts)

  let parse_text =
    lift (fun text -> text)
      (take_while is_not_eol) <* crlf

  let parse_text_l =
    lift (fun text -> [text])
      (take_while is_not_eol) <* crlf

  let parse_multiline =
    lift2 (fun code text -> (code, text))
      (integer)
      ((string "-") *> parse_text)

  let parse_reply =
    lift3 (fun multilines code text ->
            let (codes, texts) = List.split multilines in
            if (List.for_all (fun nbr -> nbr = code) codes)
            then
              v code (texts @ text)
            else
              raise (MultilineInvalidCode (code::codes)))
      (many parse_multiline)
      (integer)
      ((crlf >>= fun () -> return []) <|> ((skip is_space) *> parse_text_l)) <* (end_of_input)

  let eval (str:string) : t =
    match parse_string parse_reply str with
    | Ok v      -> v
    | Error msg -> failwith msg

  let code : t -> int = function
    | `PP_211 _ -> 211
    | `PP_214 _ -> 214
    | `PP_220 _ -> 220
    | `PP_221 _ -> 221
    | `PP_250 _ -> 250
    | `PP_251 _ -> 251
    | `PP_252 _ -> 252
    | `TP_354 _ -> 354
    | `TN_421 _ -> 421
    | `TN_450 _ -> 450
    | `TN_451 _ -> 451
    | `TN_452 _ -> 452
    | `TN_455 _ -> 455
    | `PN_500 _ -> 500
    | `PN_501 _ -> 501
    | `PN_502 _ -> 502
    | `PN_503 _ -> 503
    | `PN_504 _ -> 504
    | `PN_550 _ -> 550
    | `PN_551 _ -> 551
    | `PN_552 _ -> 552
    | `PN_553 _ -> 553
    | `PN_554 _ -> 554
    | `PN_555 _ -> 555
    | `Other (code, _) -> code

  let texts : t -> string list = function
    | `PP_211 (texts) -> texts
    | `PP_214 (texts) -> texts
    | `PP_220 (texts) -> texts
    | `PP_221 (texts) -> texts
    | `PP_250 (texts) -> texts
    | `PP_251 (texts) -> texts
    | `PP_252 (texts) -> texts
    | `TP_354 (texts) -> texts
    | `TN_421 (texts) -> texts
    | `TN_450 (texts) -> texts
    | `TN_451 (texts) -> texts
    | `TN_452 (texts) -> texts
    | `TN_455 (texts) -> texts
    | `PN_500 (texts) -> texts
    | `PN_501 (texts) -> texts
    | `PN_502 (texts) -> texts
    | `PN_503 (texts) -> texts
    | `PN_504 (texts) -> texts
    | `PN_550 (texts) -> texts
    | `PN_551 (texts) -> texts
    | `PN_552 (texts) -> texts
    | `PN_553 (texts) -> texts
    | `PN_554 (texts) -> texts
    | `PN_555 (texts) -> texts
    | `Other (_, texts) -> texts
end

module Decoder = struct
  [@@@warning "-32-34"]

  type decoder =
    { buffer : Bytes.t
    ; mutable pos : int
    ; mutable max : int }

  type error =
    | End_of_input
    | Expected_char of char
    | Unexpected_char of char
    | Expected_string of string
    | Expected_eol
    | No_enough_space
    | Invalid_code of int
    | Assert_predicate of (char -> bool)

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
    (* XXX(dinosaure): in [agnstrom] world, [peek_char] should try to read input
       again. However, SMTP is a line-directed protocol where we can ensure to
       have the full line at the top (with a queue) instead to have a
       systematic check (which slow-down the process). *)

  let leave_with (decoder : decoder) error =
    raise (Leave { error; buffer= decoder.buffer; committed= decoder.pos; })

  let junk_char decoder =
    if decoder.pos < end_of_input decoder
    then decoder.pos <- decoder.pos + 1
    else leave_with decoder End_of_input

  let char chr decoder =
    match peek_char decoder with
    | Some chr' ->
      if not (Char.equal chr chr') then leave_with decoder (Expected_char chr) ;
      junk_char decoder
    | None -> leave_with decoder End_of_input

  let satisfy predicate decoder =
    match peek_char decoder with
    | Some chr ->
      if not (predicate chr) then leave_with decoder (Assert_predicate predicate) ;
      junk_char decoder
    | None -> leave_with decoder End_of_input

  let space = fun decoder -> char ' ' decoder
  let null = fun decoder -> char '\000' decoder

  type sub = bytes * int * int

  let while1 predicate decoder =
    let idx = ref decoder.pos in
    while !idx < end_of_input decoder
          && predicate (Bytes.unsafe_get decoder.buffer !idx)
    do incr idx done ;
    if !idx - decoder.pos = 0
    then leave_with decoder (Assert_predicate predicate) ;
    let sub = decoder.buffer, decoder.pos, decoder.pos - !idx in
    (* XXX(dinosaure): avoid sub-string operation. *)
    decoder.pos <- !idx ; sub

  let while0 predicate decoder =
    let idx = ref decoder.pos in
    while !idx < end_of_input decoder
          && predicate (Bytes.unsafe_get decoder.buffer !idx)
    do incr idx done ;
    let sub = decoder.buffer, decoder.pos, decoder.pos - !idx in
    decoder.pos <- !idx ; sub

  let string str decoder =
    let idx = ref 0 in
    let len = String.length str in
    while decoder.pos + !idx < end_of_input decoder
          && !idx < len
          && Char.equal
            (Bytes.unsafe_get decoder.buffer (decoder.pos + !idx))
            (String.unsafe_get str !idx)
    do incr idx done ;
    if !idx = len then str else leave_with decoder (Expected_string str)

  let crlf = fun decoder -> string "\r\n" decoder
  let is_digit = function '0' .. '9' -> true | _ -> false

  external unsafe_get_uint8 : bytes -> int -> int = "%string_unsafe_get"

  let number decoder =
    let raw, off, len = while1 is_digit decoder in
    let idx = ref 0 in
    let res = ref 0 in
    while !idx < len
    do res := (!res * 10) + (unsafe_get_uint8 raw (off + !idx) - 48) ; incr idx done ;
    !res

  let dash = fun decoder -> char '-' decoder

  let take_while_eol decoder =
    let idx = ref decoder.pos in
    let has_cr = ref false in
    while !idx < end_of_input decoder
          && not (Char.equal '\n' (Bytes.unsafe_get decoder.buffer !idx) && !has_cr)
    do has_cr := Char.equal '\r' (Bytes.unsafe_get decoder.buffer !idx) ; incr idx done ;
    if !idx < end_of_input decoder
    && Char.equal '\n' (Bytes.unsafe_get decoder.buffer !idx)
    && !has_cr
    then decoder.buffer, decoder.pos, !idx - decoder.pos
    else leave_with decoder Expected_eol

  let at_least_one_line decoder =
    let pos = ref decoder.pos in
    let has_cr = ref false in
    while !pos < decoder.max
          && not (Char.equal '\n' (Bytes.unsafe_get decoder.buffer !pos) && !has_cr)
    do has_cr := Char.equal '\r' (Bytes.unsafe_get decoder.buffer !pos) ; incr pos done ;
    (!pos < decoder.max
     && Char.equal '\n' (Bytes.unsafe_get decoder.buffer !pos)
     && !has_cr)

  (* XXX(dinosaure): [prompt] expects at least, one line. So we have a /loop/
     which fills [decoder.buffer] while we did not have CRLF inside
     [decoder.buffer].

     Of course, we have a limit: [Bytes.length decoder.buffer]. *)
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
      else if not (at_least_one_line decoder)
      then Read { buffer= decoder.buffer
                ; off
                ; len= Bytes.length decoder.buffer - off
                ; continue= (fun len -> go (off + len)) }
      else
        ( decoder.max <- off ;
          safe k decoder ) in
    go decoder.max

  let response k decoder =
    let rec go code lines decoder =
      let code' = number decoder in
      if code <> code' then leave_with decoder (Invalid_code code') ;
      match peek_char decoder with
      | Some ' ' ->
        junk_char decoder ;
        let data = take_while_eol decoder in
        let reply = Reply.v code (List.rev (data :: lines)) in
        k reply decoder
      | Some '-' ->
        junk_char decoder ;
        let data = take_while_eol decoder in
        if end_of_input decoder = decoder.pos
        then prompt (go code lines) decoder
        else go code (data :: lines) decoder
      | Some chr ->
        leave_with decoder (Unexpected_char chr)
      | None ->
        leave_with decoder End_of_input in
    let code = number decoder in
    match peek_char decoder with
    | Some ' ' ->
      junk_char decoder ;
      let data = take_while_eol decoder in
      let reply = Reply.v code [ data ] in
      k reply decoder
    | Some '-' ->
      junk_char decoder ;
      let data = take_while_eol decoder in
      if end_of_input decoder = decoder.pos
      then prompt (go code [ data ]) decoder
      else go code [ data ] decoder
    | Some chr ->
      leave_with decoder (Unexpected_char chr)
    | None ->
      leave_with decoder End_of_input
end
