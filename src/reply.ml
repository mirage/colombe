module Reply = struct
  (* Permanent positive response *)
  type pos_completion =
    [ `PP_211 of string list
    | `PP_214 of string list
    | `PP_220 of string list
    | `PP_221 of string list
    | `PP_250 of string list
    | `PP_251 of string list
    | `PP_252 of string list ]

  (* Temporary positive response *)
  type pos_intermediate =
    [ `TP_354 of string list ]

  (* Temporary negativ response *)
  type transient_neg_completion =
    [ `TN_421 of string list
    | `TN_450 of string list
    | `TN_451 of string list
    | `TN_452 of string list
    | `TN_455 of string list ]

  (* Permanent negativ response *)
  type permanent_neg_completion =
    [ `PN_500 of string list
    | `PN_501 of string list
    | `PN_502 of string list
    | `PN_503 of string list
    | `PN_504 of string list
    | `PN_550 of string list
    | `PN_551 of string list
    | `PN_552 of string list
    | `PN_553 of string list
    | `PN_554 of string list
    | `PN_555 of string list ]

  type t =
    [ pos_completion
    | pos_intermediate
    | transient_neg_completion
    | permanent_neg_completion
    | `Other of int * string list ]

  let equal_values a b =
    try List.for_all2 String.equal a b
    with _ -> false

  let equal a b = match a, b with
    | `PP_211 a, `PP_211 b -> equal_values a b
    | `PP_214 a, `PP_214 b -> equal_values a b
    | `PP_220 a, `PP_220 b -> equal_values a b
    | `PP_221 a, `PP_221 b -> equal_values a b
    | `PP_250 a, `PP_250 b -> equal_values a b
    | `PP_251 a, `PP_251 b -> equal_values a b
    | `PP_252 a, `PP_252 b -> equal_values a b
    | `TP_354 a, `TP_354 b -> equal_values a b
    | `TN_421 a, `TN_421 b -> equal_values a b
    | `TN_450 a, `TN_450 b -> equal_values a b
    | `TN_451 a, `TN_451 b -> equal_values a b
    | `TN_452 a, `TN_452 b -> equal_values a b
    | `TN_455 a, `TN_455 b -> equal_values a b
    | `PN_500 a, `PN_500 b -> equal_values a b
    | `PN_501 a, `PN_501 b -> equal_values a b
    | `PN_502 a, `PN_502 b -> equal_values a b
    | `PN_503 a, `PN_503 b -> equal_values a b
    | `PN_504 a, `PN_504 b -> equal_values a b
    | `PN_550 a, `PN_550 b -> equal_values a b
    | `PN_551 a, `PN_551 b -> equal_values a b
    | `PN_552 a, `PN_552 b -> equal_values a b
    | `PN_553 a, `PN_553 b -> equal_values a b
    | `PN_554 a, `PN_554 b -> equal_values a b
    | `PN_555 a, `PN_555 b -> equal_values a b
    | `Other (code_a, a), `Other (code_b, b) ->
      code_a = code_b && equal_values a b
    | _, _ -> false

  let pp ppf = function
    | `PP_211 lines -> Fmt.pf ppf "(211 @[<hov>%a@])" Fmt.(Dump.list string) lines
    | `PP_214 lines -> Fmt.pf ppf "(214 @[<hov>%a@])" Fmt.(Dump.list string) lines
    | `PP_220 lines -> Fmt.pf ppf "(220 @[<hov>%a@])" Fmt.(Dump.list string) lines
    | `PP_221 lines -> Fmt.pf ppf "(221 @[<hov>%a@])" Fmt.(Dump.list string) lines
    | `PP_250 lines -> Fmt.pf ppf "(250 @[<hov>%a@])" Fmt.(Dump.list string) lines
    | `PP_251 lines -> Fmt.pf ppf "(251 @[<hov>%a@])" Fmt.(Dump.list string) lines
    | `PP_252 lines -> Fmt.pf ppf "(252 @[<hov>%a@])" Fmt.(Dump.list string) lines
    | `TP_354 lines -> Fmt.pf ppf "(354 @[<hov>%a@])" Fmt.(Dump.list string) lines
    | `TN_421 lines -> Fmt.pf ppf "(421 @[<hov>%a@])" Fmt.(Dump.list string) lines
    | `TN_450 lines -> Fmt.pf ppf "(450 @[<hov>%a@])" Fmt.(Dump.list string) lines
    | `TN_451 lines -> Fmt.pf ppf "(451 @[<hov>%a@])" Fmt.(Dump.list string) lines
    | `TN_452 lines -> Fmt.pf ppf "(452 @[<hov>%a@])" Fmt.(Dump.list string) lines
    | `TN_455 lines -> Fmt.pf ppf "(455 @[<hov>%a@])" Fmt.(Dump.list string) lines
    | `PN_500 lines -> Fmt.pf ppf "(500 @[<hov>%a@])" Fmt.(Dump.list string) lines
    | `PN_501 lines -> Fmt.pf ppf "(501 @[<hov>%a@])" Fmt.(Dump.list string) lines
    | `PN_502 lines -> Fmt.pf ppf "(502 @[<hov>%a@])" Fmt.(Dump.list string) lines
    | `PN_503 lines -> Fmt.pf ppf "(503 @[<hov>%a@])" Fmt.(Dump.list string) lines
    | `PN_504 lines -> Fmt.pf ppf "(504 @[<hov>%a@])" Fmt.(Dump.list string) lines
    | `PN_550 lines -> Fmt.pf ppf "(550 @[<hov>%a@])" Fmt.(Dump.list string) lines
    | `PN_551 lines -> Fmt.pf ppf "(551 @[<hov>%a@])" Fmt.(Dump.list string) lines
    | `PN_552 lines -> Fmt.pf ppf "(552 @[<hov>%a@])" Fmt.(Dump.list string) lines
    | `PN_553 lines -> Fmt.pf ppf "(553 @[<hov>%a@])" Fmt.(Dump.list string) lines
    | `PN_554 lines -> Fmt.pf ppf "(554 @[<hov>%a@])" Fmt.(Dump.list string) lines
    | `PN_555 lines -> Fmt.pf ppf "(555 @[<hov>%a@])" Fmt.(Dump.list string) lines
    | `Other (code, lines) -> Fmt.pf ppf "(%03d @[<hov>%a@])" code Fmt.(Dump.list string) lines

  let to_int = function
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

  let code x = to_int x

  let lines = function
    | `PP_211 lines
    | `PP_214 lines
    | `PP_220 lines
    | `PP_221 lines
    | `PP_250 lines
    | `PP_251 lines
    | `PP_252 lines
    | `TP_354 lines
    | `TN_421 lines
    | `TN_450 lines
    | `TN_451 lines
    | `TN_452 lines
    | `TN_455 lines
    | `PN_500 lines
    | `PN_501 lines
    | `PN_502 lines
    | `PN_503 lines
    | `PN_504 lines
    | `PN_550 lines
    | `PN_551 lines
    | `PN_552 lines
    | `PN_553 lines
    | `PN_554 lines
    | `PN_555 lines
    | `Other (_, lines) -> lines

  let compare a b = (to_int a) - (to_int b)

  let v code lines = match code with
    | 211 -> `PP_211 lines
    | 214 -> `PP_214 lines
    | 220 -> `PP_220 lines
    | 221 -> `PP_221 lines
    | 250 -> `PP_250 lines
    | 251 -> `PP_251 lines
    | 252 -> `PP_252 lines
    | 354 -> `TP_354 lines
    | 421 -> `TN_421 lines
    | 450 -> `TN_450 lines
    | 451 -> `TN_451 lines
    | 452 -> `TN_452 lines
    | 455 -> `TN_455 lines
    | 500 -> `PN_500 lines
    | 501 -> `PN_501 lines
    | 502 -> `PN_502 lines
    | 503 -> `PN_503 lines
    | 504 -> `PN_504 lines
    | 550 -> `PN_550 lines
    | 551 -> `PN_551 lines
    | 552 -> `PN_552 lines
    | 553 -> `PN_553 lines
    | 554 -> `PN_554 lines
    | 555 -> `PN_555 lines
    | code -> `Other (code, lines)
end

module Decoder = struct
  type decoder =
    { buffer : Bytes.t
    ; mutable pos : int
    ; mutable max : int }

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
    | Expected_eol
    | No_enough_space
    | Invalid_code of int
    | Assert_predicate of (char -> bool)

  let pp_error ppf = function
    | End_of_input -> Fmt.string ppf "End_of_input"
    | Expected_char chr -> Fmt.pf ppf "(Expected_char %02x)" (Char.code chr)
    | Unexpected_char chr -> Fmt.pf ppf "(Unexpected_char %02x)" (Char.code chr)
    | Expected_string s -> Fmt.pf ppf "(Expected_string %s)" s
    | Expected_eol -> Fmt.string ppf "Expected_eol"
    | No_enough_space -> Fmt.string ppf "No_enough_space"
    | Invalid_code c -> Fmt.pf ppf "(Invalid_code %03d)" c
    | Assert_predicate _ -> Fmt.string ppf "(Assert_predicate #predicate)"

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

  let is_digit = function '0' .. '9' -> true | _ -> false

  external unsafe_get_uint8 : bytes -> int -> int = "%string_unsafe_get"

  let number decoder =
    let raw, off, len = while1 is_digit decoder in
    let idx = ref 0 in
    let res = ref 0 in
    while !idx < len
    do res := (!res * 10) + (unsafe_get_uint8 raw (off + !idx) - 48) ; incr idx done ; !res

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
        let raw_crlf, off, len = peek_while_eol decoder in
        let reply = Reply.v code (List.rev (Bytes.sub_string raw_crlf off (len - 2) :: lines)) in
        decoder.pos <- decoder.pos + len ; k reply decoder
      | Some '-' ->
        junk_char decoder ;
        let raw_crlf, off, len= peek_while_eol decoder in
        decoder.pos <- decoder.pos + len ;
        if end_of_input decoder = decoder.pos
        then prompt (go code lines) decoder
        else go code (Bytes.sub_string raw_crlf off (len - 2) :: lines) decoder
      | Some chr ->
        leave_with decoder (Unexpected_char chr)
      | None ->
        leave_with decoder End_of_input in
    let code = number decoder in
    match peek_char decoder with
    | Some ' ' ->
      junk_char decoder ;
      let raw_crlf, off, len = peek_while_eol decoder in
      let reply = Reply.v code [ Bytes.sub_string raw_crlf off (len - 2) ] in
      decoder.pos <- decoder.pos + len ; k reply decoder
    | Some '-' ->
      junk_char decoder ;
      let raw_crlf, off, len = peek_while_eol decoder in
      decoder.pos <- decoder.pos + len ;
      if end_of_input decoder = decoder.pos
      then prompt (go code [ Bytes.sub_string raw_crlf off (len - 2) ]) decoder
      else go code [ Bytes.sub_string raw_crlf off (len - 2) ] decoder
    | Some chr ->
      leave_with decoder (Unexpected_char chr)
    | None ->
      leave_with decoder End_of_input

  let response decoder =
    let k v decoder = return v decoder in
    if at_least_one_line decoder
    then safe (response k) decoder
    else prompt (response k) decoder

  let of_string x =
    let decoder = decoder_from x in
    let go x : (Reply.t, error) result = match x with
      | Read _ -> Error End_of_input
      | Error { error; _ } -> Error error
      | Ok v -> Ok v in
    go (response decoder)

  let of_string_raw x r =
    let decoder = decoder_from x in
    let go x : (Reply.t, error) result = match x with
      | Read _ -> Error End_of_input
      | Error { error; _ } -> Error error
      | Ok v -> r := decoder.pos ; Ok v in
    go (response decoder)
end

module Encoder = struct
  type encoder =
    { payload : Bytes.t
    ; mutable pos : int }

  type error = No_enough_space

  let pp_error ppf No_enough_space = Fmt.string ppf "No_enough_space"

  type state =
    | Write of { buffer : Bytes.t
               ; off : int
               ; len : int
               ; continue : int -> state }
    | Error of error
    | Ok

  let io_buffer_size = 65536

  let encoder () =
    { payload= Bytes.create io_buffer_size
    ; pos= 0 }

  exception Leave of error

  let leave_with (_ : encoder) error =
    raise (Leave error)

  let safe k encoder : state =
    try k encoder with Leave error -> Error error

  let flush k0 encoder =
    if encoder.pos > 0
    then
      let rec k1 n =
        if n < encoder.pos
        then Write { buffer= encoder.payload
                   ; off= n
                   ; len= encoder.pos - n
                   ; continue= (fun m -> k1 (n + m)) }
        else ( encoder.pos <- 0 ; k0 encoder ) in
      k1 0
    else k0 encoder

  let write s encoder =
    let max = Bytes.length encoder.payload in
    let go j l encoder =
      let rem = max - encoder.pos in
      let len = if l > rem then rem else l in
      Bytes.blit_string s j encoder.payload encoder.pos len ;
      encoder.pos <- encoder.pos + len ;
      if len < l then leave_with encoder No_enough_space in
    (* XXX(dinosaure): should never appear, but avoid continuation allocation. *)
    go 0 (String.length s) encoder

  let crlf encoder = write "\r\n" encoder

  let write_number n encoder =
    write Fmt.(to_to_string int n) encoder

  let response response k encoder =
    match Reply.lines response with
    | [] -> Fmt.invalid_arg "Reply.Encoder.response: response can not be empty"
    | [ x ] ->
      write_number (Reply.code response) encoder ;
      write " " encoder ;
      write x encoder ;
      crlf encoder ;
      flush k encoder
    | x :: r ->
      let code = Reply.code response in
      write_number code encoder ;
      write "-" encoder ;
      write x encoder ;
      crlf encoder ;

      let rec go l k encoder = match l with
        | [] -> assert false (* XXX(dinosaure): impossible case. *)
        | [ x ] ->
          write_number code encoder ;
          write " " encoder ;
          write x encoder ;
          crlf encoder ;
          flush k encoder
        | x :: r ->
          write_number code encoder ;
          write "-" encoder ;
          write x encoder ;
          crlf encoder ;
          flush (safe (go r k)) encoder in
      flush (safe (go r k)) encoder

  let response x encoder =
    let k _ = Ok in
    flush (safe (response x k)) encoder

  let to_string x =
    let encoder = encoder () in
    let res = Buffer.create 16 in
    let rec go x : (string, error) result = match x with
      | Write { buffer; off; len; continue } ->
        Buffer.add_subbytes res buffer off len ;
        go (continue len)
      | Error error -> Error error
      | Ok -> Ok (Buffer.contents res) in
    go (response x encoder)
end
