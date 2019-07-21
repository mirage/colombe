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

module Decoder = struct
  open Decoder

  let is_digit = function '0' .. '9' -> true | _ -> false

  external unsafe_get_uint8 : bytes -> int -> int = "%string_unsafe_get"

  let number decoder =
    let raw, off, len = while1 is_digit decoder in
    let idx = ref 0 in
    let res = ref 0 in
    while !idx < len
    do res := (!res * 10) + (unsafe_get_uint8 raw (off + !idx) - 48) ; incr idx done ;
    if len <> 3 then leave_with decoder (Invalid_code !res) ; !res

  let response k decoder =
    let rec go code lines decoder =
      let code' = number decoder in
      if code <> code' then leave_with decoder (Invalid_code code') ;
      match peek_char decoder with
      | Some ' ' ->
        junk_char decoder ;
        let raw_crlf, off, len = peek_while_eol decoder in
        let reply = v code (List.rev (Bytes.sub_string raw_crlf off (len - 2) :: lines)) in
        decoder.pos <- decoder.pos + len ; k reply decoder
      | Some '-' ->
        junk_char decoder ;
        let raw_crlf, off, len= peek_while_eol decoder in
        decoder.pos <- decoder.pos + len ;
        if end_of_input decoder = decoder.pos
        then prompt (go code (Bytes.sub_string raw_crlf off (len - 2) :: lines)) decoder
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
      let reply = v code [ Bytes.sub_string raw_crlf off (len - 2) ] in
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
    let go x : (t, error) result = match x with
      | Read _ -> Error End_of_input
      | Error { error; _ } -> Error error
      | Ok v -> Ok v in
    go (response decoder)

  let of_string_raw x r =
    let decoder = decoder_from x in
    let go x : (t, error) result = match x with
      | Read _ -> Error End_of_input
      | Error { error; _ } -> Error error
      | Ok v -> r := decoder.pos ; Ok v in
    go (response decoder)
end

module Encoder = struct
  open Encoder

  let crlf encoder = write "\r\n" encoder

  let write_number n encoder =
    let number = Fmt.fmt "%03d" in
    write Fmt.(to_to_string number n) encoder

  let response response k encoder =
    match lines response with
    | [] -> Fmt.invalid_arg "Reply.Encoder.response: response can not be empty"
    | [ x ] ->
      write_number (code response) encoder ;
      write " " encoder ;
      write x encoder ;
      crlf encoder ;
      flush k encoder
    | x :: r ->
      let code = code response in
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
        Buffer.add_substring res buffer off len ;
        go (continue len)
      | Error error -> Error error
      | Ok -> Ok (Buffer.contents res) in
    go (response x encoder)
end
