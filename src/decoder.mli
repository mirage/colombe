type decoder =
  { buffer : bytes
  ; mutable pos : int
  ; mutable max : int }

val pp : decoder Fmt.t

val io_buffer_size : int
val decoder : unit -> decoder
val decoder_from : string -> decoder
val end_of_input : decoder -> int

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

val pp_error : error Fmt.t

type 'v state =
  | Ok of 'v
  | Read of { buffer : bytes
            ; off : int
            ; len : int
            ; continue : int -> 'v state }
  | Error of info
and info = { error : error
           ; buffer : bytes
           ; committed : int }

val safe : (decoder -> 'v state) -> decoder -> 'v state
val leave_with : decoder -> error -> 'a

val return : 'v -> decoder -> 'v state
val peek_char : decoder -> char option
val string : string -> decoder -> unit
val junk_char : decoder -> unit
val while1 : (char -> bool) -> decoder -> (bytes * int * int)
val at_least_one_line : decoder -> bool
val prompt : (decoder -> 'v state) -> decoder -> 'v state
val peek_while_eol : decoder -> (bytes * int * int)
val peek_while_eol_or_space : decoder -> (bytes * int * int)
