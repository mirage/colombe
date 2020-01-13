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
  [ `End_of_input
  | `Expected_char of char
  | `Unexpected_char of char
  | `Expected_string of string
  (* | Invalid_command of string *)
  | `Expected_eol
  | `Expected_eol_or_space
  | `Not_enough_space
  | `Assert_predicate of (char -> bool) (* TODO: (char -> bool) to char list *) ]
  (* | Invalid_code of int *)

val pp_error : error Fmt.t

type 'err info =
  { error : 'err
  ; buffer : bytes
  ; committed : int }

type ('v, 'err) state =
  | Done of 'v
  | Read of { buffer : bytes
            ; off : int
            ; len : int
            ; continue : int -> ('v, 'err) state }
  | Error of 'err info

val safe : (decoder -> ('v, [> error ] as 'err) state) -> decoder -> ('v, 'err) state
val leave_with : decoder -> error -> 'a
val fail : decoder -> ([> error ] as 'err) -> ('v, 'err) state

val return : 'v -> decoder -> ('v, 'err) state
val peek_char : decoder -> char option
val string : string -> decoder -> unit
val junk_char : decoder -> unit
val while1 : (char -> bool) -> decoder -> (bytes * int * int)
val at_least_one_line : ?relax:bool -> decoder -> bool
val prompt : ?relax:bool -> (decoder -> ('v, [> error ] as 'err) state) -> decoder -> ('v, 'err) state
val peek_while_eol : ?relax:bool -> decoder -> (bytes * int * int)
val peek_while_eol_or_space : ?relax:bool -> decoder -> (bytes * int * int)
