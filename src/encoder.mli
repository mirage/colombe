type encoder

val io_buffer_size : int
val encoder : unit -> encoder

type error = No_enough_space

val pp_error : error Fmt.t

type state =
  | Write of { buffer : string
             ; off : int
             ; len : int
             ; continue : int -> state }
  | Error of error
  | Ok

val safe : (encoder -> state) -> encoder -> state
val flush : (encoder -> state) -> encoder -> state
val write : string -> encoder -> unit
val blit : buf:string -> off:int -> len:int -> encoder -> unit
