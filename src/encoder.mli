type encoder

val io_buffer_size : int
val encoder : unit -> encoder

val pp : encoder Fmt.t

type error = [ `Not_enough_space ]

val pp_error : error Fmt.t

type 'err state =
  | Write of { buffer : string
             ; off : int
             ; len : int
             ; continue : int -> 'err state }
  | Error of 'err
  | Done

val safe : (encoder -> ([> error ] as 'err) state) -> encoder -> 'err state
val flush : (encoder -> ([> error ] as 'err) state) -> encoder -> 'err state
val write : string -> encoder -> unit
val blit : buf:string -> off:int -> len:int -> encoder -> unit
