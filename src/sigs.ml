module type FUNCTOR = sig type 'a t end

type (+'a, 't) io
type ('l, 'r) either = L of 'l | R of 'r
type 'a or_error = ('a, [ `Msg of string ]) result

type 't impl =
  { bind : 'a 'b. ('a, 't) io -> ('a -> ('b, 't) io) -> ('b, 't) io
  ; return : 'a. 'a -> ('a, 't) io }

type ('flow, 's) rdwr =
  { rd : 'flow -> bytes -> int -> int -> (int, 's) io
  ; wr : 'flow -> string -> int -> int -> (unit, 's) io }

module type X = sig
  type 'a s
  type t

  external inj : 'a s -> ('a, t) io = "%identity"
  external prj : ('a, t) io -> 'a s = "%identity"
end

module Common = struct
  type t

  external inj : 'a -> 'b = "%identity"
  external prj : 'a -> 'b = "%identity"
end

module Make (T : FUNCTOR) = struct
  type 'a s = 'a T.t
  include Common
end
