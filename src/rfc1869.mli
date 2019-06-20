type verb = string
type txts = int * string list

module type CLIENT = sig
  type t
  type error

  val ehlo : t -> string -> (t, error) result
  val encode : t -> verb * string option
  val decode : txts -> t -> (t, error) result
  val mail_from : t -> Reverse_path.t -> (string * string option) list
  val rcpt_to : t -> Forward_path.t -> (string * string option) list
end

type description =
  { name : string
  ; elho : string
  ; verb : verb list }

type 'a client = description * (module CLIENT with type t = 'a)
type t = private ..

module type S = sig
  type x
  type t += T of x
end

type 'a extension = (module S with type x = 'a)
type 'a ctor = 'a -> t
type instance = V : 'a * 'a client * 'a ctor -> instance

module Injection (X : sig type t val instance : t client end) : S with type x = X.t

val inj : 'a client -> 'a extension
val prj : t -> instance
