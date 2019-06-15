type verb = private string
type txts = int * string list

module type I = sig
  type t

  val decode : verb -> string option -> t -> t
  val encode : t -> txts
  val mail_from : t -> Reverse_path.t * (string * string option) list -> t
  val rcpt_to : t -> Forward_path.t * (string * string option) list -> t
end

module type O = sig
  type t

  val encode : t -> verb * string option
  val decode : txts -> t -> t
  val mail_from : t -> Reverse_path.t -> (string * string option) list
  val rcpt_to : t -> Forward_path.t -> (string * string option) list
end

type description =
  { name : string
  ; elho : string
  ; verb : verb list }

type 'a w =
  | As_client : description * (module O with type t = 'o) -> 'o w
  | As_server : description * (module I with type t = 'i) -> 'i w

type t = private ..

module type S = sig
  type x
  type t += T of x
end

type 'a extension = (module S with type x = 'a)
type instance = V : 'a * 'a w * ('a -> t) -> instance

module Injection (X : sig type t val instance : t w end) : S with type x = X.t

val inj : 'a w -> 'a extension
val prj : t -> instance
