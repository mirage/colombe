type verb = string

type encode =
  | Request of { verb : verb; args : string list }
  | Payload of { buf : Bytes.t; off : int; len : int }

type action =
  | Recv_code of int
  | Send of encode
  | Waiting_payload

type decode =
  | Response of { code : int; txts : string list }
  | Payload of { buf : Bytes.t; off : int; len : int }

type error = ..

val pp_error : error Fmt.t

module type CLIENT = sig
  type t
  type nonrec error = error

  val pp_error : error Fmt.t

  val ehlo : t -> string -> (t, error) result
  val encode : t -> encode
  val action : t -> action option
  val decode : decode -> t -> (t, error) result
  val handle : t -> t
  val mail_from : t -> Reverse_path.t -> (string * string option) list
  val rcpt_to : t -> Forward_path.t -> (string * string option) list
end

type description =
  { name : string
  ; elho : string
  ; verb : verb list }

type 'a client = (module CLIENT with type t = 'a)
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
val eq : t -> 'a extension -> 'a option
