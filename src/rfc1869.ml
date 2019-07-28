(* Client part (only!) *)

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

let pp_error : error Fmt.t = fun _ _ -> ()

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

module Make (Functor : Sigs.FUNCTOR) = struct
  type t = ..

  module type S = sig
    type x
    type t += T of x
  end

  type 'a extension = (module S with type x = 'a)
  type 'a ctor = 'a -> t
  type instance = V : 'a * 'a Functor.t  * 'a ctor -> instance

  let handlers = Hashtbl.create 16

  module Injection (X : sig
    type t

    val instance : t Functor.t
  end) : S with type x = X.t = struct
    type x = X.t
    type t += T of x

    let () =
      let instance = X.instance in
      Hashtbl.add handlers
        (Obj.extension_id [%extension_constructor T])
        (function T x -> V (x, instance, (fun x -> T x))
                | _ -> raise Not_found)
  end

  let inj (type a) (f : a Functor.t) : a extension =
    ( module Injection (struct
      type t = a

      let instance = f
    end) )

  let prj (t : t) =
    let rec go = function
      | [] -> assert false (* totality *)
      | x :: r -> ( try x t with Not_found -> go r )
    in
    go
      (Hashtbl.find_all handlers
         (Obj.extension_id (Obj.extension_constructor t)))

  let eq
    : type a. t -> a extension -> a option
    = fun t (module E) -> match t with
      | E.T v -> Some v
      | _ -> None
end

type description =
  { name : string
  ; elho : string
  ; verb : verb list }

module W = struct type 'a t = (module CLIENT with type t = 'a) end
include Make(W)

type 'a client = (module CLIENT with type t = 'a)
