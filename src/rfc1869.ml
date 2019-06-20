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
end

type description =
  { name : string
  ; elho : string
  ; verb : verb list }

module W0 = struct type 'a t = description * (module CLIENT with type t = 'a) end
include Make(W0)

type 'a client = 'a W0.t
