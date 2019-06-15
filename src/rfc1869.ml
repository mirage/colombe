type verb = string
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

module Make (Functor : Sigs.FUNCTOR) = struct
  type t = ..

  module type S = sig
    type x
    type t += T of x
  end

  type 'a extension = (module S with type x = 'a)
  type instance = V : 'a * 'a Functor.t  * ('a -> t) -> instance

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

module Witness = struct
  type 'a t =
    | As_client : description * (module O with type t = 'o) -> 'o t
    | As_server : description * (module I with type t = 'i) -> 'i t
end

include Make(Witness)

type 'a w = 'a Witness.t =
  | As_client : description * (module O with type t = 'o) -> 'o w
  | As_server : description * (module I with type t = 'i) -> 'i w
