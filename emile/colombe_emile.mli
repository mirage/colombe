val of_local : [ `Dot_string of string list | `String of string ] -> Emile.local
val of_domain : Colombe.Domain.t -> Emile.domain
val of_path : Colombe.Path.t -> Emile.mailbox
val of_forward_path : Colombe.Forward_path.t -> Emile.mailbox option
val of_reverse_path : Colombe.Reverse_path.t -> Emile.mailbox option
val to_local : Emile.local -> [ `Dot_string of string list | `String of string ]
val to_domain : Emile.domain -> Colombe.Domain.t
val to_path : ?route:Colombe.Domain.t list -> Emile.mailbox -> Colombe.Path.t

val to_reverse_path :
  ?route:Colombe.Domain.t list -> Emile.mailbox -> Colombe.Reverse_path.t

val to_forward_path :
  ?route:Colombe.Domain.t list -> Emile.mailbox -> Colombe.Forward_path.t
