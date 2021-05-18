val of_local : [ `Dot_string of string list | `String of string ] -> Emile.local

val to_path :
  ?route:Colombe.Domain.t list ->
  Emile.mailbox ->
  (Colombe.Path.t, [> `Msg of string ]) result

val to_reverse_path :
  ?route:Colombe.Domain.t list ->
  Emile.mailbox ->
  (Colombe.Reverse_path.t, [> `Msg of string ]) result

val to_forward_path :
  ?route:Colombe.Domain.t list ->
  Emile.mailbox ->
  (Colombe.Forward_path.t, [> `Msg of string ]) result

val to_domain : Emile.domain -> (Colombe.Domain.t, [> `Msg of string ]) result
