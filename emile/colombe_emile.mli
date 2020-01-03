val to_path : ?route:Colombe.Domain.t list -> Emile.mailbox -> (Colombe.Path.t, [> Rresult.R.msg ]) result
val to_reverse_path : ?route:Colombe.Domain.t list -> Emile.mailbox -> (Colombe.Reverse_path.t, [> Rresult.R.msg ]) result
val to_forward_path : ?route:Colombe.Domain.t list -> Emile.mailbox -> (Colombe.Forward_path.t, [> Rresult.R.msg ]) result
