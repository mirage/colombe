val to_path : ?route:Colombe.Domain.t list -> Mrmime.Mailbox.t -> (Colombe.Path.t, [ `Msg of string ]) result
val to_reverse_path : ?route:Colombe.Domain.t list -> Mrmime.Mailbox.t -> (Colombe.Reverse_path.t, [ `Msg of string ]) result
val to_forward_path : ?route:Colombe.Domain.t list -> Mrmime.Mailbox.t -> (Colombe.Forward_path.t, [ `Msg of string ]) result
