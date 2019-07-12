module Client : Colombe.Rfc1869.CLIENT
module Extension : Colombe.Rfc1869.S with type x = Client.t

val description : Colombe.Rfc1869.description
val extension : Client.t Colombe.Rfc1869.extension
val inj : Client.t -> Colombe.Rfc1869.t

type authenticator = Client.t
type mechanism

val pp_mechanism : mechanism Fmt.t
val plain : mechanism
val make : ?mechanism:mechanism -> username:string -> string -> authenticator

val is_authenticated : authenticator -> bool

