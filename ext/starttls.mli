module Client : Colombe.Rfc1869.CLIENT
module Extension : Colombe.Rfc1869.S with type x = Client.t

val description : Colombe.Rfc1869.description
val extension : Client.t Colombe.Rfc1869.extension
val inj : Client.t -> Colombe.Rfc1869.t

type state = Client.t
type fiber

val fiber : ('s, 'error) Colombe.State.process -> fiber
val make : fiber -> ?domain:[ `host ] Domain_name.t -> Tls.Config.client -> state
