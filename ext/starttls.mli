module Client : Colombe.Rfc1869.CLIENT
module Extension : Colombe.Rfc1869.S with type x = Client.t

val description : Colombe.Rfc1869.description
val extension : Client.t Colombe.Rfc1869.extension
val inj : Client.t -> Colombe.Rfc1869.t

type state = Client.t
type 'e error constraint 'e = [> Rresult.R.msg ]
type fiber = Fiber : 'error error * ('s, 'error) Colombe.State.process -> fiber
type f = private fiber

val fiber : ('s, 'e) Colombe.State.process -> f * 'e error
val make : f -> ?domain:[ `host ] Domain_name.t -> Tls.Config.client -> state
val extract_fiber : state -> f
