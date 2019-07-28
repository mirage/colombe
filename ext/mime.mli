module Client : Colombe.Rfc1869.CLIENT

val description : Colombe.Rfc1869.description
val extension : Client.t Colombe.Rfc1869.extension
val inj : Client.t -> Colombe.Rfc1869.t

type encoding = Client.t

val none : encoding
val bit7 : encoding
val bit8 : encoding

