module Client : functor (_ : Logs.LOG) -> Colombe.Rfc1869.CLIENT
type on = bool

val description : Colombe.Rfc1869.description
val extension : (module Logs.LOG) -> on Colombe.Rfc1869.extension
