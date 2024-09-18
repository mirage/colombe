type error

val pp_error : error Fmt.t

val run :
  ?logger:(module Logs.LOG) ->
  hostname:'a Domain_name.t ->
  ?port:int ->
  domain:Colombe.Domain.t ->
  authenticator:X509.Authenticator.a ->
  from:Colombe.Reverse_path.t ->
  recipients:Colombe.Forward_path.t list ->
  Auth.authenticator option ->
  (unit -> (bytes * int * int) option) ->
  (unit, error) result Lwt.t
