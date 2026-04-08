module Miou_scheduler : sig
  type t

  external prj : ('a, t) Colombe.Sigs.io -> 'a = "%identity"
  external inj : 'a -> ('a, t) Colombe.Sigs.io = "%identity"
end

val miou : Miou_scheduler.t Colombe.Sigs.impl
val tcp : (Mnet.TCP.flow, Miou_scheduler.t) Colombe.Sigs.rdwr
val tls : (Mnet_tls.t, Miou_scheduler.t) Colombe.Sigs.rdwr
val pp_error : [ `Msg of string | Sendmail_with_starttls.error ] Fmt.t

val submit :
  ?encoder:(unit -> bytes) ->
  ?decoder:(unit -> bytes) ->
  ?queue:(unit -> (char, Bigarray.int8_unsigned_elt) Ke.Rke.t) ->
  Mnet_happy_eyeballs.t ->
  destination:string ->
  ?port:int ->
  domain:Colombe.Domain.t ->
  ?cfg:Tls.Config.client ->
  ?authenticator:X509.Authenticator.t ->
  ?authentication:Sendmail.authentication ->
  Colombe.Reverse_path.t ->
  Colombe.Forward_path.t list ->
  (string * int * int) Flux.stream ->
  (unit, [ `Msg of string | Sendmail_with_starttls.error ]) result

val sendmail :
  ?encoder:(unit -> bytes) ->
  ?decoder:(unit -> bytes) ->
  ?queue:(unit -> (char, Bigarray.int8_unsigned_elt) Ke.Rke.t) ->
  Mnet_happy_eyeballs.t ->
  destination:[ `Ips of Ipaddr.t list | `Host of [ `host ] Domain_name.t ] ->
  ?port:int ->
  domain:Colombe.Domain.t ->
  ?cfg:Tls.Config.client ->
  ?authenticator:X509.Authenticator.t ->
  ?authentication:Sendmail.authentication ->
  Colombe.Reverse_path.t ->
  Colombe.Forward_path.t list ->
  (string * int * int) Flux.stream ->
  (unit, [ `Msg of string | Sendmail_with_starttls.error ]) result

type tx =
  Colombe.Reverse_path.t
  * Colombe.Forward_path.t
  * (unit, Sendmail_with_starttls.error) result

val many :
  ?encoder:(unit -> bytes) ->
  ?decoder:(unit -> bytes) ->
  ?queue:(unit -> (char, Bigarray.int8_unsigned_elt) Ke.Rke.t) ->
  Mnet_happy_eyeballs.t ->
  destination:[ `Ips of Ipaddr.t list | `Host of [ `host ] Domain_name.t ] ->
  ?port:int ->
  domain:Colombe.Domain.t ->
  ?cfg:Tls.Config.client ->
  ?authenticator:X509.Authenticator.t ->
  ?authentication:Sendmail.authentication ->
  (Colombe.Reverse_path.t * Colombe.Forward_path.t) list ->
  (string * int * int) Flux.stream Seq.t ->
  (tx list, [ `Msg of string | Sendmail_with_starttls.error ]) result
