(** {1 Sendmail with [mkernel]/[mnet].}

    This implementation proposes to reuse the logic for sending emails for
    [mkernel]/[mnet].

    It exposes the Miou scheduler so that the actions for injecting and
    projecting values can be reused (see the {!module:Miou_scheduler} module and
    the {!val:miou} value).

    Furthermore, this implementation exposes the implementation of two
    protocols: {!val:tcp} and {!val:tls} (using [mnet] and [mnet.tls]). This
    allows an email to be sent from a TCP/IP connection or from an already
    established TLS connection (i.e. one where the handshake has already taken
    place).

    Finally, like the other derivatives, msendmail offers three ways to send
    emails:
    + the most common method using {!val:sendmail}, which involves initiating
      communication via a TCP/IP connection using [STARTTLS] and simply
      transmitting the email
    + a method which involves initiating, this time, a TLS connection by default
      to {!val:submit} an email to an SMTP submission server (which will likely
      require authentication). Basically, this is when you wish to send an email
      under a specific authority such as Gmail.
    + a way to send {!val:many} emails (like {!val:sendmail}) using a single
      TCP/IP and a [STARTTLS] connection. *)

module Miou_scheduler : sig
  type t

  external prj : ('a, t) Colombe.Sigs.io -> 'a = "%identity"
  external inj : 'a -> ('a, t) Colombe.Sigs.io = "%identity"
end

val miou : Miou_scheduler.t Colombe.Sigs.impl
val tcp : (Mnet.TCP.flow, Miou_scheduler.t) Colombe.Sigs.rdwr
val tls : (Mnet_tls.t, Miou_scheduler.t) Colombe.Sigs.rdwr

val pp_error : [ `Msg of string | Sendmail_with_starttls.error ] Fmt.t
(** Pretty printer of errors. *)

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
