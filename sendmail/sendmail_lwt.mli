(** Sending an email with lwt.

    This module lets you send emails directly ({!val:sendmail}) or via an
    authority ({!val:submit}).

    The first function is the simplest (and recommended), sending an email
    directly to a destination on port [25] (if port is not specified). By
    default, we always try to communicate with [STARTTLS] if the service allows
    it. The user can specify a TLS configuration or a specific
    [X509.Authenticator.t] (in particular to accept self-signed certificates).
    The user can also specify [authentication], which is generally {b not}
    required for this type of SMTP service. Note that the implementation will
    never attempt authentication if [STARTTLS] is not available - and will fail.

    The second function allows you to {!val:submit} an email to an authority so
    that the authority can send your email to its destinations. By default, we
    use port [465] on which we directly use TLS (and not [STARTTLS]) to encrypt
    all communications between you and the service. You can specify port [587]
    (if the service on [465] is not available) which this time will use
    [STARTTLS] to encrypt the communication. This service often requires
    authentication (so that you can prove that you can use this authority). Like
    {!val:sendmail}, if authentication is required without the communication
    being encrypted, we will fail.

    The second function allows you to send an email under an identity other than
    your own. In fact, when you want to send an email using the identity
    [foo@gmail.com], you need to authenticate yourself and send the email to
    [smtp.gmail.com] so that this service can in turn send your email to the
    destinations (adding several pieces of information such as a DKIM
    signature). *)

type destination =
  [ `Ipaddr of Ipaddr.t | `Domain_name of [ `host ] Domain_name.t ]

val sendmail :
  ?encoder:(unit -> bytes) ->
  ?decoder:(unit -> bytes) ->
  ?queue:(unit -> (char, Bigarray.int8_unsigned_elt) Ke.Rke.t) ->
  destination:destination ->
  ?port:int ->
  domain:Colombe.Domain.t ->
  ?cfg:Tls.Config.client ->
  ?authenticator:X509.Authenticator.t ->
  ?authentication:Sendmail.authentication ->
  Colombe.Reverse_path.t ->
  Colombe.Forward_path.t list ->
  (unit -> (string * int * int) option Lwt.t) ->
  (unit, [> `Msg of string | Sendmail_with_starttls.error ]) result Lwt.t

val submit :
  ?encoder:(unit -> bytes) ->
  ?decoder:(unit -> bytes) ->
  ?queue:(unit -> (char, Bigarray.int8_unsigned_elt) Ke.Rke.t) ->
  destination:destination ->
  ?port:int ->
  domain:Colombe.Domain.t ->
  ?cfg:Tls.Config.client ->
  ?authenticator:X509.Authenticator.t ->
  ?authentication:Sendmail.authentication ->
  Colombe.Reverse_path.t ->
  Colombe.Forward_path.t list ->
  (unit -> (string * int * int) option Lwt.t) ->
  (unit, [> `Msg of string | Sendmail_with_starttls.error ]) result Lwt.t
