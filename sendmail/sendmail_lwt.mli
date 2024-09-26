(** Sending an email with lwt.

    There are two ways of sending an email:
    - the first is to send an email to a service which often requires
      authentication and allows the email to be sent back under the identity of
      the service. For example, if I wanted to send an email under my
      [foo@gmail.com] email address, I would {val:submit} it to [smtp.gmail.com]
      and authenticate as [foo]. This service will send your email to its real
      destination but under the identity of [gmail.com].
    - the second method is to send an email under the current authority that I
      represent (my machine). This is particularly the case on a local network
      where I want the recipient to recognise me under my own identity without
      going through a third party. This method does not generally require
      authentication.

    In other words, if you want a service to send your email, you should
    {val:submit} it. If you simply want to send an email to a destination, you
    should use {val:sendmail}.

    Both methods can use TLS (directly or via STARTTLS). The submission service
    of an authority such as [gmail.com] offers 2 ports where you can send an
    email: [465] and [587]. The first uses TLS directly and the second uses
    STARTTLS. It is possible, if the [authenticator] is not given not to use
    TLS or STARTTLS but:
    - most services will reply [`Encryption_required]
    - it is not advisable to do this, as all your information will be in clear
      text.

    If [authentication] is specified and we do not use TLS or STARTTLS, we
    {b fail}. In this situation, if we were to continue, your password would be
    transmitted unencrypted over the network.

    For all other specified ports, we use TLS directly (and not STARTTLS) if the
    [authenticator] is specified. The default port for submission is [465].

    Simply sending an email (via {!val:sendmail}) only uses STARTTLS if the
    [authenticator] is specified - again, [authentication] is often not required
    to send an email to such a service. It is advisable to specify an
    [authenticator]. Our implementation will attempt to send the email via
    STARTTLS if it is available. Otherwise we will try to send the email (if
    [authentication] is {b not} specified).
*)

val submit :
  ?encoder:(unit -> bytes) ->
  ?decoder:(unit -> bytes) ->
  ?queue:(unit -> (char, Bigarray.int8_unsigned_elt) Ke.Rke.t) ->
  hostname:'a Domain_name.t ->
  ?port:int ->
  domain:Colombe.Domain.t ->
  ?authenticator:X509.Authenticator.t ->
  ?authentication:Sendmail.authentication ->
  Colombe.Reverse_path.t ->
  Colombe.Forward_path.t list ->
  (unit -> (string * int * int) option Lwt.t) ->
  (unit, [> `Msg of string | Sendmail_with_starttls.error ]) result Lwt.t

val sendmail :
  ?encoder:(unit -> bytes) ->
  ?decoder:(unit -> bytes) ->
  ?queue:(unit -> (char, Bigarray.int8_unsigned_elt) Ke.Rke.t) ->
  hostname:'a Domain_name.t ->
  ?port:int ->
  domain:Colombe.Domain.t ->
  ?authenticator:X509.Authenticator.t ->
  ?authentication:Sendmail.authentication ->
  Colombe.Reverse_path.t ->
  Colombe.Forward_path.t list ->
  (unit -> (string * int * int) option Lwt.t) ->
  (unit, [> `Msg of string | Sendmail_with_starttls.error ]) result Lwt.t
