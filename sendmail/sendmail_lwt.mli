type error = Sendmail.error

val sendmail :
  hostname:'a Domain_name.t ->
  ?port:int ->
  domain:Colombe.Domain.t ->
  authenticator:X509.Authenticator.t ->
  ?authentication:Sendmail.authentication ->
  Colombe.Reverse_path.t ->
  Colombe.Forward_path.t list ->
  (unit -> (string * int * int) option Lwt.t) ->
  (unit, error) result Lwt.t
(** [sendmail ~hostname ?port ~domain ~authenticator ?authentication sender
   recipients mail]
    where:

    - [hostname] is the hostname of the peer
    - [port] the port of the SMTP peer
    - [domain] is the domain of the sender (probably [localhost])
    - [authenticator] is the TLS authenticator
    - [authentication] is the username and the password of the user
    - [sender] is the sender
    - [recipients] are recipients of the email
    - [mail] stream of the mail

    The connection already start a TLS connection to the peer. The peer is
    probably available on [*:465] (the default of [port] argument). The [mail]
    stream {b must} emit for each chunk a CRLF at the end (a line). As an user
    of GMail, the call of [sendmail] looks like:

    {[
      open Mrmime

      let my_domain = Colombe.Domain.of_string_exn (Unix.gethostname ())

      let my_authentication =
        {
          Sendmail.username = "my_login";
          Sendmail.password = "my_password";
          Sendmail.mechanism = Sendmail.PLAIN;
        }

      let sender =
        let open Mrmime.Mailbox in
        let v =
          Local.[ "my"; "address"; "mail" ] @ Domain.(domain, [ "gmail"; "com" ])
        in
        Result.get_ok (Colombe_emile.to_reverse_path v)
      (* "my.address.mail@gmail.com" *)

      let destination =
        let open Mrmime.Mailbox in
        let v = Local.[ "to"; "joe" ] @ Domain.(domain, [ "gmail"; "com" ]) in
        Result.get_ok (Colombe_emile.to_forward_path v)
      (* "to.joe@gmail.com" *)

      let run () =
        sendmail
          ~hostname:Domain_name.(host_exe (of_string_exn "gmail.com"))
          ~domain:my_domain ~authentictor ~authentication sender [ destination ]
          mail

      let () =
        match Lwt_main.run (run ()) with
        | Ok () -> ()
        | Error err -> Format.eprintf "%a.\n%!" Sendmail.pp_error err
    ]}

    [sendmail] does not strictly depend on [mrmime] or [emile]. However, we
    advise to use them to produce typed and well-formed mails. [sendmail] does
    not handle properly contents of mails as are. It just emits the stream to
    the pipeline directly without any changes if the line does not start with a
    dot (["."]). Otherwise, it prepends the line with a new dot (which has a
    signification in terms of SMTP).

    We assume that each call of [mail ()] gives to us a line - something which
    ends up with CRLF (["\r\n"]). By this way, we can {i sanitize} the dot
    character - and only on this way.

    [mrmime] ensures to make on its way a stream which emits line per line. A
    non-user of [mrmime] should be aware about this assumption.

    [sendmail] starts by itself a TLS connection with the SMTP server. *)

val sendmail_with_starttls :
  hostname:'a Domain_name.t ->
  ?port:int ->
  domain:Colombe.Domain.t ->
  authenticator:X509.Authenticator.t ->
  ?authentication:Sendmail.authentication ->
  Colombe.Reverse_path.t ->
  Colombe.Forward_path.t list ->
  (unit -> (string * int * int) option Lwt.t) ->
  (unit, Sendmail_with_starttls.error) result Lwt.t
(** [sendmail_with_starttls] is {!sendmail} but a part of the communication is
    insecure. Usually, a SMTP service provides 2 submission services:

    - An implicitely secured one on [*:465].
    - An explicitely secured one (with [STARTTLS]) on [*:587].

    The user should use the first one but in the context of the non-existence of
    it, the second one is available. Usage and arguments are the same. However,
    default value of [port] is the default value of your operating system (see
    {!Unix.getprotobyname}). *)
