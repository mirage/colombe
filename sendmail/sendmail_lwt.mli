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
(** [sendmail ~hostname ?port ~domain ~authenticator ?authentication sender recipients mail] where:

    {ul
    {- [hostname] is the hostname of the peer}
    {- [port] the port of the SMTP peer}
    {- [domain] is the domain of the sender (probably [localhost])}
    {- [authenticator] is the TLS authenticator}
    {- [authentication] is the username and the password of the user}
    {- [sender] is the sender}
    {- [recipients] are recipients of the email}
    {- [mail] stream of the mail}}

    The connection already start a TLS connection to the peer.
    The peer is probably available on [*:465]. The mail must emit for each chunk a CRLF at the end. *)
