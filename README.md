# Colombe - an implementation of SMTP protocol in OCaml

`colombe` is a little library which wants to implement the [SMTP][smtp] protocol
according RFC5321. It is a low-level library used by some others projects to
implements clients or servers.

The library __does not__ handle properly an email. If you want to generate a
proper email or introspect an email, you should be interested by
[mrmime][mrmime].

## As a client

The distribution provides `sendmail`, `sendmail.tls` and `sendmail-lwt` which
are respectively:
- An agnostic (to the system) implementation of an usual client with
  authentication
- An agnostic (to the system) implementation of an usual client with
  authentication and `STARTTLS` extension
- An [LWT][lwt] implementation of an usual client with authentication

Depending on your context:
- If you want to communicate with a SMTP server which require a TLS connection
  (like, `*:465`), `sendmail` with a scheduler such as [LWT][lwt] or
  [ASYNC][async] (or [UNIX][unix]) should be use. `sendmail-lwt` is a
  _specialization_ with [LWT][lwt].
- If you want to communicate with a SMTP server which handles (and surely
  requires) `STARTTLS` extension, (like `*:587`), `sendmail.tls` which a
  scheduler should be use.
  
Of course, a client can be more complex and can handle:
- file attachments
- a well formed email
- some usual metadata such as the timezone

If you are interested by that, you should look into [facteur][facteur] which
wants to provide a little binary to send an email with a nice use between
`colombe`, `mrmime` and some others MirageOS projects.

## As a server

Of course, `colombe` implements both sides. By this library, you are able to
implement an SMTP server - if you are interested by that, you should look into
[ptt][ptt].

## How to use `colombe`

As we said, the library wants to be a low-level one. At least, the core does not
handle properly eSMTP extensions or any high mechanisms such the authentication.
It's mostly because the library wants to be used by a server implementation
__and__ a client implementation.

It only ensures a high abstracted interface on top of the low-level of the
communication. The library defines only few parts of the SMTP protocol and a
_monadic_ interface to describe the state-machine while the communication with
an other peer.

`sendmail` is a good example about how to use `colombe`. It describes
structurally the SMTP protocol (only what it really needs) and the state machine
to send an email.

## Received field

RFC5321 describes the SMTP protocols and some mechanisms such as the `Received:`
field. The distribution provides a way to introspect `Received:` fields (and can
produce a graph of them) or generate them from a server configuration.

[SMTP]: https://en.wikipedia.org/wiki/Simple_Mail_Transfer_Protocol
[RFC5321]: https://tools.ietf.org/html/rfc5321
[LWT]: https://github.com/ocsigen/lwt
[ASYNC]: https://opensource.janestreet.com/async/
[UNIX]: https://caml.inria.fr/pub/docs/manual-ocaml/libref/Unix.html
[facteur]: https://github.com/dinosaure/facteur
[ptt]: https://github.com/dinosaure/ptt
[mrmime]: https://github.com/mirage/mrmime
