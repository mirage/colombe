### v0.10.0 2024-10-15 Paris (France)

- Be able to compose errors from sendmail packages (@dinosaure, #76)
- Add the new package `sendmail-mirage` (@dinosaure, #77)
- **breaking-change** Improve the `sendmail` library.

  The sendmail library is able to send an email with or without STARTTLS. If
  the user gives an authentication method (with a password), we require STARTTLS
  in anyway. Otherwise, we return the `Encryption_required` error. By this way,
  the `sendmail` package does not leak such information.

  We also separate two kind of use about `sendmail`:
  - the submission of an email to an authority
  - how send an email to its destination

  The second is the most basic (and probably what you want). The first is useful
  when you want to pass through an authority (such as gmail.com or your own
  mail exchange server) to send an email to a destination.

### v0.9.0 2024-09-18 Paris (France)

- Upgrade to `tls.1.0.0` (#74, @dinosaure, @hannesm)

### v0.8.1 2024-04-22 Paris (France)

- Upgrade to `tls.0.17.4` (#71, @dinosaure)
- Upgrade to `ocamlformat.0.26.1` (#72, @dinosaure)

### v0.8.0 2023-02-20 Paris (France)

- Provide a basic line decoder (#66, @dinosaure)
- Upgrade `sendmail-lwt` to `tls.0.16.0` (#67, @dinosaure)

### v0.7.0 2022-11-30 Paris (France)

- Implement the `LOGIN` mechanism when we want to send an email (@dinosaure,
  issued by @aronerben & @mabiede, #60, #61)
- Update the codebase with `ocamlformat` (@dinosaure, #62, #64)

### v0.6.0 2022-01-03 Paris (France)

- Better implementation of `STARTTLS` (@dinosaure, #50)
- Properly quit if the server does not implement `STARTTLS` (@dinosaure, #51)
- Add `let+` operator which manipulate `result` type (@dinosaure, #52)
- Upgrade `fmt.0.8.9` (@dinosaure, #53)
- Be able to pre-allocate resources when we want to send an email (@dinosaure,
  #54)

### v0.5.0 2021-08-30 Paris (France)

- Use `Cstruct.length` instead of `Cstruct.len` (@dinosaure, #45)
- Let the user to emit the end of the stream (spotted by @jsthomas, @dinosaure,
  review @mikonieminen, #47)

### v0.4.2 2021-07-26 Paris (France)

- Add `Path.of_string_exn` (@dinosaure, #40)
- Be resilient about 334 argument and add regression test (@dinosaure,
  @jsthomas, #41)

### v0.4.1 2021-04-27 Paris (France)

- Upgrade to `tls.0.13.0` (#34, @dinosaure)

### v0.4.0 2020-11-29 Paris (France)

- Be resilient when we parse a replies (mirage/colombe#27, @dinosaure, review by
  @mikonieminen)
- **breaking changes**
  Rename `sendmail.tls` to `sendmail.starttls`
  Rename `Sendmail_with_tls` to `Sendmail_with_starttls`
  (mirage/colombe#28, @dinosaure, issue mirage/colombe#25)
- Handle dot special character when we transmit the mail
  (mirage/colombe#30, @dinosaure, review by @mikonieminen, @jerben and @Julow,
  issue mirage/colombe#29)

  `sendmail` already expects a stream which emits line per line the mail
  but it sanitizes now the dot character according the SMTP protocol. If
  the user uses `mrmime` to generate the mail, he should upgrade it to
  `mrmime.0.3.2`. Otherwise, it must respect this assumption.        

### v0.3.0 2020-05-17 Paris (France)

- Fix opam file (#22, @kit-ty-kate)
- Better documentation (#23 & #24, @dinosaure)
- Update to `angstrom.0.14.0` (#24, @dinosaure)

### v0.2.0 2020-03-14 Paris (France)

- Fix warnings from OCaml 4.08.1
- Monadic view about implementation of the state machine 
- Handle `let*` syntax and add dependency with `future_syntax`
- Use at least `dune.1.8.0`
- Use `emile` instead `mrmime` about email address
- Support `8BITMIME`
- Add some logs
- Rename `Parser` to `Decoder`
- Add `Domain.compare`
- Externalize some parsers/decoders
- Add Received encoder/decoder
- Functorize `STARTTLS` implementation
- Close properly a TLS connection
- Relax SMTP parser about End-Of-Line character (be compatible with
  `gnutls-cli`)
- Add tests
- Clean the distribution

### v0.1.0 2019-07-30 Бар (Црна Гора / Crna Gora)

- First release
