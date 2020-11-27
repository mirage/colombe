### v0.4.0 2020-27-11 Paris (France)

- Be resilient when we parse a replies (#27, @dinosaure, review by @mikonieminen)
- **breaking changes**
  Rename `sendmail.tls` to `sendmail.starttls`
  Rename `Sendmail_with_tls` to `Sendmail_with_starttls`
  (#28, @dinosaure, issue #25)
- Handle dot special character when we transmit the mail
  (#30, @dinosaure, review by @mikonieminen, @jerben and @Julow, issue #29)

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
- Relax SMTP parser about End-Of-Line character (be compatible with `gnutls-cli`)
- Add tests
- Clean the distribution

### v0.1.0 2019-07-30 Бар (Црна Гора / Crna Gora)

- First release
