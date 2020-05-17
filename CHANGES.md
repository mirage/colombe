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
