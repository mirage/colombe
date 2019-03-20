open Lwt
open Colombe
open Protocol

let context = {decoder= Reply.Decoder.decoder (); encoder= Request.Encoder.encoder (); status= Beginning}

let main ic oc =
  let rec go = function
    | `Error s -> Fmt.pf Fmt.stdout "Error: %s\n%!" s; Lwt.fail Exit
    | `Read (buf, off, len, continue) -> Lwt_io.(read_line ic) >>= (fun s -> let s = s ^ "\r\n" in Bytes.blit_string s 0 buf off (min (String.length s) len); go (continue (min (String.length s) len)))
    | `Write (buf, off, len, continue) -> Lwt_io.(let s = Bytes.sub_string buf off len in Fmt.pf Fmt.stdout "Sending: \x1b[31m%s\x1b[0m\n" s; write oc s) >>= (fun () -> go (continue len))
    | `End state ->
      match state with
      | Beginning -> Lwt.fail Exit
      | Established -> go (run context (`Hello Domain.(Domain [ "bar"; "com" ])))
      | Connected -> go (run context `Auth1)
      | Auth1 -> go (run context `Auth2)
      | Auth2 -> go (run context `Auth3)
      | Auth3 -> go (run context (`Mail (Some { Path.local= `String "foo.bar.foo" ; domain= Domain.(Domain [ "laposte"; "net" ]) ; rest= [] }, [])))
      | Mail -> go (run context (`Recipient Forward_path.(Forward_path { Path.local= `String "charles-edouard.lecat" ; domain= Domain.(Domain [ "epitech"; "eu" ]) ; rest= [] }, [])))
      | Rcpt -> go (run context `Data)
      | Data -> go (run context (`Data_feed
                  [ "Return-Path: foo.bar.foo@laposte.net"
                  ; "Date: Fri, 9 Nov 2018 16:27:30 +0100 (CET)"
                  ; "From: foo.bar.foo@laposte.net"
                  ; "To: charles-edouard.lecat@epitech.eu"
                  ; "Subject: Far from the Core, close to the Sky"
                  ; "MIME-Version: 1.0"
                  ; "Content-type: text/plain; charset=us-ascii"
                  ; "Hello OCaml world !"
                  ; "."
                  ; ""]))
      | Data_feed -> go (run context `Quit)
      | End -> Lwt.return_unit
  in
    go (run context `Establishment)

let tls_wrap () =
  let port = 465 in
  let host = "smtp.laposte.net" in
  Fmt.pf Fmt.stdout "Initializing TLS connection\n%!";
  X509_lwt.authenticator `No_authentication_I'M_STUPID >>= fun authenticator ->
  Tls_lwt.connect_ext
    Tls.Config.(client ~authenticator ~ciphers:Ciphers.supported ())
    (host, port) >>= fun (ic, oc) ->
  Fmt.pf Fmt.stdout "Initialized TLS connection\n%!";
  main ic oc

let () =
  Lwt_main.run (tls_wrap ())
