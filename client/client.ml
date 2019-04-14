open Lwt
open Colombe
open Protocol

let context = {decoder= Reply.Decoder.decoder (); encoder= Request.Encoder.encoder (); status= Beginning}

let main ~sender ~recipient ~data ic oc =
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
      | Auth3 -> go (run context (`Mail sender))
      | Mail -> go (run context (`Recipient recipient))
      | Rcpt -> go (run context `Data)
      | Data -> go (run context (`Data_feed data))
      | Data_feed -> go (run context `Quit)
      | End -> Lwt.return_unit
  in
    go (run context `Establishment)

let tls_wrap server =
  Fmt.pf Fmt.stdout "Initializing TLS connection\n%!";
  X509_lwt.authenticator `No_authentication_I'M_STUPID
  >>= fun authenticator ->
  let config = Tls.Config.(client ~authenticator ~ciphers:Ciphers.supported ()) in
  Tls_lwt.connect_ext config server

(** Send an email
    [from] and [to_] must be enclosed in angle brackets, eg. ["<foo@bar.net>"] *)
let send_mail
    ~server
    ~from ~to_
    ?(headers=[])
    ?(content="text/html")
    subject body =
  let sender = Reverse_path.Parser.of_string from in
  let recipient = Forward_path.Parser.of_string to_ in
  let data =
    let f = Printf.sprintf in
    [
      f "From: %s" from;
      f "To: %s" to_;
      f "Subject: %s" subject;
      f "Content-type: %s" content;
    ]
    @ headers
    @ [ body; "."; "" ]
  in
  tls_wrap server
  >>= fun (ic, oc) ->
  main ~sender ~recipient ~data ic oc
