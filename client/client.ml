open Lwt
open Colombe
open Protocol

let context = {decoder= Reply.Decoder.decoder (); encoder= Request.Encoder.encoder (); status= Beginning}

let main ~auth:(login, password) ~sender ~recipient ~data ic oc =
  let rec go = function
    | `Error s -> Fmt.pf Fmt.stdout "Error: %s\n%!" s; Lwt.fail Exit
    | `Read (buf, off, len, continue) -> Lwt_io.(read_line ic) >>= (fun s -> let s = s ^ "\r\n" in Bytes.blit_string s 0 buf off (min (String.length s) len); go (continue (min (String.length s) len)))
    | `Write (buf, off, len, continue) -> Lwt_io.(let s = Bytes.sub_string buf off len in Fmt.pf Fmt.stdout "Sending: \x1b[31m%s\x1b[0m\n" s; write oc s) >>= (fun () -> go (continue len))
    | `End state ->
      match state with
      | Beginning -> Lwt.fail Exit
      | Established -> go (run context (`Hello Domain.(Domain [ "bar"; "com" ])))
      | Connected -> go (run context `Auth1)
      | Auth1 -> go (run context (`Auth2 login))
      | Auth2 -> go (run context (`Auth3 password))
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

let stream_of_list lst =
  let ptr = ref lst in
  fun () ->
    match !ptr with
    | hd :: tl -> ptr := tl; Some hd
    | [] -> None

let stream_concat streams =
  let streams = ref streams in
  let rec next () =
    match !streams with
    | s :: tl ->
        begin match s () with
        | Some _ as r -> r
        | None -> streams := tl; next ()
        end
    | [] -> None
  in
  next

(** Send an email
    [~auth] is the tuple (login, password) base64-encoded, padded.
    [body] is a function [unit -> string option], that is called repeatedly for
    each line of the body until it returns [None]. *)
let send_mail
    ~server
    ~auth
    ~from:(from_name, from_email)
    ~to_:(to_name, to_email)
    ?(headers=[])
    subject body =
  let f = Printf.sprintf in
  let from_email = f "<%s>" from_email
  and to_email = f "<%s>" to_email in
  let sender = Reverse_path.Parser.of_string from_email in
  let recipient = Forward_path.Parser.of_string to_email in
  let data =
    let maybe_name = function Some name -> name ^ " " | None -> "" in
    stream_concat [
      stream_of_list [
        f "From: %s%s" (maybe_name from_name) from_email;
        f "To: %s%s" (maybe_name to_name) to_email;
        f "Subject: %s" subject;
      ];
      stream_of_list headers;
      body;
      stream_of_list [ "."; "" ];
    ]
  in
  tls_wrap server
  >>= fun (ic, oc) ->
  main ~auth ~sender ~recipient ~data ic oc
