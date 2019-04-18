open Colombe

type status = Beginning | Established | Connected | Auth1 | Auth2 | Auth3 | Mail | Rcpt | Data | Data_feed | End

type context = {decoder: Reply.Decoder.decoder; encoder: Request.Encoder.encoder; status: status}

let encode request k ctx =
  let rec loop = function
    | Request.Encoder.Ok -> Fmt.pf Fmt.stdout "\x1b[31;1m%a\x1b[0m\n%!" Request.pp request; k ctx
    | Request.Encoder.Write {buffer; off; len; continue} ->
        `Write (buffer, off, len, fun n -> loop (continue n))
    | Request.Encoder.Error _ -> assert false
  in
  loop (Request.Encoder.request request ctx.encoder)

let decode k ctx =
  let rec loop = function
    | Reply.Decoder.Ok reply -> k reply ctx
    | Reply.Decoder.Read {buffer; off; len; continue} ->
      `Read (buffer, off, len, fun n -> loop (continue n))
    | Reply.Decoder.Error _ -> assert false
  in
  loop (Reply.Decoder.response ctx.decoder)

let reply_to_connection_establishment reply _ctx =
  match reply with
  | `PP_220 _ as reply -> Fmt.pf Fmt.stdout "Connection establishment response: \x1b[33;1m%a\x1b[0m\n%!" Reply.pp reply; `End Established
  | `PN_554 _ -> `Error "Connection establishment error: PN_554"
  | reply -> Fmt.pf Fmt.stdout "Invalid reponse: \x1b[31;2m%a\x1b[0m\n%!" Reply.pp reply; assert false

let reply_to_hello reply _ctx =
  match reply with
  | `PP_250 _ as reply -> Fmt.pf Fmt.stdout "Hello response: \x1b[33;1m%a\x1b[0m\n%!" Reply.pp reply; `End Connected
  | `PN_502 _ -> `Error "Hello error: PN_502"
  | `PN_504 _ -> `Error "Hello error: PN_504"
  | `PN_550 _ -> `Error "Hello error: PN_550"
  | reply -> Fmt.pf Fmt.stdout "Invalid reponse: \x1b[31;2m%a\x1b[0m\n%!" Reply.pp reply; assert false

let reply_to_auth r reply _ctx =
  match reply with
  | `Other _ as reply -> Fmt.pf Fmt.stdout "Auth response: \x1b[33;1m%a\x1b[0m\n%!" Reply.pp reply; `End r
  | reply -> Fmt.pf Fmt.stdout "Invalid reponse: \x1b[31;2m%a\x1b[0m\n%!" Reply.pp reply; assert false

let reply_to_mail reply _ctx =
  match reply with
  | `PP_250 _ as reply -> Fmt.pf Fmt.stdout "Mail response: \x1b[33;1m%a\x1b[0m\n%!" Reply.pp reply; `End Mail
  | `TN_451 _ -> `Error "Mail error: PN_451"
  | `TN_452 _ -> `Error "Mail error: PN_452"
  | `TN_455 _ -> `Error "Mail error: PN_455"
  | `PN_503 _ -> `Error "Mail error: PN_503"
  | `PN_550 _ -> `Error "Mail error: PN_550"
  | `PN_552 _ -> `Error "Mail error: PN_552"
  | `PN_553 _ -> `Error "Mail error: PN_553"
  | `PN_555 _ -> `Error "Mail error: PN_555"
  | reply -> Fmt.pf Fmt.stdout "Invalid reponse: \x1b[31;2m%a\x1b[0m\n%!" Reply.pp reply; assert false

let reply_to_rcpt reply _ctx =
  match reply with
  | `PP_250 _ as reply -> Fmt.pf Fmt.stdout "Recipient response: \x1b[33;1m%a\x1b[0m\n%!" Reply.pp reply; `End Rcpt
  | `PP_251 _ as reply -> Fmt.pf Fmt.stdout "Recipient response: \x1b[33;1m%a\x1b[0m\n%!" Reply.pp reply; `End Rcpt
  | `TN_450 _ -> `Error "Recipient error: PN_450"
  | `TN_451 _ -> `Error "Recipient error: PN_451"
  | `TN_452 _ -> `Error "Recipient error: PN_452"
  | `TN_455 _ -> `Error "Recipient error: PN_455"
  | `PN_503 _ -> `Error "Recipient error: PN_503"
  | `PN_550 _ -> `Error "Recipient error: PN_550"
  | `PN_551 _ -> `Error "Recipient error: PN_551"
  | `PN_552 _ -> `Error "Recipient error: PN_552"
  | `PN_553 _ -> `Error "Recipient error: PN_553"
  | `PN_555 _ -> `Error "Recipient error: PN_555"
  | reply -> Fmt.pf Fmt.stdout "Invalid reponse: \x1b[31;2m%a\x1b[0m\n%!" Reply.pp reply; assert false

let reply_to_data reply _ctx =
  match reply with
  | `TP_354 _ as reply -> Fmt.pf Fmt.stdout "Data response: \x1b[33;1m%a\x1b[0m\n%!" Reply.pp reply; `End Data
  | `PN_503 _ -> `Error "Data error: PN_503"
  | `PN_554 _ -> `Error "Data error: PN_554"
  | reply -> Fmt.pf Fmt.stdout "Invalid reponse: \x1b[31;2m%a\x1b[0m\n%!" Reply.pp reply; assert false

let reply_to_data_feed reply _ctx =
  match reply with
  | `PP_250 _ as reply -> Fmt.pf Fmt.stdout "Data_feed response: \x1b[33;1m%a\x1b[0m\n%!" Reply.pp reply; `End Data_feed
  | `TN_450 _ -> `Error "Data_feed error: TN_450"
  | `TN_451 _ -> `Error "Data_feed error: TN_451"
  | `TN_452 _ -> `Error "Data_feed error: TN_452"
  | `PN_550 _ -> `Error "Data_feed error: PN_550"
  | `PN_552 _ -> `Error "Data_feed error: PN_552"
  | `PN_554 _ -> `Error "Data_feed error: PN_554"
  | reply -> Fmt.pf Fmt.stdout "Invalid reponse: \x1b[31;2m%a\x1b[0m\n%!" Reply.pp reply; assert false

let reply_to_quit reply _ctx =
  match reply with
  | `PP_221 _ as reply -> Fmt.pf Fmt.stdout "Quit response: \x1b[33;1m%a\x1b[0m\n%!" Reply.pp reply; `End End
  | reply -> Fmt.pf Fmt.stdout "Invalid reponse: \x1b[31;2m%a\x1b[0m\n%!" Reply.pp reply; assert false

let write_line str cont =
  let len = String.length str in
  let b = Bytes.create (len + 2) in
  Bytes.blit_string str 0 b 0 len;
  Bytes.blit_string "\r\n" 0 b len 2;
  `Write (b, 0, len + 2, cont)

let run context = function
  | `Establishment ->
    (decode reply_to_connection_establishment)
    context
  | `Hello _ as hello ->
    encode (hello)
      (decode reply_to_hello)
    context
  | `Auth1 ->
    Fmt.pf Fmt.stdout "\x1b[36mAuth\x1b[0m\n%!" ;
    write_line "AUTH LOGIN" (fun _ -> decode (reply_to_auth Auth1) context)
  | `Auth2 login ->
    write_line login (fun _ -> decode (reply_to_auth Auth2) context)
  | `Auth3 password ->
    write_line password (fun _ -> decode (reply_to_auth Auth3) context)
  | `Mail _ as mail ->
    encode (mail)
      (decode reply_to_mail)
    context
  | `Recipient _ as rcpt ->
    encode (rcpt)
      (decode reply_to_rcpt)
    context
  | `Data ->
    encode (`Data)
      (decode reply_to_data)
    context
  | `Data_feed l ->
    let str = String.concat "\r\n" l in
    `Write (Bytes.of_string str, 0, String.length str, (fun _ -> decode (reply_to_data_feed) context))
  | `Quit ->
    encode (`Quit)
      (decode reply_to_quit)
    context
