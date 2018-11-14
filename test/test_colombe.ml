open Colombe

let verify_parsing (reply, (code, texts)) =
  let verify_text (reply, text) = assert (String.compare reply text == 0) in
  let parsed = Reply.eval reply in
  let parsed_code = Reply.code parsed in
  let parsed_texts = Reply.texts parsed in
  assert (parsed_code == code); List.iter verify_text (List.combine parsed_texts texts)

let replies =
  [ "220 foo.com Simple Mail Transfer Service Ready\r\n"
  ; "250-foo.com greets bar.com\r\n\
     250-8BITMIME\r\n\
     250-SIZE\r\n\
     250-DSN\r\n\
     250-VRFY\r\n\
     250 HELP\r\n"
  ; "250 Mark Crispin <Admin.MRC@foo.com>\r\n"
  ; "250 OK\r\n"
  ; "250 OK\r\n"
  ; "354 Start mail input; end with <CRLF>.<CRLF>\r\n"
  ; "250 OK\r\n"
  ; "221 foo.com Service closing transmission channel\r\n"
  ; "220 xyz.com Simple Mail Transfer Service Ready\r\n"
  ; "250 xyz.com is on the air\r\n"
  ; "250 OK\r\n"
  ; "250 OK\r\n"
  ; "354 Start mail input; end with <CRLF>.<CRLF>\r\n"
  ; "250 OK\r\n"
  ; "221 foo.com Service closing transmission channel\r\n"
  ; "220 foo.com Simple Mail Transfer Service Ready\r\n"
  ; "250-foo.com greets bar.com\r\n\
     250-8BITMIME\r\n\
     250-SIZE\r\n\
     250-DSN\r\n\
     250 HELP\r\n"
  ; "250 OK\r\n"
  ; "250 OK\r\n"
  ; "354 Start mail input; end with <CRLF>.<CRLF>\r\n"
  ; "250 OK\r\n"
  ; "221 foo.com Service closing transmission channel\r\n"
  ; "220 foo.com Simple Mail Transfer Service Ready\r\n"
  ; "250-foo.com greets bar.com\r\n\
     250-8BITMIME\r\n\
     250-SIZE\r\n\
     250-DSN\r\n\
     250 HELP\r\n"
  ; "250 OK\r\n"
  ; "250 OK\r\n"
  ; "550 No such user here\r\n"
  ; "250 OK\r\n"
  ; "221 foo.com Service closing transmission channel\r\n"
  ; "220 foo.com Simple Mail Transfer Service Ready\r\n"
  ; "250-foo.com greets bar.com\r\n\
     250-8BITMIME\r\n\
     250-SIZE\r\n\
     250-DSN\r\n\
     250 HELP\r\n"
  ; "250 OK\r\n"
  ; "250 OK\r\n"
  ; "550 No such user here\r\n"
  ; "250 OK\r\n"
  ; "354 Start mail input; end with <CRLF>.<CRLF>\r\n"
  ; "250 OK\r\n"
  ; "221 foo.com Service closing transmission channel\r\n" ]

let results =
  [ (220, ["foo.com Simple Mail Transfer Service Ready"])
  ; (250, ["foo.com greets bar.com"; "8BITMIME"; "SIZE"; "DSN"; "VRFY"; "HELP"])
  ; (250, ["Mark Crispin <Admin.MRC@foo.com>"])
  ; (250, ["OK"])
  ; (250, ["OK"])
  ; (354, ["Start mail input; end with <CRLF>.<CRLF>"])
  ; (250, ["OK"])
  ; (221, ["foo.com Service closing transmission channel"])
  ; (220, ["xyz.com Simple Mail Transfer Service Ready"])
  ; (250, ["xyz.com is on the air"])
  ; (250, ["OK"])
  ; (250, ["OK"])
  ; (354, ["Start mail input; end with <CRLF>.<CRLF>"])
  ; (250, ["OK"])
  ; (221, ["foo.com Service closing transmission channel"])
  ; (220, ["foo.com Simple Mail Transfer Service Ready"])
  ; (250, ["foo.com greets bar.com"; "8BITMIME"; "SIZE"; "DSN"; "HELP"])
  ; (250, ["OK"])
  ; (250, ["OK"])
  ; (354, ["Start mail input; end with <CRLF>.<CRLF>"])
  ; (250, ["OK"])
  ; (221, ["foo.com Service closing transmission channel"])
  ; (220, ["foo.com Simple Mail Transfer Service Ready"])
  ; (250, ["foo.com greets bar.com"; "8BITMIME"; "SIZE"; "DSN"; "HELP"])
  ; (250, ["OK"])
  ; (250, ["OK"])
  ; (550, ["No such user here"])
  ; (250, ["OK"])
  ; (221, ["foo.com Service closing transmission channel"])
  ; (220, ["foo.com Simple Mail Transfer Service Ready"])
  ; (250, ["foo.com greets bar.com"; "8BITMIME"; "SIZE"; "DSN"; "HELP"])
  ; (250, ["OK"])
  ; (250, ["OK"])
  ; (550, ["No such user here"])
  ; (250, ["OK"])
  ; (354, ["Start mail input; end with <CRLF>.<CRLF>"])
  ; (250, ["OK"])
  ; (221, ["foo.com Service closing transmission channel"])]

let _ =
  List.iter verify_parsing (List.combine replies results);
  Format.printf "%d checks succesful\n%!" (List.length replies)