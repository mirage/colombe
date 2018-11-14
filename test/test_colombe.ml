open Colombe

exception WrongRequestParsing of string * string

let requests =
  (* [ "EHLO bar.com"
  ; "MAIL FROM:<Smith@bar.com>"
  ; "RCPT TO:<Jones@foo.com>"
  ; "RCPT TO:<Green@foo.com>"
  ; "RCPT TO:<Brown@foo.com>"
  ; "DATA"
  ; "Blah blah blah...\
    ...etc. etc. etc.\
    ."
  ; "QUIT"
  ; "EHLO bar.com"
  ; "MAIL FROM:<Smith@bar.com>"
  ; "RCPT TO:<Jones@foo.com>"
  ; "RCPT TO:<Green@foo.com>"
  ; "RSET"
  ; "QUIT"
  ; "EHLO bar.com"
  ; "MAIL FROM:<JQP@bar.com>"
  ; "RCPT TO:<Jones@XYZ.COM>"
  ; "DATA"
  ; "Date: Thu, 21 May 1998 05:33:29 -0700\
    From: John Q. Public <JQP@bar.com>\
    Subject: The Next Meeting of the Board\
    To: Jones@xyz.com\
    Bill:\
    The next meeting of the board of directors will be\
    on Tuesday.\
    John.\
    ."
  ; "QUIT"
  ; "EHLO foo.com"
  ; "MAIL FROM:<JQP@bar.com>"
  ; "RCPT TO:<Jones@XYZ.COM>"
  ; "DATA"
  ; "Received: from bar.com by foo.com ; Thu, 21 May 1998\
        05:33:29 -0700\
    Date: Thu, 21 May 1998 05:33:22 -0700\
    From: John Q. Public <JQP@bar.com>\
    Subject:  The Next Meeting of the Board\
    To: Jones@xyz.com\
    Bill:\
    The next meeting of the board of directors will be\
    on Tuesday.\
                            John.\
    ."
  ; "QUIT"
  ; "EHLO bar.com"
  ; "VRFY Crispin"
  ; "MAIL FROM:<EAK@bar.com>"
  ; "RCPT TO:<Admin.MRC@foo.com>"
  ; "DATA"
  ; "Blah blah blah...\
    ...etc. etc. etc.\
    ."
  ; "QUIT" ] *)
  [ "EHLO\r\n"
  ; "MAIL\r\n"
  ; "RCPT\r\n"
  ; "RCPT\r\n"
  ; "RCPT\r\n"
  ; "DATA\r\n"
  ; "QUIT\r\n"
  ; "EHLO\r\n"
  ; "MAIL\r\n"
  ; "RCPT\r\n"
  ; "RCPT\r\n"
  ; "RSET\r\n"
  ; "QUIT\r\n"
  ; "EHLO\r\n"
  ; "MAIL\r\n"
  ; "RCPT\r\n"
  ; "DATA\r\n"
  ; "QUIT\r\n"
  ; "EHLO\r\n"
  ; "MAIL\r\n"
  ; "RCPT\r\n"
  ; "DATA\r\n"
  ; "QUIT\r\n"
  ; "EHLO\r\n"
  ; "VRFY\r\n"
  ; "MAIL\r\n"
  ; "RCPT\r\n"
  ; "DATA\r\n"
  ; "QUIT\r\n"
  ; "NOOP\r\n" ]

let requests_results =
  [ `Other "EHLO"
  ; `Other "MAIL"
  ; `Other "RCPT"
  ; `Other "RCPT"
  ; `Other "RCPT"
  ; `Other "DATA"
  ; `Other "QUIT"
  ; `Other "EHLO"
  ; `Other "MAIL"
  ; `Other "RCPT"
  ; `Other "RCPT"
  ; `Other "RSET"
  ; `Other "QUIT"
  ; `Other "EHLO"
  ; `Other "MAIL"
  ; `Other "RCPT"
  ; `Other "DATA"
  ; `Other "QUIT"
  ; `Other "EHLO"
  ; `Other "MAIL"
  ; `Other "RCPT"
  ; `Other "DATA"
  ; `Other "QUIT"
  ; `Other "EHLO"
  ; `Other "VRFY"
  ; `Other "MAIL"
  ; `Other "RCPT"
  ; `Other "DATA"
  ; `Other "QUIT"
  ; `NOOP ]

let test_request () =
  let verify_request (request, expected) =
    let parsed = Request.eval request in
    if Request.compare parsed expected = false
    then
      raise (WrongRequestParsing (Request.command parsed, Request.command expected))
  in
  List.iter verify_request (List.combine requests requests_results);
  Format.printf "Request: %d checks succesful\n%!" (List.length requests)

exception WrongReplyParsing of (int * string list) * (int * string list)

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
  ; "221 foo.com Service closing transmission channel\r\n"
  ; "666 test\r\n" ]

let replies_results =
  [ `PP_220 ["foo.com Simple Mail Transfer Service Ready"]
  ; `PP_250 ["foo.com greets bar.com"; "8BITMIME"; "SIZE"; "DSN"; "VRFY"; "HELP"]
  ; `PP_250 ["Mark Crispin <Admin.MRC@foo.com>"]
  ; `PP_250 ["OK"]
  ; `PP_250 ["OK"]
  ; `TP_354 ["Start mail input; end with <CRLF>.<CRLF>"]
  ; `PP_250 ["OK"]
  ; `PP_221 ["foo.com Service closing transmission channel"]
  ; `PP_220 ["xyz.com Simple Mail Transfer Service Ready"]
  ; `PP_250 ["xyz.com is on the air"]
  ; `PP_250 ["OK"]
  ; `PP_250 ["OK"]
  ; `TP_354 ["Start mail input; end with <CRLF>.<CRLF>"]
  ; `PP_250 ["OK"]
  ; `PP_221 ["foo.com Service closing transmission channel"]
  ; `PP_220 ["foo.com Simple Mail Transfer Service Ready"]
  ; `PP_250 ["foo.com greets bar.com"; "8BITMIME"; "SIZE"; "DSN"; "HELP"]
  ; `PP_250 ["OK"]
  ; `PP_250 ["OK"]
  ; `TP_354 ["Start mail input; end with <CRLF>.<CRLF>"]
  ; `PP_250 ["OK"]
  ; `PP_221 ["foo.com Service closing transmission channel"]
  ; `PP_220 ["foo.com Simple Mail Transfer Service Ready"]
  ; `PP_250 ["foo.com greets bar.com"; "8BITMIME"; "SIZE"; "DSN"; "HELP"]
  ; `PP_250 ["OK"]
  ; `PP_250 ["OK"]
  ; `PN_550 ["No such user here"]
  ; `PP_250 ["OK"]
  ; `PP_221 ["foo.com Service closing transmission channel"]
  ; `PP_220 ["foo.com Simple Mail Transfer Service Ready"]
  ; `PP_250 ["foo.com greets bar.com"; "8BITMIME"; "SIZE"; "DSN"; "HELP"]
  ; `PP_250 ["OK"]
  ; `PP_250 ["OK"]
  ; `PN_550 ["No such user here"]
  ; `PP_250 ["OK"]
  ; `TP_354 ["Start mail input; end with <CRLF>.<CRLF>"]
  ; `PP_250 ["OK"]
  ; `PP_221 ["foo.com Service closing transmission channel"]
  ; `Other (666, ["test"])]

let test_reply () =
  let verify_reply (reply, expected) =
    let parsed = Reply.eval reply in
    if Reply.compare parsed expected = false
    then
      raise (WrongReplyParsing ((Reply.code parsed, Reply.texts expected), (Reply.code parsed, Reply.texts expected)))
  in
  List.iter verify_reply (List.combine replies replies_results);
  Format.printf "Reply: %d checks succesful\n%!" (List.length replies)

let _ =
  test_request ();
  test_reply ();;