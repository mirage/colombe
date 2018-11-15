open Colombe

exception WrongRequestParsing of string * string

let requests =
  [ "EHLO bar.com\r\n"
  ; "MAIL FROM:<Smith@bar.com>\r\n"
  ; "RCPT TO:<Jones@foo.com>\r\n"
  ; "RCPT TO:<Green@foo.com>\r\n"
  ; "RCPT TO:<Brown@foo.com>\r\n"
  ; "DATA\r\n"
  ; "Blah blah blah...\r\n"
  ; "...etc. etc. etc.\r\n"
  ; ".\r\n"
  ; "QUIT\r\n"
  ; "EHLO bar.com\r\n"
  ; "MAIL FROM:<Smith@bar.com>\r\n"
  ; "RCPT TO:<Jones@foo.com>\r\n"
  ; "RCPT TO:<Green@foo.com>\r\n"
  ; "RSET\r\n"
  ; "QUIT\r\n"
  ; "EHLO bar.com\r\n"
  ; "MAIL FROM:<JQP@bar.com>\r\n"
  ; "RCPT TO:<Jones@XYZ.COM>\r\n"
  ; "DATA\r\n"
  ; "Date: Thu, 21 May 1998 05:33:29 -0700\r\n"
  ; "From: John Q. Public <JQP@bar.com>\r\n"
  ; "Subject: The Next Meeting of the Board\r\n"
  ; "To: Jones@xyz.com\r\n"
  ; "Bill:\r\n"
  ; "The next meeting of the board of directors will be\r\n"
  ; "on Tuesday.\r\n"
  ; "John.\r\n"
  ; ".\r\n"
  ; "QUIT\r\n"
  ; "EHLO foo.com\r\n"
  ; "MAIL FROM:<JQP@bar.com>\r\n"
  ; "RCPT TO:<Jones@XYZ.COM>\r\n"
  ; "DATA\r\n"
  ; "Received: from bar.com by foo.com ; Thu, 21 May 1998\r\n"
  ; "    05:33:29 -0700\r\n"
  ; "Date: Thu, 21 May 1998 05:33:22 -0700\r\n"
  ; "From: John Q. Public <JQP@bar.com>\r\n"
  ; "Subject:  The Next Meeting of the Board\r\n"
  ; "To: Jones@xyz.com\r\n"
  ; "Bill:\r\n"
  ; "The next meeting of the board of directors will be\r\n"
  ; "on Tuesday.\r\n"
  ; "                        John.\r\n"
  ; ".\r\n"
  ; "QUIT\r\n"
  ; "EHLO bar.com\r\n"
  ; "VRFY Crispin\r\n"
  ; "MAIL FROM:<EAK@bar.com>\r\n"
  ; "RCPT TO:<Admin.MRC@foo.com>\r\n"
  ; "DATA\r\n"
  ; "Blah blah blah...\r\n"
  ; "...etc. etc. etc.\r\n"
  ; ".\r\n"
  ; "QUIT\r\n"
  ; "NOOP\r\n" ]

let requests_results =
  [ `EHLO "bar.com"
  ; `MAIL "<Smith@bar.com>"
  ; `RCPT "<Jones@foo.com>"
  ; `RCPT "<Green@foo.com>"
  ; `RCPT "<Brown@foo.com>"
  ; `DATA
  ; `Text "Blah blah blah..."
  ; `Text "...etc. etc. etc."
  ; `TextEnd
  ; `QUIT
  ; `EHLO "bar.com"
  ; `MAIL "<Smith@bar.com>"
  ; `RCPT "<Jones@foo.com>"
  ; `RCPT "<Green@foo.com>"
  ; `RSET
  ; `QUIT
  ; `EHLO "bar.com"
  ; `MAIL "<JQP@bar.com>"
  ; `RCPT "<Jones@XYZ.COM>"
  ; `DATA
  ; `Text "Date: Thu, 21 May 1998 05:33:29 -0700"
  ; `Text "From: John Q. Public <JQP@bar.com>"
  ; `Text "Subject: The Next Meeting of the Board"
  ; `Text "To: Jones@xyz.com"
  ; `Text "Bill:"
  ; `Text "The next meeting of the board of directors will be"
  ; `Text "on Tuesday."
  ; `Text "John."
  ; `TextEnd
  ; `QUIT
  ; `EHLO "foo.com"
  ; `MAIL "<JQP@bar.com>"
  ; `RCPT "<Jones@XYZ.COM>"
  ; `DATA
  ; `Text "Received: from bar.com by foo.com ; Thu, 21 May 1998"
  ; `Text "    05:33:29 -0700"
  ; `Text "Date: Thu, 21 May 1998 05:33:22 -0700"
  ; `Text "From: John Q. Public <JQP@bar.com>"
  ; `Text "Subject:  The Next Meeting of the Board"
  ; `Text "To: Jones@xyz.com"
  ; `Text "Bill:"
  ; `Text "The next meeting of the board of directors will be"
  ; `Text "on Tuesday."
  ; `Text "                        John."
  ; `TextEnd
  ; `QUIT
  ; `EHLO "bar.com"
  ; `VRFY "Crispin"
  ; `MAIL "<EAK@bar.com>"
  ; `RCPT "<Admin.MRC@foo.com>"
  ; `DATA
  ; `Text "Blah blah blah..."
  ; `Text "...etc. etc. etc."
  ; `TextEnd
  ; `QUIT
  ; `NOOP ]

let test_request () =
  let verify_request (request, expected) =
    Format.printf ".%!";
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
  ; "666 test\r\n"]

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
    Format.printf ".%!";
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