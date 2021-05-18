open Colombe

let requests =
  [
    "EHLO bar.com\r\n";
    "MAIL FROM:<Smith@bar.com>\r\n";
    "RCPT TO:<Jones@foo.com>\r\n";
    "RCPT TO:<Green@foo.com>\r\n";
    "RCPT TO:<Brown@foo.com>\r\n";
    "DATA\r\n";
    "QUIT\r\n";
    "EHLO bar.com\r\n";
    "MAIL FROM:<Smith@bar.com>\r\n";
    "RCPT TO:<Jones@foo.com>\r\n";
    "RCPT TO:<Green@foo.com>\r\n";
    "RSET\r\n";
    "QUIT\r\n";
    "EHLO bar.com\r\n";
    "MAIL FROM:<JQP@bar.com>\r\n";
    "RCPT TO:<Jones@XYZ.COM>\r\n";
    "DATA\r\n"
    (* ; "Date: Thu, 21 May 1998 05:33:29 -0700\r\n"
       ; "From: John Q. Public <JQP@bar.com>\r\n"
       ; "Subject: The Next Meeting of the Board\r\n"
       ; "To: Jones@xyz.com\r\n"
       ; "Bill:\r\n"
       ; "The next meeting of the board of directors will be\r\n"
       ; "on Tuesday.\r\n"
       ; "John.\r\n"
       ; ".\r\n" *);
    "QUIT\r\n";
    "EHLO foo.com\r\n";
    "MAIL FROM:<JQP@bar.com>\r\n";
    "RCPT TO:<Jones@XYZ.COM>\r\n";
    "DATA\r\n"
    (* ; "Received: from bar.com by foo.com ; Thu, 21 May 1998\r\n"
       ; "    05:33:29 -0700\r\n"
       ; "Date: Thu, 21 May 1998 05:33:22 -0700\r\n"
       ; "From: John Q. Public <JQP@bar.com>\r\n"
       ; "Subject:  The Next Meeting of the Board\r\n"
       ; "To: Jones@xyz.com\r\n"
       ; "Bill:\r\n"
       ; "The next meeting of the board of directors will be\r\n"
       ; "on Tuesday.\r\n"
       ; "                        John.\r\n"
       ; ".\r\n" *);
    "QUIT\r\n";
    "EHLO bar.com\r\n";
    "VRFY Crispin\r\n";
    "MAIL FROM:<EAK@bar.com>\r\n";
    "RCPT TO:<Admin.MRC@foo.com>\r\n";
    "DATA\r\n";
    "QUIT\r\n";
    "NOOP\r\n";
  ]

let results =
  [
    `Hello Domain.(Domain [ "bar"; "com" ]);
    `Mail
      ( Some
          {
            Path.local = `String "Smith";
            domain = Domain.(Domain [ "bar"; "com" ]);
            rest = [];
          },
        [] );
    `Recipient
      Forward_path.
        ( Forward_path
            {
              Path.local = `String "Jones";
              domain = Domain.(Domain [ "foo"; "com" ]);
              rest = [];
            },
          [] );
    `Recipient
      Forward_path.
        ( Forward_path
            {
              Path.local = `String "Green";
              domain = Domain.(Domain [ "foo"; "com" ]);
              rest = [];
            },
          [] );
    `Recipient
      Forward_path.
        ( Forward_path
            {
              Path.local = `String "Brown";
              domain = Domain.(Domain [ "foo"; "com" ]);
              rest = [];
            },
          [] );
    `Data;
    `Quit;
    `Hello Domain.(Domain [ "bar"; "com" ]);
    `Mail
      ( Some
          {
            Path.local = `String "Smith";
            domain = Domain.(Domain [ "bar"; "com" ]);
            rest = [];
          },
        [] );
    `Recipient
      Forward_path.
        ( Forward_path
            {
              Path.local = `String "Jones";
              domain = Domain.(Domain [ "foo"; "com" ]);
              rest = [];
            },
          [] );
    `Recipient
      Forward_path.
        ( Forward_path
            {
              Path.local = `String "Green";
              domain = Domain.(Domain [ "foo"; "com" ]);
              rest = [];
            },
          [] );
    `Reset;
    `Quit;
    `Hello Domain.(Domain [ "bar"; "com" ]);
    `Mail
      ( Some
          {
            Path.local = `String "JQP";
            domain = Domain.(Domain [ "bar"; "com" ]);
            rest = [];
          },
        [] );
    `Recipient
      Forward_path.
        ( Forward_path
            {
              Path.local = `String "Jones";
              domain = Domain.(Domain [ "XYZ"; "COM" ]);
              rest = [];
            },
          [] );
    `Data
    (* ; `Text "Date: Thu, 21 May 1998 05:33:29 -0700"
       ; `Text "From: John Q. Public <JQP@bar.com>"
       ; `Text "Subject: The Next Meeting of the Board"
       ; `Text "To: Jones@xyz.com"
       ; `Text "Bill:"
       ; `Text "The next meeting of the board of directors will be"
       ; `Text "on Tuesday."
       ; `Text "John."
       ; `TextEnd *);
    `Quit;
    `Hello Domain.(Domain [ "foo"; "com" ]);
    `Mail
      ( Some
          {
            Path.local = `String "JQP";
            domain = Domain.(Domain [ "bar"; "com" ]);
            rest = [];
          },
        [] );
    `Recipient
      Forward_path.
        ( Forward_path
            {
              Path.local = `String "Jones";
              domain = Domain.(Domain [ "XYZ"; "COM" ]);
              rest = [];
            },
          [] );
    `Data
    (* ; `Text "Received: from bar.com by foo.com ; Thu, 21 May 1998"
       ; `Text "    05:33:29 -0700"
       ; `Text "Date: Thu, 21 May 1998 05:33:22 -0700"
       ; `Text "From: John Q. Public <JQP@bar.com>"
       ; `Text "Subject:  The Next Meeting of the Board"
       ; `Text "To: Jones@xyz.com"
       ; `Text "Bill:"
       ; `Text "The next meeting of the board of directors will be"
       ; `Text "on Tuesday."
       ; `Text "                        John."
       ; `TextEnd *);
    `Quit;
    `Hello Domain.(Domain [ "bar"; "com" ]);
    `Verify "Crispin";
    `Mail
      ( Some
          {
            Path.local = `String "EAK";
            domain = Domain.(Domain [ "bar"; "com" ]);
            rest = [];
          },
        [] );
    `Recipient
      Forward_path.
        ( Forward_path
            {
              Path.local = `Dot_string [ "Admin"; "MRC" ];
              domain = Domain.(Domain [ "foo"; "com" ]);
              rest = [];
            },
          [] );
    `Data;
    `Quit;
    `Noop None;
  ]

let request = Alcotest.testable Request.pp Request.equal

let test_requests_0 () =
  let make (raw, expected) =
    Alcotest.test_case (String.sub raw 0 (String.length raw - 2)) `Quick
    @@ fun () ->
    match Request.Decoder.of_string raw with
    | Ok result -> Alcotest.(check request) raw expected result
    | Error err -> Alcotest.failf "%a." Request.Decoder.pp_error err in
  List.map make (List.combine requests results)

let test_requests_1 () =
  let make (value, raw) =
    Alcotest.test_case (Fmt.to_to_string Request.pp value) `Quick @@ fun () ->
    match Request.Encoder.to_string value with
    | Ok result ->
        Alcotest.(check string) (Fmt.to_to_string Request.pp value) raw result
    | Error err -> Alcotest.failf "%a." Request.Encoder.pp_error err in
  List.map make (List.combine results requests)

let replies =
  [
    "220 foo.com Simple Mail Transfer Service Ready\r\n";
    "250-foo.com greets bar.com\r\n\
     250-8BITMIME\r\n\
     250-SIZE\r\n\
     250-DSN\r\n\
     250-VRFY\r\n\
     250 HELP\r\n";
    "250 Mark Crispin <Admin.MRC@foo.com>\r\n";
    "250 OK\r\n";
    "250 OK\r\n";
    "354 Start mail input; end with <CRLF>.<CRLF>\r\n";
    "250 OK\r\n";
    "221 foo.com Service closing transmission channel\r\n";
    "220 xyz.com Simple Mail Transfer Service Ready\r\n";
    "250 xyz.com is on the air\r\n";
    "250 OK\r\n";
    "250 OK\r\n";
    "354 Start mail input; end with <CRLF>.<CRLF>\r\n";
    "250 OK\r\n";
    "221 foo.com Service closing transmission channel\r\n";
    "220 foo.com Simple Mail Transfer Service Ready\r\n";
    "250-foo.com greets bar.com\r\n\
     250-8BITMIME\r\n\
     250-SIZE\r\n\
     250-DSN\r\n\
     250 HELP\r\n";
    "250 OK\r\n";
    "250 OK\r\n";
    "354 Start mail input; end with <CRLF>.<CRLF>\r\n";
    "250 OK\r\n";
    "221 foo.com Service closing transmission channel\r\n";
    "220 foo.com Simple Mail Transfer Service Ready\r\n";
    "250-foo.com greets bar.com\r\n\
     250-8BITMIME\r\n\
     250-SIZE\r\n\
     250-DSN\r\n\
     250 HELP\r\n";
    "250 OK\r\n";
    "250 OK\r\n";
    "550 No such user here\r\n";
    "250 OK\r\n";
    "221 foo.com Service closing transmission channel\r\n";
    "220 foo.com Simple Mail Transfer Service Ready\r\n";
    "250-foo.com greets bar.com\r\n\
     250-8BITMIME\r\n\
     250-SIZE\r\n\
     250-DSN\r\n\
     250 HELP\r\n";
    "250 OK\r\n";
    "250 OK\r\n";
    "550 No such user here\r\n";
    "250 OK\r\n";
    "354 Start mail input; end with <CRLF>.<CRLF>\r\n";
    "250 OK\r\n";
    "221 foo.com Service closing transmission channel\r\n";
    "666 Going to Hell!\r\n";
  ]

let results =
  [
    `PP_220 [ "foo.com Simple Mail Transfer Service Ready" ];
    `PP_250
      [ "foo.com greets bar.com"; "8BITMIME"; "SIZE"; "DSN"; "VRFY"; "HELP" ];
    `PP_250 [ "Mark Crispin <Admin.MRC@foo.com>" ];
    `PP_250 [ "OK" ];
    `PP_250 [ "OK" ];
    `TP_354 [ "Start mail input; end with <CRLF>.<CRLF>" ];
    `PP_250 [ "OK" ];
    `PP_221 [ "foo.com Service closing transmission channel" ];
    `PP_220 [ "xyz.com Simple Mail Transfer Service Ready" ];
    `PP_250 [ "xyz.com is on the air" ];
    `PP_250 [ "OK" ];
    `PP_250 [ "OK" ];
    `TP_354 [ "Start mail input; end with <CRLF>.<CRLF>" ];
    `PP_250 [ "OK" ];
    `PP_221 [ "foo.com Service closing transmission channel" ];
    `PP_220 [ "foo.com Simple Mail Transfer Service Ready" ];
    `PP_250 [ "foo.com greets bar.com"; "8BITMIME"; "SIZE"; "DSN"; "HELP" ];
    `PP_250 [ "OK" ];
    `PP_250 [ "OK" ];
    `TP_354 [ "Start mail input; end with <CRLF>.<CRLF>" ];
    `PP_250 [ "OK" ];
    `PP_221 [ "foo.com Service closing transmission channel" ];
    `PP_220 [ "foo.com Simple Mail Transfer Service Ready" ];
    `PP_250 [ "foo.com greets bar.com"; "8BITMIME"; "SIZE"; "DSN"; "HELP" ];
    `PP_250 [ "OK" ];
    `PP_250 [ "OK" ];
    `PN_550 [ "No such user here" ];
    `PP_250 [ "OK" ];
    `PP_221 [ "foo.com Service closing transmission channel" ];
    `PP_220 [ "foo.com Simple Mail Transfer Service Ready" ];
    `PP_250 [ "foo.com greets bar.com"; "8BITMIME"; "SIZE"; "DSN"; "HELP" ];
    `PP_250 [ "OK" ];
    `PP_250 [ "OK" ];
    `PN_550 [ "No such user here" ];
    `PP_250 [ "OK" ];
    `TP_354 [ "Start mail input; end with <CRLF>.<CRLF>" ];
    `PP_250 [ "OK" ];
    `PP_221 [ "foo.com Service closing transmission channel" ];
    `Other (666, [ "Going to Hell!" ]);
  ]

let reply = Alcotest.testable Reply.pp Reply.equal

let test_replies_0 () =
  let get_first_line raw =
    let chr = ref '\000' in
    let idx = ref 0 in
    let has_cr = ref false in
    while
      !idx < String.length raw
      &&
      (chr := String.get raw !idx ;
       not (!chr = '\n' && !has_cr))
    do
      has_cr := !chr = '\r' ;
      incr idx
    done ;
    String.sub raw 0 (!idx - 1)
    ^ if !idx + 1 = String.length raw then "" else "..." in
  let make (raw, expected) =
    Alcotest.test_case (get_first_line raw) `Quick @@ fun () ->
    match Reply.Decoder.of_string raw with
    | Ok result -> Alcotest.(check reply) raw expected result
    | Error err -> Alcotest.failf "%a." Reply.Decoder.pp_error err in
  List.map make (List.combine replies results)

let test_replies_1 () =
  let make (value, raw) =
    Alcotest.test_case (Fmt.to_to_string Reply.pp value) `Quick @@ fun () ->
    match Reply.Encoder.to_string value with
    | Ok result ->
        Alcotest.(check string) (Fmt.to_to_string Reply.pp value) raw result
    | Error err -> Alcotest.failf "%a." Reply.Encoder.pp_error err in
  List.map make (List.combine results replies)

let malformed_354 () =
  Alcotest.test_case "malformed 354" `Quick @@ fun () ->
  let res0 = Reply.Decoder.of_string "354\r\n" in
  let res1 = Reply.Decoder.of_string "354foo\r\n" in
  match (res0, res1) with
  | Ok _, Error _ -> Alcotest.(check pass) "resilient 354 reply parser" () ()
  | Error _, _ -> Alcotest.failf "Impossible to parse \"354\""
  | _, Ok _ -> Alcotest.failf "Should not be able to parse \"354foo\""

let () =
  Alcotest.run "colombe"
    [
      ("requests 0", test_requests_0 ());
      ("requests 1", test_requests_1 ());
      ("replies 0", test_replies_0 ());
      ("replies 1", test_replies_1 ());
      ("malformed 354", [ malformed_354 () ]);
    ]
