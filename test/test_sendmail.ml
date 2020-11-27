let romain_calascibetta =
  let open Mrmime in
  let open Mailbox in
  Local.[ w "romain"; w "calascibetta" ] @ Domain.(domain, [ a "gmail"; a "com" ])

let anil =
  let open Mrmime in
  let open Mailbox in
  Local.[ w "anil" ] @ Domain.(domain, [ a "recoil"; a "org" ])

module Unix_scheduler = Colombe.Sigs.Make(struct type 'a t = 'a end)
let unix = { Colombe.Sigs.bind= (fun x f -> let open Unix_scheduler in f (prj x))
           ; return= (fun x -> Unix_scheduler.inj x) }

let put_crlf x = x ^ "\r\n"

let rdwr_from_flows inputs outputs =
  let inputs = ref (List.map put_crlf inputs) in
  let outputs = ref (List.map put_crlf outputs) in
  let read () bytes off len =
    match !inputs with
    | [] -> Unix_scheduler.inj 0
    | x :: r ->
      let len = min (String.length x) len in
      Bytes.blit_string x 0 bytes off len ;
      Fmt.epr "[rd] >>> %S\n%!" (String.sub x 0 len) ;
      if len = String.length x then inputs := r else inputs := (String.sub x len (String.length x - len)) :: r ;
      Unix_scheduler.inj len in
  let rec write () bytes off len =
    match !outputs with
    | [] ->
      Fmt.failwith "Unexpected output: %S" (String.sub bytes off len)
    | x :: r ->
      let max = len in
      let len = min (String.length x) len in
      Fmt.epr "[wr] <<< %S\n%!" (String.sub x 0 len) ;
      if String.sub x 0 len <> String.sub bytes off len
      then Fmt.failwith "Expected %S, have %S" (String.sub x 0 len) (String.sub bytes off len) ;
      if String.length x = len then outputs := r else outputs := (String.sub x len (String.length x - len)) :: r ;
      if len < max then write () bytes (off + len) (max - len) else Unix_scheduler.inj () in
  { Colombe.Sigs.rd= read; wr= write; },
  (fun () -> match !inputs, !outputs with
     | [], [] -> ()
     | r, w -> Fmt.failwith "inputs or outputs are not empty: @[<hov>%a@] and @[<hov>%a@]"
                 Fmt.(Dump.list string) r Fmt.(Dump.list string) w)

let test_0 () =
  Alcotest.test_case "usual without authentication" `Quick @@ fun () ->
  let ctx = Colombe.State.Context.make () in
  let rdwr, is_empty =
    rdwr_from_flows
      [ "220 smtp.gmail.com ESTMP - gsmtp"
      ; "250-smtp.gmail.com at your service, [8.8.8.8]"
      ; "250 AUTH LOGIN PLAIN"
      ; "250 <romain.calascibetta@gmail.com> as sender"
      ; "250 <anil@recoil.org> as recipient"
      ; "354 "
      ; "250 Sended!"
      ; "221 Closing connection." ]
      [ "EHLO gmail.com"
      ; "MAIL FROM:<romain.calascibetta@gmail.com>"
      ; "RCPT TO:<anil@recoil.org>"
      ; "DATA"
      ; "."
      ; "QUIT" ] in
  let fiber = Sendmail.sendmail
      unix rdwr () ctx
      ~domain:(Colombe.Domain.Domain [ "gmail"; "com" ])
      (Rresult.R.get_ok @@ Colombe_emile.to_reverse_path romain_calascibetta)
      [ Rresult.R.get_ok @@ Colombe_emile.to_forward_path anil ]
      (fun () -> unix.return None) in
  match Unix_scheduler.prj fiber with
  | Error err -> Fmt.failwith "Got an error: %a" Sendmail.pp_error err
  | Ok _ -> is_empty ()

let test_1 () =
  Alcotest.test_case "usual with authentication" `Quick @@ fun () ->
  let ctx = Colombe.State.Context.make () in
  let rdwr, is_empty =
    rdwr_from_flows
      [ "220 smtp.gmail.com ESTMP - gsmtp"
      ; "250-smtp.gmail.com at your service, [8.8.8.8]"
      ; "250 AUTH LOGIN PLAIN"
      ; "334 "
      ; "235 authenticated"
      ; "250 <romain.calascibetta@gmail.com> as sender"
      ; "250 <anil@recoil.org> as recipient"
      ; "354 "
      ; "250 Sended!"
      ; "221 Closing connection." ]
      [ "EHLO gmail.com"
      ; "AUTH PLAIN"
      ; Base64.encode_exn ~pad:true (Fmt.strf "\000romain\000foobar")
      ; "MAIL FROM:<romain.calascibetta@gmail.com>"
      ; "RCPT TO:<anil@recoil.org>"
      ; "DATA"
      ; "."
      ; "QUIT" ] in
  let authentication = 
    { Sendmail.mechanism= PLAIN
    ; username= "romain"
    ; password= "foobar" } in
  let fiber = Sendmail.sendmail
      unix rdwr () ctx
      ~domain:(Colombe.Domain.Domain [ "gmail"; "com" ])
      (Rresult.R.get_ok @@ Colombe_emile.to_reverse_path romain_calascibetta)
      [ Rresult.R.get_ok @@ Colombe_emile.to_forward_path anil ]
      ~authentication (fun () -> unix.return None) in
  match Unix_scheduler.prj fiber with
  | Error err -> Fmt.failwith "Got an error: %a" Sendmail.pp_error err
  | Ok _ -> is_empty ()

let test_2 () =
  Alcotest.test_case "bad authentication" `Quick @@ fun () ->
  let ctx = Colombe.State.Context.make () in
  let rdwr, is_empty =
    rdwr_from_flows
      [ "220 smtp.gmail.com ESMTP - gsmtp"
      ; "250-smtp.gmail.com at your service, [8.8.8.8]"
      ; "250-SIZE 0"
      ; "250-AUTH LOGIN PLAIN"
      ; "250-ENHANCEDSTATUSCODES"
      ; "250 CHUNKING"
      ; "334 "
      ; "501 Authentication rejected!"
      ; "221 Closing connection." ]
      [ "EHLO gmail.com"
      ; "AUTH PLAIN"
      ; Base64.encode_exn ~pad:true (Fmt.strf "\000romain\000foobar")
      ; "QUIT" ] in
  let authentication = 
    { Sendmail.mechanism= PLAIN
    ; username= "romain"
    ; password= "foobar" } in
  let fiber = Sendmail.sendmail
      unix rdwr () ctx
      ~domain:(Colombe.Domain.Domain [ "gmail"; "com" ])
      (Rresult.R.get_ok @@ Colombe_emile.to_reverse_path romain_calascibetta)
      [ Rresult.R.get_ok @@ Colombe_emile.to_forward_path anil ]
      ~authentication (fun () -> unix.return None) in
  match Unix_scheduler.prj fiber with
  | Error err ->
    is_empty () ; assert (err = `Authentication_rejected)
  | Ok _ -> Fmt.failwith "Should fail with [Authentication_rejected]"

let test_3 () =
  Alcotest.test_case "PLAIN mechanism unavailable" `Quick @@ fun () ->
  let ctx = Colombe.State.Context.make () in
  let rdwr, is_empty =
    rdwr_from_flows
      [ "220 smtp.gmail.com ESMTP - gsmtp"
      ; "250-smtp.gmail.com at your service, [8.8.8.8]"
      ; "250 AUTH LOGIN"
      ; "504 Unsupported mechanism"
      ; "221 Closing connection." ]
      [ "EHLO gmail.com"
      ; "AUTH PLAIN"
      ; "QUIT" ] in
  let authentication = 
    { Sendmail.mechanism= PLAIN
    ; username= "romain"
    ; password= "foobar" } in
  let fiber = Sendmail.sendmail
      unix rdwr () ctx
      ~domain:(Colombe.Domain.Domain [ "gmail"; "com" ])
      (Rresult.R.get_ok @@ Colombe_emile.to_reverse_path romain_calascibetta)
      [ Rresult.R.get_ok @@ Colombe_emile.to_forward_path anil ]
      ~authentication (fun () -> unix.return None) in
  match Unix_scheduler.prj fiber with
  | Error err ->
    is_empty () ; assert (err = `Unsupported_mechanism)
  | Ok _ -> Fmt.failwith "Should fail with [Unsupported_mechanism]"

let test_4 () =
  Alcotest.test_case "authentication required" `Quick @@ fun () ->
  let ctx = Colombe.State.Context.make () in
  let rdwr, is_empty =
    rdwr_from_flows
      [ "220 smtp.gmail.com ESMTP - gsmtp"
      ; "250-smtp.gmail.com at your service, [8.8.8.8]"
      ; "250 AUTH LOGIN"
      ; "530 Authentication required"
      ; "221 Closing connection." ]
      [ "EHLO gmail.com"
      ; "MAIL FROM:<romain.calascibetta@gmail.com>"
      ; "QUIT" ] in
  let fiber = Sendmail.sendmail
      unix rdwr () ctx
      ~domain:(Colombe.Domain.Domain [ "gmail"; "com" ])
      (Rresult.R.get_ok @@ Colombe_emile.to_reverse_path romain_calascibetta)
      [ Rresult.R.get_ok @@ Colombe_emile.to_forward_path anil ]
      (fun () -> unix.return None) in
  match Unix_scheduler.prj fiber with
  | Error err -> is_empty () ; assert (`Authentication_required = err)
  | Ok _ -> Fmt.failwith "Should fail with [Encryption_required]"

let test_5 () =
  Alcotest.test_case "8BITMIME" `Quick @@ fun () ->
  let ctx = Colombe.State.Context.make () in
  let rdwr, is_empty =
    rdwr_from_flows
      [ "220 smtp.gmail.com ESTMP - gsmtp"
      ; "250-smtp.gmail.com at your service, [8.8.8.8]"
      ; "250-AUTH LOGIN PLAIN"
      ; "250 8BITMIME"
      ; "334 "
      ; "235 authenticated"
      ; "250 <romain.calascibetta@gmail.com> as sender"
      ; "250 <anil@recoil.org> as recipient"
      ; "354 "
      ; "250 Sended!"
      ; "221 Closing connection." ]
      [ "EHLO gmail.com"
      ; "AUTH PLAIN"
      ; Base64.encode_exn ~pad:true (Fmt.strf "\000romain\000foobar")
      ; "MAIL FROM:<romain.calascibetta@gmail.com> BODY=8BITMIME"
      ; "RCPT TO:<anil@recoil.org>"
      ; "DATA"
      ; "."
      ; "QUIT" ] in
  let authentication = 
    { Sendmail.mechanism= PLAIN
    ; username= "romain"
    ; password= "foobar" } in
  let fiber = Sendmail.sendmail
      unix rdwr () ctx
      ~domain:(Colombe.Domain.Domain [ "gmail"; "com" ])
      (Rresult.R.get_ok @@ Colombe_emile.to_reverse_path romain_calascibetta)
      [ Rresult.R.get_ok @@ Colombe_emile.to_forward_path anil ]
      ~authentication (fun () -> unix.return None) in
  match Unix_scheduler.prj fiber with
  | Error err -> Fmt.failwith "Got an error: %a" Sendmail.pp_error err
  | Ok _ -> is_empty ()

let test_6 () =
  Alcotest.test_case "spam" `Quick @@ fun () ->
  let ctx = Colombe.State.Context.make () in
  let rdwr, is_empty =
    rdwr_from_flows
      [ "220 smtp.gmail.com ESTMP - gsmtp"
      ; "250-smtp.gmail.com at your service, [8.8.8.8]"
      ; "250-AUTH LOGIN PLAIN"
      ; "250 8BITMIME"
      ; "550 Client host blocked!" ]
      [ "EHLO gmail.com"
      ; "MAIL FROM:<romain.calascibetta@gmail.com> BODY=8BITMIME" ] in
  let fiber = Sendmail.sendmail
      unix rdwr () ctx
      ~domain:(Colombe.Domain.Domain [ "gmail"; "com" ])
      (Rresult.R.get_ok @@ Colombe_emile.to_reverse_path romain_calascibetta)
      [ Rresult.R.get_ok @@ Colombe_emile.to_forward_path anil ]
      (fun () -> unix.return None) in
  match Unix_scheduler.prj fiber with
  | Error (`Protocol (`Unexpected_response (550, _))) -> is_empty ()
  | Error err -> Fmt.failwith "Got an error: %a" Sendmail.pp_error err
  | Ok _ -> Fmt.failwith "Unexpected valid result"

let test_7 () =
  Alcotest.test_case "ethereal" `Quick @@ fun () ->
  let ctx = Colombe.State.Context.make () in
  let rdwr, is_empty =
    rdwr_from_flows
      [ "220 smtp.gmail.com ESTMP - gsmtp"
      ; "250-smtp.gmail.com at your service, [8.8.8.8]"
      ; "250 AUTH LOGIN PLAIN"
      ; "250 <romain.calascibetta@gmail.com> as sender"
      ; "250 <anil@recoil.org> as recipient"
      ; "354" (* no space, must resilient. *)
      ; "250 Sended!"
      ; "221 Closing connection." ]
      [ "EHLO ethereal.email"
      ; "MAIL FROM:<romain.calascibetta@gmail.com>"
      ; "RCPT TO:<anil@recoil.org>"
      ; "DATA"
      ; "."
      ; "QUIT" ] in
  let fiber = Sendmail.sendmail
      unix rdwr () ctx
      ~domain:(Colombe.Domain.Domain [ "ethereal"; "email" ])
      (Rresult.R.get_ok @@ Colombe_emile.to_reverse_path romain_calascibetta)
      [ Rresult.R.get_ok @@ Colombe_emile.to_forward_path anil ]
      (fun () -> unix.return None) in
  match Unix_scheduler.prj fiber with
  | Error err -> Fmt.failwith "Got an error: %a" Sendmail.pp_error err
  | Ok _ -> is_empty ()

let stream_of_string str =
  let once = ref false in
  fun () -> if !once then None else ( once := true ; Some (str, 0, String.length str) )

let stream_of_strings vs =
  let vs = ref vs in
  fun () -> match !vs with
            | [] -> None
            | hd :: tl -> vs := tl ; Some (hd, 0, String.length hd)

let _78_a_with_dot_rw =
  [ String.make 78 'a' ^ "\r\n"
  ; "." ^ String.make 77 'a' ]

let _75_a_with_dot_qp =
  String.make 75 'a' ^ "." ^ String.make 74 'a'

let test_8 () =
  Alcotest.test_case "dot" `Quick @@ fun () ->
  let ctx = Colombe.State.Context.make () in
  let mail =
    let open Mrmime in
    let subject = Unstructured.Craft.(compile [ v "Simple"; sp 1; v "Mail" ]) in
    let hd0 =
      let content0 = Content_type.(make `Text (Subtype.v `Text "plain") Parameters.(of_list [ k "charset", v "utf-8" ])) in
      Header.of_list Field.[ Field (Field_name.content_type, Content, content0)
                                         ; Field (Field_name.content_encoding, Encoding, `Bit8) ] in
    let hd1 =
      let content1 = Content_type.(make `Text (Subtype.v `Text "plain") Parameters.(of_list [ k "charset", v "utf-8" ])) in
      Header.of_list Field.[ Field (Field_name.content_type, Content, content1)
                                         ; Field (Field_name.content_encoding, Encoding, `Quoted_printable) ] in
    let part0 = Mt.part ~header:hd0 (stream_of_strings _78_a_with_dot_rw) in
    let part1 = Mt.part ~header:hd1 (stream_of_string _75_a_with_dot_qp) in
    let header =
      Header.of_list
        Field.[ Field (Field_name.sender, Mailbox, romain_calascibetta)
                     ; Field (Field_name.subject, Unstructured, subject)
                     ; Field (Field_name.v "To", Addresses, [ `Mailbox anil ]) ] in
    Mt.make header Mt.multi (Mt.multipart ~boundary:"boundary" ~rng:Mt.rng [ part0; part1 ]) in
  let stream =
    let stream = Mrmime.Mt.to_stream mail in
    fun () -> unix.return (stream ()) in
  let rdwr, is_empty =
    rdwr_from_flows
      [ "220 smtp.gmail.com ESTMP - gsmtp"
      ; "250-smtp.gmail.com at your service, [8.8.8.8]"
      ; "250 AUTH LOGIN PLAIN"
      ; "250 <romain.calascibetta@gmail.com> as sender"
      ; "250 <anil@recoil.org> as recipient"
      ; "354 "
      ; "250 Sended!"
      ; "221 Closing connection." ]
      [ "EHLO mailtrap.io"
      ; "MAIL FROM:<romain.calascibetta@gmail.com>"
      ; "RCPT TO:<anil@recoil.org>"
      ; "DATA"
      ; "Sender: romain.calascibetta@gmail.com"
      ; "Subject: Simple Mail"
      ; "To: anil@recoil.org"
      ; "Content-Type: multipart/mixed; boundary=boundary"
      ; ""
      ; "--boundary"
      ; "Content-Type: text/plain; charset=utf-8"
      ; "Content-Transfer-Encoding: 8bit"
      ; ""
      ; "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
      ; "..aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
      ; "--boundary"
      ; "Content-Type: text/plain; charset=utf-8"
      ; "Content-Transfer-Encoding: quoted-printable"
      ; ""
      ; "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa="
      ; "..aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"
      ; ""
      ; "--boundary--"
      ; "."
      ; "QUIT" ] in
  let fiber = Sendmail.sendmail
                unix rdwr () ctx
                ~domain:(Colombe.Domain.Domain [ "mailtrap"; "io" ])
                (Rresult.R.get_ok @@ Colombe_emile.to_reverse_path romain_calascibetta)
                [ Rresult.R.get_ok @@ Colombe_emile.to_forward_path anil ]
                stream in
  match Unix_scheduler.prj fiber with
  | Error err -> Fmt.failwith "Got an error: %a" Sendmail.pp_error err
  | Ok _ -> is_empty ()

let () =
  Alcotest.run "sendmail" [ "mock", [ test_0 ()
                                    ; test_1 ()
                                    ; test_2 ()
                                    ; test_3 ()
                                    ; test_4 ()
                                    ; test_5 ()
                                    ; test_6 ()
                                    ; test_7 ()
                                    ; test_8 () ] ]
