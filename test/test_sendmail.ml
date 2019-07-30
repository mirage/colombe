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
      if len = String.length x then inputs := r else inputs := (String.sub x len (String.length x - len)) :: r ;
      Unix_scheduler.inj len in
  let rec write () bytes off len =
    match !outputs with
    | [] ->
      Fmt.failwith "Unexpected output: %S" (String.sub bytes off len)
    | x :: r ->
      let max = len in
      let len = min (String.length x) len in
      if String.sub x 0 len <> String.sub bytes off len
      then Fmt.failwith "Expected %S, have %S" (String.sub x 0 len) (String.sub bytes off len) ;
      if String.length x = len then outputs := r else outputs := (String.sub x len (String.length x - len)) :: r ;
      if len < max then write () bytes (off + len) (max - len) else Unix_scheduler.inj () in
  { Colombe.Sigs.rd= read; wr= write; },
  (fun () -> match !inputs, !outputs with
     | [], [] -> ()
     | r, w -> Fmt.failwith "inputs or ouputs are not empty: @[<hov>%a@] and @[<hov>%a@]"
                 Fmt.(Dump.list string) r Fmt.(Dump.list string) w)

let test_0 () =
  Alcotest.test_case "usual without authentication" `Quick @@ fun () ->
  let ctx = Colombe.State.make_ctx () in
  let state = Sendmail.make_state
      ~encoding:Mime.bit8 (* default value *)
      ~domain:(Colombe.Domain.Domain [ "gmail"; "com" ])
      ~from:(Rresult.R.get_ok @@ Colombe_mrmime.to_reverse_path romain_calascibetta)
      ~recipients:[ Rresult.R.get_ok @@ Colombe_mrmime.to_forward_path anil ]
      None (fun () -> None) in
  let state = Sendmail.make state in
  let rdwr, is_empty =
    rdwr_from_flows
      [ "220 smtp.gmail.com ESTMP - gsmtp"
      ; "250-smtp.gmail.com at your service, [8.8.8.8]"
      ; "250-SIZE 0"
      ; "250-8BITMIME"
      ; "250-AUTH LOGIN PLAIN"
      ; "250-ENHANCEDSTATUSCODES"
      ; "250 CHUNKING"
      ; "250 <romain.calascibetta@gmail.com> as sender"
      ; "250 <anil@recoil.org> as recipient"
      ; "354 "
      ; "250 Sended!"
      ; "221 Closing connection." ]
      [ "EHLO gmail.com"
      ; "MAIL FROM:<romain.calascibetta@gmail.com> BODY=8BITMIME"
      ; "RCPT TO:<anil@recoil.org>"
      ; "DATA"
      ; "."
      ; "QUIT" ] in
  let fiber = Sendmail.run unix rdwr () state ctx in
  match Unix_scheduler.prj fiber with
  | Error err -> Fmt.failwith "Got an error: %a" Sendmail.pp_error err
  | Ok _ -> is_empty ()

let test_1 () =
  Alcotest.test_case "usual with authentication" `Quick @@ fun () ->
  let ctx = Colombe.State.make_ctx () in
  let auth = Auth.make ~username:"romain" "foobar" in
  let state = Sendmail.make_state
      ~encoding:Mime.bit8
      ~domain:(Colombe.Domain.Domain [ "gmail"; "com" ])
      ~from:(Rresult.R.get_ok @@ Colombe_mrmime.to_reverse_path romain_calascibetta)
      ~recipients:[ Rresult.R.get_ok @@ Colombe_mrmime.to_forward_path anil ]
      (Some auth) (fun () -> None) in
  let state = Sendmail.make state in
  let rdwr, is_empty =
    rdwr_from_flows
      [ "220 smtp.gmail.com ESTMP - gsmtp"
      ; "250-smtp.gmail.com at your service, [8.8.8.8]"
      ; "250-SIZE 0"
      ; "250-8BITMIME"
      ; "250-AUTH LOGIN PLAIN"
      ; "250-ENHANCEDSTATUSCODES"
      ; "250 CHUNKING"
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
  let fiber = Sendmail.run unix rdwr () state ctx in
  match Unix_scheduler.prj fiber with
  | Error err -> Fmt.failwith "Got an error: %a" Sendmail.pp_error err
  | Ok _ -> is_empty ()

let test_2 () =
  Alcotest.test_case "bad authentication" `Quick @@ fun () ->
  let ctx = Colombe.State.make_ctx () in
  let auth = Auth.make ~username:"romain" "foobar" in
  let state = Sendmail.make_state
      ~encoding:Mime.bit8
      ~domain:(Colombe.Domain.Domain [ "gmail"; "com" ])
      ~from:(Rresult.R.get_ok @@ Colombe_mrmime.to_reverse_path romain_calascibetta)
      ~recipients:[ Rresult.R.get_ok @@ Colombe_mrmime.to_forward_path anil ]
      (Some auth) (fun () -> None) in
  let state = Sendmail.make state in
  let rdwr, is_empty =
    rdwr_from_flows
      [ "220 smtp.gmail.com ESMTP - gsmtp"
      ; "250-smtp.gmail.com at your service, [8.8.8.8]"
      ; "250-SIZE 0"
      ; "250-8BITMIME"
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
  let fiber = Sendmail.run unix rdwr () state ctx in
  match Unix_scheduler.prj fiber with
  | Error _ -> is_empty () (* XXX(dinosaure): check if the error is [Auth_error]? *)
  | Ok _ -> Fmt.failwith "Should fail with [Auth_error]"

let test_3 () =
  Alcotest.test_case "PLAIN mechanism unavailable" `Quick @@ fun () ->
  let ctx = Colombe.State.make_ctx () in
  let auth = Auth.make ~username:"romain" "foobar" in
  let state = Sendmail.make_state
      ~encoding:Mime.bit8
      ~domain:(Colombe.Domain.Domain [ "gmail"; "com" ])
      ~from:(Rresult.R.get_ok @@ Colombe_mrmime.to_reverse_path romain_calascibetta)
      ~recipients:[ Rresult.R.get_ok @@ Colombe_mrmime.to_forward_path anil ]
      (Some auth) (fun () -> None) in
  let state = Sendmail.make state in
  let rdwr, is_empty =
    rdwr_from_flows
      [ "220 smtp.gmail.com ESMTP - gsmtp"
      ; "250-smtp.gmail.com at your service, [8.8.8.8]"
      ; "250 AUTH LOGIN"
      ; "221 Closing connection" ]
      [ "EHLO gmail.com"
      ; "QUIT" ] in
  let fiber = Sendmail.run unix rdwr () state ctx in
  match Unix_scheduler.prj fiber with
  | Error _ -> is_empty () (* XXX(dinosaure): check if the error is [Auth_error]? *)
  | Ok _ -> Fmt.failwith "Should fail with [Auth_missing_mechanism]"

let () =
  Alcotest.run "sendmail" [ "mock", [ test_0 ()
                                    ; test_1 ()
                                    ; test_2 ()
                                    ; test_3 () ] ]
