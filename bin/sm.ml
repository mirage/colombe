let () = Printexc.record_backtrace true

module Option = struct
  let get_exn = function
    | Some x -> x
    | None -> Fmt.invalid_arg "Option.get_exn"
end

let to_path mailbox =
  let domain = match mailbox.Mrmime.Mailbox.domain with
    | `Domain domain, _ -> Colombe.Domain.unsafe_domain_of_list_exn domain
    | `Addr (Mrmime.Mailbox.IPv4 x), _ -> Colombe.Domain.(v ipv4 x)
    | `Addr (Mrmime.Mailbox.IPv6 x), _ -> Colombe.Domain.(v ipv6 x)
    | `Addr (Mrmime.Mailbox.Ext (k, v)), _ ->
      Fmt.invalid_arg "Invalid domain (extension): %S:%S" k v
    | `Literal v, _ ->
      Fmt.invalid_arg "Invalid domain (literal): %S" v in
  let local =
    `Dot_string (List.map
                   (function
                     | `Atom x -> x
                     | `String _ -> Fmt.invalid_arg "Invalid atom of local-part")
                   mailbox.Mrmime.Mailbox.local) in
  { Colombe.Path.local
  ; domain
  ; rest= [] }

let to_reverse_path x = Some (to_path x)
let to_forward_path x = Colombe.Forward_path.Forward_path (to_path x)

let pp_header = Logs_fmt.pp_header
let () = Fmt_tty.setup_std_outputs ~style_renderer:`Ansi_tty ~utf_8:true ()
let () = Logs.set_reporter @@ Logs_fmt.reporter ~pp_header ~app:Fmt.stdout ~dst:Fmt.stderr ()

let run hostname port domain tls_authenticator from recipients smtp_authenticator stream =
  let open Lwt.Infix in

  Sendmail_lwt.run
    ~logger:(Logs.src_log (Logs.Src.create ~doc:"sendmail" "sendmail"))
    ~hostname ?port
    ~domain
    ~authenticator:tls_authenticator
    ~from ~recipients
    smtp_authenticator
    stream
  >>= function
  | Ok () -> Lwt.return ()
  | Error err ->
    Fmt.epr "Error: %a.\n%!" Sendmail_lwt.pp_error err ;
    Lwt.return ()

module Unix_scheduler = Colombe.Sigs.Make(struct type 'a t = 'a end)

let unix =
  { Colombe.Sigs.bind= (fun x f -> f (Unix_scheduler.prj x))
  ; return= (fun x -> Unix_scheduler.inj x) }

type flow = Unix.file_descr (* socket *)

let rdwr =
  { Colombe.Sigs.rd= (fun socket bytes off len ->
        let res = Unix.read socket bytes off len in
        Unix_scheduler.inj res)
  ; wr= (fun socket bytes off len ->
        let _ = Unix.write socket bytes off len in
        Unix_scheduler.inj ()) }

module M = struct
  open Mrmime

  let romain =
    let open Mailbox in
    Local.[ w "romain"; w "calascibetta" ] @ Domain.(domain, [ a "gmail"; a "com" ])

  let thomas =
    let open Mailbox in
    Local.[ w "thomas" ] @ Domain.(domain, [ a "gazagnaire"; a "org" ])

  let hannes =
    let open Mailbox in
    Local.[ w "hannes" ] @ Domain.(domain, [ a "mehnert"; a "org" ])

  let anil =
    let open Mailbox in
    Local.[ w "anil" ] @ Domain.(domain, [ a "recoil"; a "org" ])

  let gemma =
    let open Mailbox in
    Local.[ w "gemma"; w "t"; w "gordon" ] @ Domain.(domain, [ a "gmail"; a "com" ])

  let subject =
    let open Unstructured in
    [ v "A"; sp 1; v "Mail"; sp 1; v "with"; sp 1; v "Attachment!" ]

  let now =
    let open Date in
    match Date.of_ptime ~zone:Zone.gmt (Ptime_clock.now ()) with
    | Ok v -> v
    | Error (`Msg err) -> failwith err

  let c =
    let content_type =
      let open Content_type in
      make `Multipart (Subtype.iana_exn `Multipart "mixed")
        Parameters.(of_list [ key_exn "boundary", value_exn "my-special-boundary" ]) in
    Content.make content_type

  let stream_of_ic ic =
    let tmp = Bytes.create 4096 in
    let go () = match input ic tmp 0 4096 with
      | 0 -> None
      | len ->
        Some (Bytes.unsafe_to_string tmp, 0, len)
      | exception End_of_file -> None in
    go

  let stream_of_file path =
    let tmp = Bytes.create 4096 in
    let closed = ref false in
    let ic = open_in path in

    let go () = match input ic tmp 0 4096 with
      | 0 -> if not !closed then ( closed := true ; close_in ic ) ; None
      | len -> Some (Bytes.unsafe_to_string tmp, 0, len)
      | exception End_of_file ->
        if not !closed then ( closed := true ; close_in ic ) ; None in
    go

  let part_0 =
    let content =
      let content_type =
        let open Content_type in
        make `Text (Subtype.iana_exn `Text "plain")
          Parameters.(of_list [ key_exn "charset", value_exn "utf-8" ]) in
      Content.make ~encoding:`Quoted_printable content_type in
    Mt.part ~content (stream_of_ic stdin)

  let part_1 =
    let content =
      let content_type =
        let open Content_type in
        make `Image (Subtype.iana_exn `Image "png") Parameters.empty in
      Content.make ~encoding:`Base64 content_type in
    Mt.part ~content (stream_of_file Sys.argv.(1))

  let multipart = Mt.multipart ~content:c [ part_0; part_1 ]

  let header =
    let open Header in
    Field.(From $ [ romain ])
    & Field.(Date $ now)
    & Field.(To $ Address.[ mailbox thomas; mailbox hannes; mailbox anil; mailbox gemma ])
    & Field.(Cc $ (Address.[ mailbox romain ]))
    & Field.(Sender $ romain)
    & Field.(Subject $ subject)
    & empty

  let mail = Mt.make header Mt.multi multipart
  let stream = Mt.to_stream mail

  let stream =
    let go () = match stream () with
      | Some (buf, off, len) -> Some (Bytes.unsafe_of_string buf, off, len)
      | None -> None in
    go
end

let to_romain = to_forward_path M.romain
let to_thomas = to_forward_path M.thomas
let to_hannes = to_forward_path M.hannes
let to_anil = to_forward_path M.anil
let to_gemma = to_forward_path M.gemma
let of_romain = to_reverse_path M.romain

let run_with_starttls hostname port domain tls_config =
  let ctx = Colombe.State.make_ctx () in
  let auth = Auth.make ~username:"romain.calascibetta" "" in
  let state = Sendmail_tls.make_state ~domain ~from:of_romain ~recipients:[ to_romain
                                                                          ; to_thomas
                                                                          ; to_hannes
                                                                          ; to_anil
                                                                          ; to_gemma ] (Some auth) M.stream tls_config in
  let state = Sendmail_tls.make state in

  let { Unix.h_addr_list; _ } = Unix.gethostbyname hostname in
  let socket = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
  Unix.connect socket (Unix.ADDR_INET (h_addr_list.(0), port)) ;

  let fiber = Sendmail_tls.run unix rdwr socket state ctx in

  match Unix_scheduler.prj fiber with
  | Ok _state -> ()
  | Error err -> Fmt.epr "Error: %a\n%!" Sendmail_tls.pp_error err

let tls_config = Tls.Config.client ~authenticator:X509.Authenticator.null ()
let g () = run_with_starttls "smtp.gmail.com" 587 (Colombe.Domain.Domain [ "gmail"; "com" ]) tls_config
let () =
  let auth = Auth.make ~username:"romain.calascibetta" "" in
  let fiber = run
      (Domain_name.of_string_exn "smtp.gmail.com") (Some 465)
      (Colombe.Domain.Domain [ "gmail"; "com" ])
      X509.Authenticator.null
      of_romain [ to_romain; to_thomas; to_hannes; to_anil; to_gemma ] (Some auth) M.stream in
  Lwt_main.run fiber

(*
let ( <.> ) f g = fun x -> f (g x)

open Cmdliner

let domain_name =
  let parser = Domain_name.of_string in
  let pp = Domain_name.pp in
  Arg.conv ~docv:"<domain-name>" (parser, pp)

let domain =
  let parser = Colombe.Domain.of_string in
  let pp = Colombe.Domain.pp in
  Arg.conv ~docv:"<domain>" (parser, pp)

let mailbox = Emile_cmdliner.mailbox

let date =
  let parser = Ptime.rfc3339_error_to_msg <.> Ptime.of_rfc3339 ~strict:true ~sub:false ~start:0 in
  let pp ppf (t, tz_offset_s, _) = Ptime.pp_rfc3339 ~space:true ?tz_offset_s () ppf t in
  Arg.conv ~docv:"<date>" (parser, pp)

let mechanism =
  let parser x = match String.lowercase_ascii x with
    | "plain" -> Ok Auth.plain
    | _ -> Rresult.R.error_msgf "Invalid authentication mechanism: %s" x in
  let pp ppf Auth.Client.PLAIN = Fmt.string ppf "PLAIN" in
  Arg.conv ~docv:"<SASL mechanism>" (parser, pp)

let existing_file =
  let parser x = match Fpath.of_string x with
    | Ok v -> if Sys.file_exists x then Ok v else Rresult.R.error_msgf "%a does not exist" Fpath.pp v
    | Error _ as err -> err in
  let pp = Fpath.pp in
  Arg.conv ~docv:"<filename>" (parser, pp)

let path =
  let parser = Fpath.of_string in
  let pp = Fpath.pp in
  Arg.conv ~docv:"<path>" (parser, pp)

let directory =
  let parser x = match Fpath.of_string x with
    | Ok v -> if Sys.is_directory x then Ok v else Rresult.R.error_msgf "%a is not a directory" Fpath.pp v
    | Error _ as err -> err in
  let pp = Fpath.pp in
  Arg.conv ~docv:"<directory>" (parser, pp)

let unstructured =
  let parser x = match Mrmime.Unstructured.of_string x with
    | v -> Ok v
    | exception (Failure err) -> Rresult.R.error_msg err in
  let pp = Mrmime.Unstructured.pp in
  Arg.conv ~docv:"<unstructured>" (parser, pp)

let encoding =
  let parser = Mrmime.Content_encoding.of_string in
  let pp = Mrmime.Content_encoding.pp in
  Arg.conv ~docv:"<encoding>" (parser, pp)

let sender =
  let doc = "Email address of the sender" in
  Arg.(required & opt (some mailbox) None & info [ "s"; "sender" ] ~doc)

let date =
  let doc = "Date of the email (rfc3339 format)" in
  Arg.(value & opt (some date) None & info [ "d"; "date" ] ~doc)

let username =
  let doc = "Username to be authenticated" in
  Arg.(value & opt (some string) None & info [ "u"; "username" ] ~doc ~docv:"<username>")

let password =
  let doc = "Password to be authenticated" in
  Arg.(value & opt (some string) None & info [ "p"; "password" ] ~doc ~docv:"<password>")

let mechanism =
  let doc = "SASL mechanism used to be authenticated" in
  Arg.(value & opt mechanism Auth.plain & info [ "m"; "mechanism" ] ~doc)

let encoding =
  let doc = "MIME encoding used into the body of the email" in
  Arg.(value & opt encoding `Quoted_printable & info [ "e"; "encoding" ] ~doc)

let subject =
  let doc = "Subject of the email" in
  Arg.(required & opt (some unstructured) None & info [ "subject" ] ~doc)

let recipients =
  let doc = "Recipients of the email" in
  Arg.(value & pos_left ~rev:true 0 mailbox [] & info [] ~docv:"<recipients>" ~doc)

let hostname =
  let doc = "Hostname of SMTP server" in
  Arg.(required & opt (some domain_name) None & info [ "domain" ] ~doc)

let port =
  let doc = "Port of SMTP server" in
  Arg.(value & opt int 25 & info [ "p"; "port" ] ~doc)

let ca_file =
  let doc = "PEM format file of CA's" in
  Arg.(value & opt (some existing_file) None & info [ "ca-file" ] ~doc)

let ca_path =
  let doc = "PREM format directory of CA's" in
  Arg.(value & opt (some directory) None & info [ "ca-path"] ~doc)
*)
