module Option = struct
  let get_exn = function
    | Some x -> x
    | None -> Fmt.invalid_arg "Option.get_exn"
end

let romain_calascibetta =
  let open Mrmime in
  let open Mailbox in
  Local.[ w "romain"; w "calascibetta" ] @ Domain.(domain, [ a "gmail"; a "com" ])

let thomas_gazagnaire =
  let open Mrmime in
  let open Mailbox in
  Local.[ w "thomas" ] @ Domain.(domain, [ a "tarides"; a "com" ])

let subject =
  let open Mrmime in
  let open Unstructured in
  [ v "A"; sp 1; v "Simple"; sp 1; v "Mail" ]

let date =
  let now = Ptime_clock.now () in
  Rresult.R.error_msg_to_invalid_arg Mrmime.(Date.of_ptime ~zone:Date.Zone.GMT now)

let content_type =
  let open Mrmime in
  let ty = `Text in
  let subty = Content_type.Subtype.v `Text "plain" in
  let paramters = Content_type.Parameters.(of_list
      [ k "charset", v "utf-8" ]) in
  Content_type.make ty subty paramters

let mime =
  let open Mrmime in
  Content.make ~encoding:`Quoted_printable content_type

let header =
  let open Mrmime in
  let open Header in
  Field.(Sender $ romain_calascibetta)
  & Field.(Date $ date)
  & Field.(To $ Address.[ mailbox romain_calascibetta ])
  & Field.(Subject $ subject)
  & Field.(Content $ mime)
  & empty

type 'a stream = unit -> 'a option

let header_stream =
  let bf = Bytes.create 4096 in
  (* XXX(dinosaure): [mrmime] should not produce bigger than [4096]. *)

  let stream = Mrmime.Header.to_stream header in
  (fun () -> match stream () with
     | Some s ->
       let ln = String.length s in
       Bytes.blit_string s 0 bf 0 ln ;
       Some (bf, 0, ln)
     | None -> None)

let quoted_printable_stream_of_ic ic : (bytes * int * int) stream =
  let io_buffer_size = 4096 in
  let bf = Bytes.create io_buffer_size in
  let encoder = Pecu.encoder `Manual in
  let close = ref false in

  let rec of_ic () =
    if !close then None
    else
      let v = match input_char ic with
        | '\n' -> `Line_break
        | chr -> `Char chr
        | exception End_of_file -> `End in
      match Pecu.encode encoder v with
      | `Ok -> of_ic ()
      | `Partial ->
        let rem = Pecu.dst_rem encoder in
        let res = Some (bf, 0, io_buffer_size - rem) in
        Pecu.dst encoder bf 0 io_buffer_size ;
        close := if v = `End then true else false ;
        res in
  Pecu.dst encoder bf 0 io_buffer_size ; of_ic

let concat_stream s0 s1 =
  let c = ref s0 in
  let rec go () = match !c () with
    | Some x -> Some x
    | None ->
      if !c == s0 then ( c := s1 ; go () ) else None in
  go

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

let run () =
  let open Lwt.Infix in

  Sendmail_lwt.run
    ~logger:(Logs.src_log (Logs.Src.create ~doc:"sendmail" "sendmail"))
    ~hostname:(Domain_name.of_string_exn "smtp.gmail.com") ~port:465
    ~domain:Colombe.Domain.(v domain [ a "gmail"; a "com" ])
    ~authenticator:X509.Authenticator.null
    ~from:(to_reverse_path romain_calascibetta)
    ~recipients:[ to_forward_path romain_calascibetta ]
    (Some { Auth.Client.mechanism= PLAIN; q= `q0; username= "romain.calascibetta"; password= ""; })
    (concat_stream header_stream (quoted_printable_stream_of_ic stdin))
  >>= function
  | Ok () -> Lwt.return ()
  | Error err ->
    Fmt.epr "Error: %a.\n%!" Sendmail_lwt.pp_error err ;
    Lwt.return ()

let () = Lwt_main.run (run ())
