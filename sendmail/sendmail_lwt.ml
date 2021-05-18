open Lwt.Infix
open Colombe

let ( <.> ) f g x = f (g x)

module Lwt_scheduler = Sigs.Make (Lwt)

let lwt_bind x f =
  let open Lwt.Infix in
  let open Lwt_scheduler in
  inj (prj x >>= (prj <.> f))

let lwt =
  { Sigs.bind = lwt_bind; return = (fun x -> Lwt_scheduler.inj (Lwt.return x)) }

type flow = { ic : Lwt_io.input_channel; oc : Lwt_io.output_channel }

let rdwr =
  {
    Sigs.rd =
      (fun { ic; _ } bytes off len ->
        let res = Lwt_io.read_into ic bytes off len in
        Lwt_scheduler.inj res);
    wr =
      (fun { oc; _ } bytes off len ->
        let res =
          Lwt_io.write_from_exactly oc (Bytes.unsafe_of_string bytes) off len
        in
        Lwt_scheduler.inj res);
  }

type error = Sendmail.error

let sendmail ~hostname ?(port = 465) ~domain ~authenticator ?authentication
    sender recipients mail =
  let hostname = Domain_name.to_string hostname in
  let ctx = Colombe.State.Context.make () in
  let mail () = Lwt_scheduler.inj (mail ()) in
  Tls_lwt.connect authenticator (hostname, port) >>= fun (ic, oc) ->
  Sendmail.sendmail lwt rdwr { ic; oc } ctx ~domain ?authentication sender
    recipients mail
  |> Lwt_scheduler.prj

let failf fmt = Fmt.kstrf (fun err -> Lwt.fail (Failure err)) fmt

let resolve host =
  Lwt_unix.getprotobyname "tcp" >>= fun tcp ->
  Lwt_unix.getaddrinfo host "submission" Unix.[ AI_PROTOCOL tcp.Unix.p_proto ]
  >>= function
  | [] -> failf "Submission service is not recognized by your system"
  | ai :: _ -> Lwt.return ai.ai_addr

let sendmail_with_starttls ~hostname ?port ~domain ~authenticator
    ?authentication sender recipients mail =
  resolve (Domain_name.to_string hostname) >>= fun addr ->
  let addr =
    match (addr, port) with
    | Unix.ADDR_INET (inet_addr, _), Some port ->
        Unix.ADDR_INET (inet_addr, port)
    | _ -> addr in
  let socket =
    Lwt_unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
  Lwt_unix.connect socket addr >>= fun () ->
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input socket in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output socket in
  let tls = Tls.Config.client ~authenticator () in
  let ctx = Sendmail_with_starttls.Context_with_tls.make () in
  let mail () = Lwt_scheduler.inj (mail ()) in
  Lwt_scheduler.prj
    (Sendmail_with_starttls.sendmail lwt rdwr { ic; oc } ctx tls ?authentication
       ~domain sender recipients mail)
