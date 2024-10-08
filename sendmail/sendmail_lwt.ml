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
        let open Lwt.Infix in
        Lwt_scheduler.inj
          (Lwt_io.read_into ic bytes off len >>= function
           | 0 -> Lwt.return `End
           | len -> Lwt.return (`Len len)));
    wr =
      (fun { oc; _ } bytes off len ->
        let res =
          Lwt_io.write_from_exactly oc (Bytes.unsafe_of_string bytes) off len
        in
        Lwt_scheduler.inj res);
  }

type destination =
  [ `Ipaddr of Ipaddr.t | `Domain_name of [ `host ] Domain_name.t ]

type error = [ `Msg of string | Sendmail_with_starttls.error ]

let open_sendmail_error = function
  | Ok _ as v -> v
  | Error (#Sendmail.error as err) -> Error err

let open_sendmail_with_starttls_error = function
  | Ok _ as v -> v
  | Error (#Sendmail_with_starttls.error as err) -> Error err

let open_error = function Ok _ as v -> v | Error (#error as err) -> Error err
let failwith_error_msg = function Ok v -> v | Error (`Msg msg) -> failwith msg
let failf fmt = Fmt.kstr (fun err -> Lwt.fail (Failure err)) fmt

let resolve host ?port service =
  Lwt_unix.getprotobyname "tcp" >>= fun tcp ->
  Lwt_unix.getaddrinfo host service Unix.[ AI_PROTOCOL tcp.Unix.p_proto ]
  >>= fun result ->
  match (result, port) with
  | [], None ->
      failf
        "Service %S is not recognized by your system or the host %s is \
         unreachable"
        service host
  | [], Some port -> (
      Lwt_unix.gethostbyname host >>= function
      | { Unix.h_addr_list = [||]; _ } -> failf "Host %s unreachable" host
      | { Unix.h_addr_list; _ } ->
          Lwt.return (Unix.ADDR_INET (h_addr_list.(0), port)))
  | ai :: _, _ ->
  match (port, ai.ai_addr) with
  | Some port, Unix.ADDR_INET (inet_addr, _) ->
      Lwt.return (Unix.ADDR_INET (inet_addr, port))
  | _ -> Lwt.return ai.ai_addr

let submit ?encoder ?decoder ?queue ~destination ?port ~domain ?authenticator
    ?authentication sender recipients mail =
  let mail () = Lwt_scheduler.inj (mail ()) in
  let protocol =
    match (port, authenticator) with
    | Some 587, Some authenticator ->
        `With_starttls
          (failwith_error_msg (Tls.Config.client ~authenticator ()))
    | (Some 587 | None), None -> `Clear
    | (Some _ | None), Some authenticator ->
        `With_tls (failwith_error_msg (Tls.Config.client ~authenticator ()))
    | Some _, None -> `Clear in
  match (protocol, authentication) with
  | `With_starttls tls, _ ->
      (match (destination, port) with
      | `Ipaddr ipaddr, Some port ->
          Lwt.return (Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ipaddr, port))
      | `Ipaddr ipaddr, None ->
          Lwt.return (Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ipaddr, 587))
      | `Domain_name domain_name, port ->
          resolve (Domain_name.to_string domain_name) ?port "submission")
      >>= fun addr ->
      let socket =
        Lwt_unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
      Lwt_unix.connect socket addr >>= fun () ->
      let ic = Lwt_io.of_fd ~mode:Lwt_io.Input socket in
      let oc = Lwt_io.of_fd ~mode:Lwt_io.Output socket in
      let ctx =
        Sendmail_with_starttls.Context_with_tls.make ?encoder ?decoder ?queue ()
      in
      Sendmail_with_starttls.sendmail lwt rdwr { ic; oc } ctx tls
        ?authentication ~domain sender recipients mail
      |> Lwt_scheduler.prj
      >|= open_sendmail_with_starttls_error
      >|= open_error
  | `Clear, Some _ -> Lwt.return_error `Encryption_required
  | `Clear, None ->
      (match (destination, port) with
      | `Ipaddr ipaddr, Some port ->
          Lwt.return (Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ipaddr, port))
      | `Ipaddr ipaddr, None ->
          Lwt.return (Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ipaddr, 587))
      | `Domain_name domain_name, port ->
          resolve (Domain_name.to_string domain_name) ?port "submission")
      >>= fun addr ->
      let socket =
        Lwt_unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
      Lwt_unix.connect socket addr >>= fun () ->
      let ic = Lwt_io.of_fd ~mode:Lwt_io.Input socket in
      let oc = Lwt_io.of_fd ~mode:Lwt_io.Output socket in
      let ctx = Colombe.State.Context.make ?encoder ?decoder () in
      Sendmail.sendmail lwt rdwr { ic; oc } ctx ~domain ?authentication sender
        recipients mail
      |> Lwt_scheduler.prj
      >|= open_sendmail_error
      >|= (function Error err -> Error (err :> error) | Ok value -> Ok value)
      >|= open_error
  | `With_tls tls, _ ->
      (match (destination, port) with
      | `Ipaddr ipaddr, Some port ->
          let addr = Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ipaddr, port) in
          let socket =
            Lwt_unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0
          in
          Lwt_unix.connect socket addr >>= fun () ->
          Tls_lwt.Unix.client_of_fd tls socket
          >|= Tls_lwt.of_t ~close:(fun () -> Lwt_unix.close socket)
      | `Ipaddr ipaddr, None ->
          let addr = Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ipaddr, 465) in
          let socket =
            Lwt_unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0
          in
          Lwt_unix.connect socket addr >>= fun () ->
          Tls_lwt.Unix.client_of_fd tls socket
          >|= Tls_lwt.of_t ~close:(fun () -> Lwt_unix.close socket)
      | `Domain_name domain_name, port ->
          let port = Option.value ~default:465 port in
          Tls_lwt.connect_ext tls (Domain_name.to_string domain_name, port))
      >>= fun (ic, oc) ->
      let ctx = Colombe.State.Context.make ?encoder ?decoder () in
      Sendmail.sendmail lwt rdwr { ic; oc } ctx ~domain ?authentication sender
        recipients mail
      |> Lwt_scheduler.prj
      >|= open_sendmail_error
      >|= (function Error err -> Error (err :> error) | Ok value -> Ok value)
      >|= open_error

let sendmail ?encoder ?decoder ?queue ~destination ?port ~domain ?authenticator
    ?authentication sender recipients mail =
  (match (destination, port) with
  | `Ipaddr ipaddr, Some port ->
      Lwt.return (Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ipaddr, port))
  | `Ipaddr ipaddr, None ->
      Lwt.return (Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ipaddr, 25))
  | `Domain_name domain_name, port ->
      resolve (Domain_name.to_string domain_name) ?port "smtp")
  >>= fun addr ->
  let socket =
    Lwt_unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
  Lwt_unix.connect socket addr >>= fun () ->
  let mail () = Lwt_scheduler.inj (mail ()) in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input socket in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output socket in
  match
    Option.map
      (fun authenticator -> Tls.Config.client ~authenticator ())
      authenticator
  with
  | Some (Error _ as err) -> Lwt.return err
  | Some (Ok tls) ->
      let ctx =
        Sendmail_with_starttls.Context_with_tls.make ?encoder ?decoder ?queue ()
      in
      Sendmail_with_starttls.sendmail lwt rdwr { ic; oc } ctx tls
        ?authentication ~domain sender recipients mail
      |> Lwt_scheduler.prj
      >|= open_sendmail_with_starttls_error
  | None ->
      let ctx = Colombe.State.Context.make ?encoder ?decoder () in
      Sendmail.sendmail lwt rdwr { ic; oc } ctx ~domain ?authentication sender
        recipients mail
      |> Lwt_scheduler.prj
      >|= open_sendmail_error
      >|= (function Error err -> Error (err :> error) | Ok value -> Ok value)
      >|= open_error
