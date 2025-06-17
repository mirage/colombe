open Lwt.Infix
open Colombe

let ( <.> ) f g x = f (g x)
let ( >>? ) = Lwt_result.bind
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt

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
          ( Lwt_io.read_into ic bytes off len >>= function
            | 0 -> Lwt.return `End
            | len -> Lwt.return (`Len len) ));
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
let authenticator = Lazy.from_fun Ca_certs.authenticator

let tls_config user's_tls_config user's_authenticator =
  match user's_tls_config with
  | Some cfg -> Ok cfg
  | None ->
      let ( let* ) = Result.bind in
      let* authenticator =
        match (Lazy.force authenticator, user's_authenticator) with
        | Ok authenticator, None -> Ok authenticator
        | _, Some authenticator -> Ok authenticator
        | (Error _ as err), None -> err in
      Tls.Config.client ~authenticator ()

let resolve host ?port service =
  Lwt_unix.getprotobyname "tcp" >>= fun tcp ->
  Lwt_unix.getaddrinfo host service Unix.[ AI_PROTOCOL tcp.Unix.p_proto ]
  >>= fun result ->
  match (result, port) with
  | [], None ->
      Lwt.return
        (error_msgf
           "Service %S is not recognized by your system or the host %s is \
            unreachable"
           service host)
  | [], Some port -> (
      Lwt_unix.gethostbyname host >>= function
      | { Unix.h_addr_list = [||]; _ } ->
          Lwt.return (error_msgf "Host %s unreachable" host)
      | { Unix.h_addr_list; _ } ->
          Lwt.return_ok (Unix.ADDR_INET (h_addr_list.(0), port)))
  | ai :: _, _ ->
  match (port, ai.ai_addr) with
  | Some port, Unix.ADDR_INET (inet_addr, _) ->
      Lwt.return_ok (Unix.ADDR_INET (inet_addr, port))
  | _ -> Lwt.return_ok ai.ai_addr

let pp_addr ppf = function
  | Unix.ADDR_INET (inet_addr, port) ->
      Fmt.pf ppf "%s:%d" (Unix.string_of_inet_addr inet_addr) port
  | Unix.ADDR_UNIX str -> Fmt.pf ppf "<%s>" str

let connect socket addr =
  Lwt.pick
    [
      Lwt_unix.sleep 5.0 >|= Fun.const `Timeout;
      Lwt_unix.connect socket addr >|= Fun.const `Connected;
    ]
  >>= function
  | `Timeout -> Lwt.return (error_msgf "Connection to %a timeout" pp_addr addr)
  | `Connected -> Lwt.return_ok ()

let submit ?encoder ?decoder ?queue ~destination ?port ~domain
    ?cfg:user's_tls_config ?authenticator:user's_authenticator ?authentication
    sender recipients mail =
  let mail () = Lwt_scheduler.inj (mail ()) in
  Lwt.return (tls_config user's_tls_config user's_authenticator)
  >>? fun tls_cfg ->
  let protocol =
    match port with
    | Some 587 -> `With_starttls tls_cfg
    | Some _ | None -> `With_tls tls_cfg in
  match protocol with
  | `With_starttls tls ->
      (match (destination, port) with
      | `Ipaddr ipaddr, Some port ->
          Lwt.return_ok (Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ipaddr, port))
      | `Ipaddr ipaddr, None ->
          Lwt.return_ok (Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ipaddr, 587))
      | `Domain_name domain_name, port ->
          resolve (Domain_name.to_string domain_name) ?port "submission")
      >>? fun addr ->
      let socket =
        Lwt_unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
      connect socket addr >>? fun () ->
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
  | `With_tls tls ->
      (match (destination, port) with
      | `Ipaddr ipaddr, Some port ->
          let addr = Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ipaddr, port) in
          let socket =
            Lwt_unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0
          in
          connect socket addr >>? fun () ->
          Tls_lwt.Unix.client_of_fd tls socket
          >|= Tls_lwt.of_t ~close:(fun () -> Lwt_unix.close socket)
          >|= Result.ok
      | `Ipaddr ipaddr, None ->
          let addr = Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ipaddr, 465) in
          let socket =
            Lwt_unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0
          in
          connect socket addr >>? fun () ->
          Tls_lwt.Unix.client_of_fd tls socket
          >|= Tls_lwt.of_t ~close:(fun () -> Lwt_unix.close socket)
          >|= Result.ok
      | `Domain_name domain_name, port ->
          let port = Option.value ~default:465 port in
          Tls_lwt.connect_ext tls (Domain_name.to_string domain_name, port)
          >|= Result.ok)
      >>? fun (ic, oc) ->
      let ctx = Colombe.State.Context.make ?encoder ?decoder () in
      Sendmail.sendmail lwt rdwr { ic; oc } ctx ~domain ?authentication sender
        recipients mail
      |> Lwt_scheduler.prj
      >|= open_sendmail_error
      >|= ( function Error err -> Error (err :> error) | Ok value -> Ok value )
      >|= open_error

let sendmail ?encoder ?decoder ?queue ~destination ?port ~domain
    ?cfg:user's_tls_config ?authenticator:user's_authenticator ?authentication
    sender recipients mail =
  (match (destination, port) with
  | `Ipaddr ipaddr, Some port ->
      Lwt.return_ok (Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ipaddr, port))
  | `Ipaddr ipaddr, None ->
      Lwt.return_ok (Unix.ADDR_INET (Ipaddr_unix.to_inet_addr ipaddr, 25))
  | `Domain_name domain_name, port ->
      resolve (Domain_name.to_string domain_name) ?port "smtp")
  >>? fun addr ->
  let socket =
    Lwt_unix.socket (Unix.domain_of_sockaddr addr) Unix.SOCK_STREAM 0 in
  connect socket addr >>? fun () ->
  let mail () = Lwt_scheduler.inj (mail ()) in
  let ic = Lwt_io.of_fd ~mode:Lwt_io.Input socket in
  let oc = Lwt_io.of_fd ~mode:Lwt_io.Output socket in
  Lwt.return (tls_config user's_tls_config user's_authenticator)
  >>? fun tls_cfg ->
  let ctx =
    Sendmail_with_starttls.Context_with_tls.make ?encoder ?decoder ?queue ()
  in
  Sendmail_with_starttls.sendmail lwt rdwr { ic; oc } ctx tls_cfg
    ?authentication ~domain sender recipients mail
  |> Lwt_scheduler.prj
  >|= open_sendmail_with_starttls_error
