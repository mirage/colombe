open Colombe

let ( <.> ) f g x = f (g x)
let ( $ ) f g x = match f x with Ok x -> g x | Error _ as err -> err

module Miou_scheduler = Sigs.Make (struct
  type 'a t = 'a
end)

let miou =
  let open Miou_scheduler in
  { Sigs.bind = (fun x f -> (f <.> prj) x); return = inj }

type error = [ `Msg of string | Sendmail_with_starttls.error ]

let open_sendmail_error = function
  | Ok _ as v -> v
  | Error (#Sendmail.error as err) -> Error err

let open_sendmail_with_starttls_error = function
  | Ok _ as v -> v
  | Error (#Sendmail_with_starttls.error as err) -> Error err

let open_error = function Ok _ as v -> v | Error (#error as err) -> Error err

let tcp =
  let open Miou_scheduler in
  let rd flow buf off len =
    match Miou_unix.read flow buf ~off ~len with
    | 0 -> inj `End
    | len -> inj (`Len len)
  and wr flow buf off len = inj (Miou_unix.write flow buf ~off ~len) in
  { Colombe.Sigs.rd; wr }

let tls =
  let open Miou_scheduler in
  let rd flow buf off len =
    match Tls_miou_unix.read flow buf ~off ~len with
    | 0 -> inj `End
    | len -> inj (`Len len)
  and wr flow buf off len = inj (Tls_miou_unix.write flow buf ~off ~len) in
  { Colombe.Sigs.rd; wr }

let authenticator :
    (X509.Authenticator.t, [ `Msg of string ]) result Miou.Lazy.t =
  Miou.Lazy.from_fun Ca_certs.authenticator

let tls_config user's_tls_config user's_authenticator =
  match user's_tls_config with
  | Some cfg -> Ok cfg
  | None ->
      let ( let* ) = Result.bind in
      let* authenticator =
        match (Miou.Lazy.force authenticator, user's_authenticator) with
        | Ok authenticator, None -> Ok authenticator
        | _, Some authenticator -> Ok authenticator
        | Error (`Msg msg), None -> Error (`Msg msg) in
      Tls.Config.client ~authenticator ()

let submit ?encoder ?decoder ?queue he ~destination ?port ~domain
    ?cfg:user's_tls_config ?authenticator:user's_authenticator ?authentication
    sender recipients mail =
  let ( let* ) = Result.bind in
  let ports = match port with None -> [ 465; 587 ] | Some port -> [ port ] in
  let mail () = Miou_scheduler.inj (mail ()) in
  let* tls_cfg = tls_config user's_tls_config user's_authenticator in
  let* (_, port), socket =
    Happy_eyeballs_miou_unix.connect he destination ports in
  let finally () = Miou_unix.close socket in
  Fun.protect ~finally @@ fun () ->
  let protocol =
    if port = 587 then `With_starttls tls_cfg else `With_tls tls_cfg in
  match protocol with
  | `With_starttls tls ->
      let ctx =
        Sendmail_with_starttls.Context_with_tls.make ?encoder ?decoder ?queue ()
      in
      Sendmail_with_starttls.sendmail miou tcp socket ctx tls ?authentication
        ~domain sender recipients mail
      |> Miou_scheduler.prj
      |> open_sendmail_with_starttls_error
      |> open_error
  | `With_tls cfg ->
      let host =
        match Ipaddr.of_string destination with
        | Ok _ -> None
        | Error _ ->
        match Domain_name.(of_string $ host) destination with
        | Ok host -> Some host
        | Error _ -> None in
      let socket_tls = Tls_miou_unix.client_of_fd cfg ?host socket in
      let ctx = Colombe.State.Context.make ?encoder ?decoder () in
      Sendmail.sendmail miou tls socket_tls ctx ~domain ?authentication sender
        recipients mail
      |> Miou_scheduler.prj
      |> open_sendmail_error
      |> (function Error err -> Error (err :> error) | Ok value -> Ok value)
      |> open_error

let sendmail ?encoder ?decoder ?queue he ~destination ?port ~domain
    ?cfg:user's_tls_config ?authenticator:user's_authenticator ?authentication
    sender recipients mail =
  let ( let* ) = Result.bind in
  let ports = match port with None -> [ 25 ] | Some port -> [ port ] in
  let mail () = Miou_scheduler.inj (mail ()) in
  let* tls_cfg = tls_config user's_tls_config user's_authenticator in
  let* _, socket = Happy_eyeballs_miou_unix.connect he destination ports in
  let finally () = Miou_unix.close socket in
  Fun.protect ~finally @@ fun () ->
  let ctx =
    Sendmail_with_starttls.Context_with_tls.make ?encoder ?decoder ?queue ()
  in
  Sendmail_with_starttls.sendmail miou tcp socket ctx tls_cfg ?authentication
    ~domain sender recipients mail
  |> Miou_scheduler.prj
  |> open_sendmail_with_starttls_error
