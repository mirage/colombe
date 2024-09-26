open Lwt.Infix
open Colombe

let ( <.> ) f g x = f (g x)
let ( >>? ) = Lwt_result.bind
let ( $ ) f g x = match f x with Ok x -> g x | Error _ as err -> err
let msgf fmt = Fmt.kstr (fun msg -> `Msg msg) fmt

module Lwt_scheduler = Sigs.Make (Lwt)

let lwt_bind x f =
  let open Lwt.Infix in
  let open Lwt_scheduler in
  inj (prj x >>= (prj <.> f))

let lwt =
  { Sigs.bind = lwt_bind; return = (fun x -> Lwt_scheduler.inj (Lwt.return x)) }

type error = [ `Msg of string | Sendmail_with_starttls.error ]

let open_sendmail_error = function
  | Ok _ as v -> v
  | Error (#Sendmail.error as err) -> Error err

let open_sendmail_with_starttls_error = function
  | Ok _ as v -> v
  | Error (#Sendmail_with_starttls.error as err) -> Error err

let open_error = function Ok _ as v -> v | Error (#error as err) -> Error err
let failwith_error_msg = function Ok v -> v | Error (`Msg msg) -> failwith msg

module Rdwr (Flow : Mirage_flow.S) = struct
  let blit0 src src_off dst dst_off len =
    let dst = Cstruct.of_bigarray ~off:dst_off ~len dst in
    Cstruct.blit src src_off dst 0 len

  let blit1 src src_off dst dst_off len =
    Bigstringaf.blit_to_bytes src ~src_off dst ~dst_off ~len

  let failwith pp = function
    | Ok v -> Lwt.return v
    | Error err -> Lwt.fail (Failure (Fmt.str "%a" pp err))

  type t = {
    queue : (char, Bigarray.int8_unsigned_elt) Ke.Rke.t;
    flow : Flow.flow;
  }

  let make flow = { flow; queue = Ke.Rke.create ~capacity:0x7ff Bigarray.char }

  let recv flow payload p_off p_len =
    if Ke.Rke.is_empty flow.queue
    then (
      Flow.read flow.flow >>= failwith Flow.pp_error >>= function
      | `Eof -> Lwt.return 0
      | `Data res ->
          Ke.Rke.N.push flow.queue ~blit:blit0 ~length:Cstruct.length res ;
          let len = min p_len (Ke.Rke.length flow.queue) in
          Ke.Rke.N.keep_exn flow.queue ~blit:blit1 ~length:Bytes.length
            ~off:p_off ~len payload ;
          Ke.Rke.N.shift_exn flow.queue len ;
          Lwt.return len)
    else
      let len = min p_len (Ke.Rke.length flow.queue) in
      Ke.Rke.N.keep_exn flow.queue ~blit:blit1 ~length:Bytes.length ~off:p_off
        ~len payload ;
      Ke.Rke.N.shift_exn flow.queue len ;
      Lwt.return len

  let send flow payload p_off p_len =
    let cs = Cstruct.of_string payload ~off:p_off ~len:p_len in
    Flow.write flow.flow cs >>= failwith Flow.pp_write_error
end

module Make
    (Socket : Mirage_flow.S)
    (Happy_eyeballs : Happy_eyeballs_mirage.S with type flow = Socket.flow) =
struct
  module TCP = Rdwr (Socket)
  module Socket_tls = Tls_mirage.Make (Socket)
  module TLS = Rdwr (Socket_tls)

  let tcp =
    let open Lwt_scheduler in
    let rd flow buf off len =
      inj
      @@ (TCP.recv flow buf off len >>= function
          | 0 -> Lwt.return `End
          | len -> Lwt.return (`Len len)) in
    {
      Colombe.Sigs.rd;
      Colombe.Sigs.wr =
        (fun flow buf off len -> inj (TCP.send flow buf off len));
    }

  let tls =
    let open Lwt_scheduler in
    let rd flow buf off len =
      inj
      @@ (TLS.recv flow buf off len >>= function
          | 0 -> Lwt.return `End
          | len -> Lwt.return (`Len len)) in
    {
      Colombe.Sigs.rd;
      Colombe.Sigs.wr =
        (fun flow buf off len -> inj (TLS.send flow buf off len));
    }

  let submit ?encoder ?decoder ?queue he ~destination ?port ~domain
      ?authenticator ?authentication sender recipients mail =
    let ports = match port with None -> [ 465; 587 ] | Some port -> [ port ] in
    let mail () = Lwt_scheduler.inj (mail ()) in
    Happy_eyeballs.connect he destination ports >>? fun ((_, port), socket) ->
    let process () =
      let protocol =
        match (authenticator, port) with
        | Some authenticator, 587 ->
            `With_starttls
              (failwith_error_msg (Tls.Config.client ~authenticator ()))
        | Some authenticator, _ ->
            `With_tls (failwith_error_msg (Tls.Config.client ~authenticator ()))
        | None, _ -> `Clear in
      match (protocol, authentication) with
      | `With_starttls tls, _ ->
          let flow = TCP.make socket in
          let ctx =
            Sendmail_with_starttls.Context_with_tls.make ?encoder ?decoder
              ?queue () in
          Sendmail_with_starttls.sendmail lwt tcp flow ctx tls ?authentication
            ~domain sender recipients mail
          |> Lwt_scheduler.prj
          >|= open_sendmail_with_starttls_error
          >|= open_error
      | `Clear, Some _ -> Lwt.return_error `Encryption_required
      | `Clear, None ->
          let flow = TCP.make socket in
          let ctx = Colombe.State.Context.make ?encoder ?decoder () in
          Sendmail.sendmail lwt tcp flow ctx ~domain ?authentication sender
            recipients mail
          |> Lwt_scheduler.prj
          >|= open_sendmail_error
          >|= (function
                | Error err -> Error (err :> error) | Ok value -> Ok value)
          >|= open_error
      | `With_tls cfg, _ ->
          let host =
            match Ipaddr.of_string destination with
            | Ok _ -> None
            | Error _ ->
            match Domain_name.(of_string $ host) destination with
            | Ok host -> Some host
            | Error _ -> None in
          Socket_tls.client_of_flow cfg ?host socket
          >|= Result.map_error (msgf "%a" Socket_tls.pp_write_error)
          >>? fun socket_tls ->
          let flow = TLS.make socket_tls in
          let ctx = Colombe.State.Context.make ?encoder ?decoder () in
          Sendmail.sendmail lwt tls flow ctx ~domain ?authentication sender
            recipients mail
          |> Lwt_scheduler.prj
          >|= open_sendmail_error
          >|= (function
                | Error err -> Error (err :> error) | Ok value -> Ok value)
          >|= open_error in
    Lwt.try_bind process
      (fun result -> Socket.close socket >|= fun () -> result)
      (fun exn ->
        Socket.close socket >>= fun () ->
        match exn with
        | Failure msg -> Lwt.return_error (`Msg msg)
        | exn -> Lwt.fail exn)

  let sendmail ?encoder ?decoder ?queue he ~destination ?port ~domain
      ?authenticator ?authentication sender recipients mail =
    let ports = match port with None -> [ 25 ] | Some port -> [ port ] in
    let mail () = Lwt_scheduler.inj (mail ()) in
    Happy_eyeballs.connect he destination ports >>? fun (_, socket) ->
    let flow = TCP.make socket in
    let process () =
      let authenticator =
        Option.map
          (fun authenticator -> Tls.Config.client ~authenticator ())
          authenticator in
      match authenticator with
      | Some (Error _ as err) -> Lwt.return err
      | Some (Ok tls) ->
          let ctx =
            Sendmail_with_starttls.Context_with_tls.make ?encoder ?decoder
              ?queue () in
          Sendmail_with_starttls.sendmail lwt tcp flow ctx tls ?authentication
            ~domain sender recipients mail
          |> Lwt_scheduler.prj
          >|= open_sendmail_with_starttls_error
      | None ->
          let ctx = Colombe.State.Context.make ?encoder ?decoder () in
          Sendmail.sendmail lwt tcp flow ctx ~domain ?authentication sender
            recipients mail
          |> Lwt_scheduler.prj
          >|= open_sendmail_error
          >|= (function
                | Error err -> Error (err :> error) | Ok value -> Ok value)
          >|= open_error in
    Lwt.try_bind process
      (fun result -> Socket.close socket >|= fun () -> result)
      (fun exn ->
        Socket.close socket >>= fun () ->
        match exn with
        | Failure msg -> Lwt.return_error (`Msg msg)
        | exn -> Lwt.fail exn)
end
