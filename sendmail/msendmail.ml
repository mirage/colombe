open Colombe

module Miou_scheduler = Sigs.Make (struct
  type 'a t = 'a
end)

let miou =
  let open Miou_scheduler in
  let bind x fn = (Fun.compose fn prj) x in
  let return = inj in
  { Sigs.bind; return }

type error = [ `Msg of string | Sendmail_with_starttls.error ]

let open_sendmail_error = function
  | Ok _ as v -> v
  | Error (#Sendmail.error as err) -> Error err

let open_sendmail_with_starttls_error = function
  | Ok _ as v -> v
  | Error (#Sendmail_with_starttls.error as err) -> Error err

let open_error = function Ok _ as v -> v | Error (#error as err) -> Error err
let error_msgf fmt = Fmt.kstr (fun msg -> Error (`Msg msg)) fmt
let ( let* ) = Result.bind

let tcp =
  let open Miou_scheduler in
  let rd flow buf off len =
    match Mnet.TCP.read flow buf ~off ~len with
    | 0 -> inj `End
    | len -> inj (`Len len) in
  let wr flow str off len = inj (Mnet.TCP.write flow str ~off ~len) in
  { Colombe.Sigs.rd; wr }

let tls =
  let open Miou_scheduler in
  let rd flow buf off len =
    match Mnet_tls.read flow buf ~off ~len with
    | 0 -> inj `End
    | len -> inj (`Len len) in
  let wr flow str off len = inj (Mnet_tls.write flow str ~off ~len) in
  { Colombe.Sigs.rd; wr }

let authenticator : (_, error) result Miou.Lazy.t =
  Miou.Lazy.from_fun Ca_certs_nss.authenticator

let tls_config user's_tls_config user's_authenticator =
  match user's_tls_config with
  | Some cfg -> Ok cfg
  | None ->
      let* authenticator =
        match (Miou.Lazy.force authenticator, user's_authenticator) with
        | Ok authenticator, None -> Ok authenticator
        | _, Some authenticator -> Ok authenticator
        | (Error _ as err), None -> err in
      Tls.Config.client ~authenticator ()

let submit ?encoder ?decoder ?queue he ~destination:dst ?port ~domain
    ?cfg:user's_tls_config ?authenticator:user's_authenticator ?authentication
    sender recipients stream =
  let ports = match port with None -> [ 465; 587 ] | Some port -> [ port ] in
  let into, bqueue = Flux.Sink.bqueue ~size:0x7ff in
  let* tls_cfg = tls_config user's_tls_config user's_authenticator in
  let* (_, port), flow = Mnet_happy_eyeballs.connect he dst ports in
  let protocol =
    if port = 587 then `With_starttls tls_cfg else `With_tls tls_cfg in
  match protocol with
  | `With_starttls cfg -> begin
      let ctx =
        Sendmail_with_starttls.Context_with_tls.make ?encoder ?decoder ?queue ()
      in
      let prm0 =
        Miou.async @@ fun () ->
        let finally = Flux.Bqueue.close in
        let resource = Miou.Ownership.create ~finally bqueue in
        Miou.Ownership.own resource ;
        Flux.Stream.into into stream ;
        Miou.Ownership.disown resource in
      let prm1 =
        Miou.async @@ fun () ->
        let finally = Mnet.TCP.close in
        let resource = Miou.Ownership.create ~finally flow in
        Miou.Ownership.own resource ;
        let seq = Flux.Bqueue.to_seq bqueue in
        let dispenser = Seq.to_dispenser seq in
        let dispenser = Fun.compose Miou_scheduler.inj dispenser in
        let result =
          Sendmail_with_starttls.sendmail miou tcp flow ctx cfg ?authentication
            ~domain sender recipients dispenser
          |> Miou_scheduler.prj
          |> open_sendmail_with_starttls_error
          |> open_error in
        Miou.Ownership.release resource ;
        result in
      match (Miou.await prm0, Miou.await prm1) with
      | Ok (), Ok (Ok ()) -> Ok ()
      | Error exn, _ ->
          error_msgf "Unexpected exception from the given email stream: %s"
            (Printexc.to_string exn)
      | Ok (), Ok (Error _ as err) -> err
      | Ok (), Error exn ->
          error_msgf "Unexpected exception from the sendmail process: %s"
            (Printexc.to_string exn)
    end
  | `With_tls cfg -> begin
      let host =
        match Ipaddr.of_string dst with
        | Ok _ -> None
        | Error _ ->
        match Result.bind (Domain_name.of_string dst) Domain_name.host with
        | Ok host -> Some host
        | Error _ -> None in
      let finally = Mnet.TCP.close in
      let res0 = Miou.Ownership.create ~finally flow in
      Miou.Ownership.own res0 ;
      let flow' = Mnet_tls.client_of_fd cfg ?host flow in
      Miou.Ownership.disown res0 ;
      let ctx = Colombe.State.Context.make ?encoder ?decoder () in
      let prm0 =
        Miou.async @@ fun () ->
        let finally = Flux.Bqueue.close in
        let resource = Miou.Ownership.create ~finally bqueue in
        Miou.Ownership.own resource ;
        Flux.Stream.into into stream ;
        Miou.Ownership.disown resource in
      let prm1 =
        Miou.async @@ fun () ->
        let finally = Mnet_tls.close in
        let resource = Miou.Ownership.create ~finally flow' in
        Miou.Ownership.own resource ;
        let seq = Flux.Bqueue.to_seq bqueue in
        let dispenser = Seq.to_dispenser seq in
        let dispenser = Fun.compose Miou_scheduler.inj dispenser in
        let result =
          Sendmail.sendmail miou tls flow' ctx ~domain ?authentication sender
            recipients dispenser
          |> Miou_scheduler.prj
          |> open_sendmail_error
          |> Result.map_error (fun err -> (err :> error))
          |> open_error in
        Miou.Ownership.release resource ;
        result in
      match (Miou.await prm0, Miou.await prm1) with
      | Ok (), Ok (Ok ()) -> Ok ()
      | Error exn, _ ->
          error_msgf "Unexpected exception from the given email stream: %s"
            (Printexc.to_string exn)
      | Ok (), Ok (Error err) -> Error err
      | Ok (), Error exn ->
          error_msgf "Unexpected exception from the sendmail process: %s"
            (Printexc.to_string exn)
    end

let sendmail ?encoder ?decoder ?queue he ~destination:dst ?port ~domain
    ?cfg:user's_tls_config ?authenticator:user's_authenticator ?authentication
    sender recipients stream =
  let ports = match port with None -> [ 25 ] | Some port -> [ port ] in
  let into, bqueue = Flux.Sink.bqueue ~size:0x7ff in
  let* tls_cfg = tls_config user's_tls_config user's_authenticator in
  let* _, flow = Mnet_happy_eyeballs.connect he dst ports in
  let ctx =
    Sendmail_with_starttls.Context_with_tls.make ?encoder ?decoder ?queue ()
  in
  let prm0 =
    Miou.async @@ fun () ->
    let finally = Flux.Bqueue.close in
    let resource = Miou.Ownership.create ~finally bqueue in
    Miou.Ownership.own resource ;
    Flux.Stream.into into stream ;
    Miou.Ownership.disown resource in
  let prm1 =
    Miou.async @@ fun () ->
    let finally = Mnet.TCP.close in
    let resource = Miou.Ownership.create ~finally flow in
    Miou.Ownership.own resource ;
    let seq = Flux.Bqueue.to_seq bqueue in
    let dispenser = Seq.to_dispenser seq in
    let dispenser = Fun.compose Miou_scheduler.inj dispenser in
    let result =
      Sendmail_with_starttls.sendmail miou tcp flow ctx tls_cfg ?authentication
        ~domain sender recipients dispenser
      |> Miou_scheduler.prj
      |> open_sendmail_with_starttls_error in
    Miou.Ownership.release resource ;
    result in
  match (Miou.await prm0, Miou.await prm1) with
  | Ok (), Ok (Ok ()) -> Ok ()
  | Error exn, _ ->
      error_msgf "Unexpected exception from the given email stream: %s"
        (Printexc.to_string exn)
  | Ok (), Ok (Error err) -> Error err
  | Ok (), Error exn ->
      error_msgf "Unexpected exception from the sendmail process: %s"
        (Printexc.to_string exn)
