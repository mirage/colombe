open Lwt.Infix
open Colombe
open Send_mail

let ( <.> ) f g = fun x -> f (g x)

module Lwt_scheduler = Sigs.Make(Lwt)

let lwt_bind x f =
  let open Lwt.Infix in
  let open Lwt_scheduler in
  inj (prj x >>= (prj <.> f))

let lwt =
  { Sigs.bind= lwt_bind
  ; return= (fun x -> Lwt_scheduler.inj (Lwt.return x)) }

type flow =
  { ic : Lwt_io.input_channel
  ; oc : Lwt_io.output_channel }

let rdwr =
  { Sigs.rd= (fun { ic; _ } bytes off len ->
        let res = Lwt_io.read_into ic bytes off len in
        Lwt_scheduler.inj res)
  ; wr= (fun { oc; _ } bytes off len ->
        let res = Lwt_io.write_from_exactly oc bytes off len in
        Lwt_scheduler.inj res) }

type error = Send_mail.error

let pp_error = Send_mail.pp_error

let run ~hostname ?(port= 587) ~domain ~authenticator ~from ~recipients auth mail =
  let hostname = Domain_name.to_string hostname in
  let ctx = State.make_ctx () in
  let state = Send_mail.make_state ~domain ~from ~recipients auth mail |> Send_mail.make in
  Fmt.epr "> TLS start (%s:%d).\n%!" hostname port ;
  Tls_lwt.connect authenticator (hostname, port) >>= fun (ic, oc) ->
  Fmt.epr "# TLS done.\n%!" ;
  let res = run lwt rdwr { ic; oc; } state ctx in Lwt_scheduler.prj res >>= function
  | Ok _ -> Lwt.return (Ok ())
  | Error err ->  Lwt.return (Error err)
