open Lwt.Infix
open Colombe

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
        let res = Lwt_io.write_from_exactly oc (Bytes.unsafe_of_string bytes) off len in
        Lwt_scheduler.inj res) }

type error = Sendmail.error

let sendmail ~hostname ?(port= 587) ~domain ~authenticator ?authentication sender recipients mail =
  let hostname = Domain_name.to_string hostname in
  let ctx = Colombe.State.Context.make () in
  let mail () = Lwt_scheduler.inj (mail ()) in
  Tls_lwt.connect authenticator (hostname, port) >>= fun (ic, oc) ->
  Sendmail.sendmail lwt rdwr { ic; oc; } ctx ~domain ?authentication sender recipients mail
  |> Lwt_scheduler.prj
