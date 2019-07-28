open Lwt.Infix
open Colombe
open Sendmail

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

let pp_error = Sendmail.pp_error

let run ?logger ~hostname ?(port= 587) ~domain ~authenticator ~from ~recipients auth mail =
  let hostname = Domain_name.to_string hostname in
  let ctx = Colombe.State.make_ctx () in
  let state = Sendmail.make_state ?logger ~domain ~from ~recipients auth mail |> Sendmail.make in
  Tls_lwt.connect authenticator (hostname, port) >>= fun (ic, oc) ->
  let res = run lwt rdwr { ic; oc; } state ctx in Lwt_scheduler.prj res >>= function
  | Ok _ -> Lwt.return (Ok ())
  | Error err ->  Lwt.return (Error err)
