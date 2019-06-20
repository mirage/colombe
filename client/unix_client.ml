open Colombe
open Send_mail

let ( <.> ) f g = fun x -> f (g x)

module Unix_scheduler = Sigs.Make(type 'a t = 'a)

let lwt =
  { Sigs.bind= (fun x f -> f (Unix_scheduler.prj x))
  ; return= Unix_scheduler.inj }

type flow =
  { ic : Unix.file_descr
  ; oc : Unix.file_descr }

let rdwr =
  { Sigs.rd= (fun { ic; _ } bytes off len ->
        let res = Unix.read ic bytes off len in
        Unix_scheduler.inj res)
  ; wr= (fun { oc; _ } bytes off len ->
        let res = Unix.write bytes off len in
        Unix_scheduler.inj res) }

type error = Send_mail.error

let pp_error = Send_mail.pp_error

let run ?logger ~hostname ?(port= 587) ~domain ~authenticator ~from ~recipients auth mail =
  let hostname = Domain_name.to_string hostname in
  let ctx = State.make_ctx () in
  let state = Send_mail.make_state ?logger ~domain ~from ~recipients auth mail |> Send_mail.make in
  Tls_lwt.connect authenticator (hostname, port) >>= fun (ic, oc) ->
  let res = run lwt rdwr { ic; oc; } state ctx in Lwt_scheduler.prj res >>= function
  | Ok _ -> Lwt.return (Ok ())
  | Error err ->  Lwt.return (Error err)
