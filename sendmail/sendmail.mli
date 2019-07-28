open Colombe.Sigs
open Colombe.State
open Colombe

type helo = Domain.t
type mail_from = Reverse_path.t * (string * string option) list
type rcpt_to = Forward_path.t * (string * string option) list
type pp_220 = string list
type pp_221 = string list
type pp_235 = string list
type pp_250 = string list
type tp_354 = string list
type tp_334 = string list
type pn_501 = string list
type pn_530 = string list
type pn_550 = string list
type pn_554 = string list
type auth = Rfc1869.t

type 'x protocol =
  | Helo : helo protocol
  | Mail_from : mail_from protocol
  | Rcpt_to : rcpt_to protocol
  | Data : unit protocol
  | Dot : unit protocol
  | Quit : unit protocol
  | Auth : auth protocol
  | PP_220 : pp_220 protocol
  | PP_221 : pp_221 protocol
  | PP_235 : pp_235 protocol
  | PP_250 : pp_250 protocol
  | TP_354 : tp_354 protocol
  | TP_334 : tp_334 protocol
  | PN_501 : pn_501 protocol
  | PN_530 : pn_530 protocol
  | PN_550 : pn_550 protocol
  | PN_554 : pn_554 protocol

module Send_mail_p : Colombe.State.PROTOCOL
  with type 'x t = 'x protocol

module Send_mail_s : sig type 's t end
module State : module type of Colombe.State.Make(Send_mail_s)(Send_mail_p)

type error = Send_mail_p.error

val pp_error : error Fmt.t

type 'a stream = unit -> 'a option
type 'x state = 'x Send_mail_s.t
type 'x t = 'x State.t

val make_state :
  ?logger:(module Logs.LOG) ->
  ?encoding:Mime.encoding ->
  domain:Domain.t ->
  from:Reverse_path.t ->
  recipients:Forward_path.t list ->
  Auth.authenticator option ->
  (string * int * int) stream -> 'x state

val make : 'x state -> 'x t

val run : 's impl -> ('flow, 's) rdwr -> 'flow -> 'x t -> ctx -> (('x state, error) result, 's) io
