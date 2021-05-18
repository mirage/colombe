open Colombe.Sigs
open Colombe.State
open Colombe

type domain = Domain.t

type reverse_path = Reverse_path.t

type forward_path = Forward_path.t

type mechanism = PLAIN

type authentication = {
  username : string;
  password : string;
  mechanism : mechanism;
}

type ('a, 's) stream = unit -> ('a option, 's) io

type error =
  [ `Protocol of
    [ Request.Encoder.error
    | Reply.Decoder.error
    | `Unexpected_response of int * string list ]
  | `Unsupported_mechanism
  | `Encryption_required
  | `Weak_mechanism
  | `Authentication_rejected
  | `Authentication_failed
  | `Authentication_required ]

val pp_error : error Fmt.t

val sendmail :
  's impl ->
  ('flow, 's) rdwr ->
  'flow ->
  Context.t ->
  ?authentication:authentication ->
  domain:Domain.t ->
  reverse_path ->
  forward_path list ->
  (string * int * int, 's) stream ->
  ((unit, error) result, 's) io
(** [sendmail impl rdwr flow ctx ?authentication ~domain sender recipients mail]
    where:

    - [impl] is the scheduler (unix, lwt or async)
    - [rdwr] read/write {i syscalls}
    - [flow] witness of the flow (can be a socket)
    - [ctx] context used by the process
    - [authentication] authentication information used by the process
    - [sender] sender of the mail
    - [recipients] recipients of the mail
    - [mail] stream of the mail

    This process try to send a mail according [RFC4409]. The process should be
    wrapped into a secured flow (like TLS). For a real use-case of [sendmail],
    you should look into [sendmail-lwt] which takes care about the TLS layer. *)
