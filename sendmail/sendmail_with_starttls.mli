open Colombe.Sigs
open Colombe

module Context_with_tls : sig
  type t
  type encoder
  type decoder

  val pp : t Fmt.t

  val encoder : t -> encoder
  val decoder : t -> decoder
  val make : unit -> t
  val tls : t -> bool
end

module type VALUE = sig
  type 'x send
  type 'x recv

  type error

  val pp_error : error Fmt.t

  val encode_without_tls : Encoder.encoder -> 'x send -> 'x -> (unit, [> `Protocol of error ]) State.t
  val decode_without_tls : Decoder.decoder -> 'x recv -> ('x, [> `Protocol of error ]) State.t
end

module Value : sig
  type error =
    [ Request.Encoder.error
    | Reply.Decoder.error
    | `Unexpected_response of int * string list ]
end

module type S = sig
  type 'x send
  type 'x recv

  module Value : sig type error end

  type error =
    [ `Protocol of Value.error
    | `Tls_alert of Tls.Packet.alert_type
    | `Tls_failure of Tls.Engine.failure ]

  val pp_error : error Fmt.t

  type encoder
  type decoder

  val starttls_as_client : encoder -> Tls.Config.client -> (unit, [> error ]) State.t
  val starttls_as_server : decoder -> Tls.Config.server -> (unit, [> error ]) State.t
  val close : encoder -> (unit, [> error ]) State.t

  val encode : encoder -> 'a send -> 'a -> (unit, [> error ]) State.t
  val decode : decoder -> 'a recv -> ('a, [> error ]) State.t
end
(* {b Note.} Even if [Make_with_tls.encoder = Make_with_tls.decoder],
   idiomaticaly a server will start to decoder and a client will start to
   encode. *)

module Make_with_tls (Value : VALUE)
  : S with type 'x send = 'x Value.send
       and type 'x recv = 'x Value.recv
       and type Value.error = Value.error
       and type encoder = Context_with_tls.encoder
       and type decoder = Context_with_tls.decoder

type domain = Domain.t
type reverse_path = Reverse_path.t
type forward_path = Forward_path.t

type mechanism = Sendmail.mechanism
type authentication = Sendmail.authentication
type ('a, 's) stream = ('a, 's) Sendmail.stream

type error =
  [ `Tls of [ `Protocol of Value.error
            | `Tls_alert of Tls.Packet.alert_type
            | `Tls_failure of Tls.Engine.failure ]
  | `Protocol of [ `Protocol of Value.error
                 | `Tls_alert of Tls.Packet.alert_type
                 | `Tls_failure of Tls.Engine.failure ]
  | `Unsupported_mechanism
  | `Encryption_required
  | `Weak_mechanism
  | `Authentication_rejected
  | `Authentication_failed
  | `Authentication_required
  | `STARTTLS_unavailable ]

val pp_error : error Fmt.t

val sendmail :
  's impl -> ('flow, 's) rdwr -> 'flow ->
  Context_with_tls.t ->
  Tls.Config.client -> ?authentication:authentication ->
  domain:Domain.t ->
  reverse_path -> forward_path list ->
  ((string * int * int), 's) stream ->
  ((unit, error) result, 's) io
(** [sendmail impl rdwr flow ctx tls_config ?authentication ~domain sender recipients mail] where:

    {ul
    {- [impl] is the scheduler (unix, lwt or async)}
    {- [rdwr] read/write {i syscall}}
    {- [flow] witness of the flow (can be a socket)}
    {- [ctx] context used by the process}
    {- [tls_config] TLS configuration used by STARTTLS}
    {- [authentication] authentication information used by the process}
    {- [sender] sender of the mail}
    {- [recipients] recipients of the mail}
    {- [mail] stream of the mail}}

    This process try to send a mail according [RFC4409]. It ensures to use [STARTTLS] (eg. [RFC3207]) while the process
    according TLS configuration [tls_config]. If [authentication] is given, it does the authentication
    only while TLS flow. Mail is sended only while TLS flow. *)
