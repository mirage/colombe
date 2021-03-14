(** Received field *)

type t
(** Type of [Received] value. *)

val compare : t -> t -> int
val equal : t -> t -> bool

type 'a with_info =
  | Only of 'a
  | With of 'a * info
and info =
  [ `Address of Colombe.Domain.t
  | `Domain_and_address of Colombe.Domain.t * Colombe.Domain.t ]

val received_by : t -> Colombe.Domain.t with_info option
val received_from : t -> Colombe.Domain.t with_info option
val received_for : t -> Colombe.Path.t option

type link = private [ `TCP | `Atom of string ]
(** Type of underlying protocol used to receive email. *)

type protocol = private
  [ `ESMTP | `SMTP | `Atom of string ]
(** Type of protocol used to receive email:
    {ul
    {- [SMTP]}
    {- [ESMTP]}
    {- Other protocol}} *)

val received_with : t -> protocol option
val received_via : t -> link option
val id : t -> [ `Local of Emile.local | `MsgID of Mrmime.MessageID.t | `Atom of string ] option
val date_time : t -> Mrmime.Date.t

type 'a stream = unit -> 'a option

val tcp : link
val link : string -> link

val smtp : protocol
val esmtp : protocol
val protocol : string -> protocol

val make :
  ?from:Colombe.Domain.t with_info ->
  ?by:Colombe.Domain.t with_info ->
  ?via:link ->
  ?protocol:protocol ->
  ?id:Mrmime.MessageID.t ->
  Colombe.Path.t option ->
  zone:Mrmime.Date.Zone.t ->
  Ptime.t -> t
(** [make ?from ?by ?via ?protocol ?id path ~zone time]:

    {ul
    {- [from] is the domain where the email comes from.}
    {- [by] is the domain where the email is received.}
    {- [via] is the underlying protocol used to receive the email (usually [tcp]).}
    {- [protocol] is the protocol used to receive the email.}
    {- [id] is the chosen ID by the {i MTA}.}
    {- [path] is the given {!Path.t} received by the {i MTA}}
    {- [zone] is the {i time-zone} of the {i MTA}.}
    {- [time] is the time when the {i MTA} received the email.}} *)

val pp : t Fmt.t

module Decoder : sig
  val stamp : t Angstrom.t
end

module Encoder : sig
  val received : t Prettym.t
  val as_field : t Prettym.t
end

val of_stream : (string * int * int) stream -> (string * t list, [> Rresult.R.msg ]) result
