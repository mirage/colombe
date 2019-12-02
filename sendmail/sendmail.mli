open Colombe.Sigs
open Colombe.State
open Colombe

type domain = Domain.t
type reverse_path = Reverse_path.t
type forward_path = Forward_path.t

type mechanism = PLAIN

type authentication =
  { username : string
  ; password : string
  ; mechanism : mechanism }

type ('a, 's) stream = unit -> ('a option, 's) io

type error =
  [ Request.Decoder.error
  | Reply.Decoder.error
  | `Unexpected_response of (int * string list)
  | `Unsupported_mechanism
  | `Encryption_required
  | `Weak_mechanism  ]

val sendmail :
  's impl ->
  ('flow, 's) rdwr ->
  'flow ->
  context ->
  domain:Domain.t ->
  ?authentication:authentication ->
  reverse_path ->
  forward_path list ->
  ((string * int * int), 's) stream ->
  ((unit, error) result, 's) io
