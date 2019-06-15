open Colombe.Sigs
open Colombe.State
open Colombe

type error

val pp_error : error Fmt.t

type 'a stream = unit -> 'a option
type 'x state
type 'x t

val make_state :
  domain:Domain.t ->
  from:Reverse_path.t ->
  recipients:Forward_path.t list ->
  Auth.authenticator option ->
  (bytes * int * int) stream -> 'x state

val make : 'x state -> 'x t

val run : 's impl -> ('flow, 's) rdwr -> 'flow -> 'x t -> ctx -> (('x state, error) result, 's) io
