val submit :
  ?encoder:(unit -> bytes) ->
  ?decoder:(unit -> bytes) ->
  ?queue:(unit -> (char, Bigarray.int8_unsigned_elt) Ke.Rke.t) ->
  Happy_eyeballs_miou_unix.t ->
  destination:string ->
  ?port:int ->
  domain:Colombe.Domain.t ->
  ?cfg:Tls.Config.client ->
  ?authenticator:X509.Authenticator.t ->
  ?authentication:Sendmail.authentication ->
  Colombe.Reverse_path.t ->
  Colombe.Forward_path.t list ->
  (unit -> (string * int * int) option) ->
  (unit, [> `Msg of string | Sendmail_with_starttls.error ]) result

val sendmail :
  ?encoder:(unit -> bytes) ->
  ?decoder:(unit -> bytes) ->
  ?queue:(unit -> (char, Bigarray.int8_unsigned_elt) Ke.Rke.t) ->
  Happy_eyeballs_miou_unix.t ->
  destination:string ->
  ?port:int ->
  domain:Colombe.Domain.t ->
  ?cfg:Tls.Config.client ->
  ?authenticator:X509.Authenticator.t ->
  ?authentication:Sendmail.authentication ->
  Colombe.Reverse_path.t ->
  Colombe.Forward_path.t list ->
  (unit -> (string * int * int) option) ->
  (unit, [> `Msg of string | Sendmail_with_starttls.error ]) result
