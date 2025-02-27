module Make
    (Socket : Mirage_flow.S)
    (Happy_eyeballs : Happy_eyeballs_mirage.S with type flow = Socket.flow) : sig
  val submit :
    ?encoder:(unit -> bytes) ->
    ?decoder:(unit -> bytes) ->
    ?queue:(unit -> (char, Bigarray.int8_unsigned_elt) Ke.Rke.t) ->
    Happy_eyeballs.t ->
    destination:
      [ `Domain_name of [ `host ] Domain_name.t | `Ipaddrs of Ipaddr.t list ] ->
    ?port:int ->
    domain:Colombe.Domain.t ->
    ?cfg:Tls.Config.client ->
    ?authenticator:X509.Authenticator.t ->
    ?authentication:Sendmail.authentication ->
    Colombe.Reverse_path.t ->
    Colombe.Forward_path.t list ->
    (unit -> (string * int * int) option Lwt.t) ->
    (unit, [> `Msg of string | Sendmail_with_starttls.error ]) result Lwt.t

  val sendmail :
    ?encoder:(unit -> bytes) ->
    ?decoder:(unit -> bytes) ->
    ?queue:(unit -> (char, Bigarray.int8_unsigned_elt) Ke.Rke.t) ->
    Happy_eyeballs.t ->
    destination:
      [ `Domain_name of [ `host ] Domain_name.t | `Ipaddrs of Ipaddr.t list ] ->
    ?port:int ->
    domain:Colombe.Domain.t ->
    ?cfg:Tls.Config.client ->
    ?authenticator:X509.Authenticator.t ->
    ?authentication:Sendmail.authentication ->
    Colombe.Reverse_path.t ->
    Colombe.Forward_path.t list ->
    (unit -> (string * int * int) option Lwt.t) ->
    (unit, [> `Msg of string | Sendmail_with_starttls.error ]) result Lwt.t
end
