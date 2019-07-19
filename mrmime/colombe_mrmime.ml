let some x = Some x

let concatene local =
  String.concat "." (List.map (function `Atom x -> x | `String x -> x) local)

let is_string = function
  | `String _ -> true
  | `Atom _ -> false

let cast_domain = function
  | `Literal _ -> Rresult.R.error_msgf "Impossible to make a path with a literal domain"
  | `Domain l -> Ok (Colombe.Domain.Domain l)
  | `Addr (Mrmime.Mailbox.IPv4 v) -> Ok (Colombe.Domain.IPv4 v)
  | `Addr (Mrmime.Mailbox.IPv6 v) -> Ok (Colombe.Domain.IPv6 v)
  | `Addr (Mrmime.Mailbox.Ext (k, v)) -> Ok (Colombe.Domain.Extension (k, v))

let to_path ?(route= []) mailbox =
  let local = mailbox.Mrmime.Mailbox.local in
  let domain, _ = mailbox.Mrmime.Mailbox.domain in

  let local =
    if List.exists is_string local
    then `String (concatene local)
    else `Dot_string (List.map (function `Atom x -> x | `String x -> x) local) in
  match cast_domain domain with
  | Ok domain -> Ok { Colombe.Path.local; domain; rest= route }
  | Error _ as err -> err

let to_reverse_path ?route mailbox =
  let open Rresult.R in
  to_path ?route mailbox >>| some

let to_forward_path ?route mailbox =
  let open Rresult.R in
  match mailbox.Mrmime.Mailbox.local with
  | [ `Atom "Postmaster" ] ->
    cast_domain (fst mailbox.Mrmime.Mailbox.domain)
    >>| fun domain -> Colombe.Forward_path.Domain domain
  | _ ->
    to_path ?route mailbox
    >>| fun path -> Colombe.Forward_path.Forward_path path
