let some x = Some x
let error_msgf fmt = Format.kasprintf (fun err -> Error (`Msg err)) fmt

let concatene local =
  String.concat "." (List.map (function `Atom x -> x | `String x -> x) local)

let is_string = function
  | `String _ -> true
  | `Atom _ -> false

let cast_domain = function
  | `Literal _ -> error_msgf "Impossible to make a path with a literal domain"
  | `Domain l -> Ok (Colombe.Domain.Domain l)
  | `Addr (Emile.IPv4 v) -> Ok (Colombe.Domain.IPv4 v)
  | `Addr (Emile.IPv6 v) -> Ok (Colombe.Domain.IPv6 v)
  | `Addr (Emile.Ext (k, v)) -> Ok (Colombe.Domain.Extension (k, v))

let of_local = function
  | `Dot_string vs -> List.map (fun x -> `Atom x) vs
  | `String v -> [ `String v ]

let to_path ?(route= []) mailbox =
  let local = mailbox.Emile.local in
  let domain, _ = mailbox.Emile.domain in

  let local =
    if List.exists is_string local
    then `String (concatene local)
    else `Dot_string (List.map (function `Atom x -> x | `String x -> x) local) in
  match cast_domain domain with
  | Ok domain -> Ok { Colombe.Path.local; domain; rest= route }
  | Error _ as err -> err

let ( >>| ) x f = match x with Ok x -> Ok (f x) | Error err -> Error err

let to_reverse_path ?route mailbox =
  to_path ?route mailbox >>| some

let to_forward_path ?route mailbox =
  match mailbox.Emile.local with
  | [ `Atom "Postmaster" ] ->
    cast_domain (fst mailbox.Emile.domain)
    >>| fun domain -> Colombe.Forward_path.Domain domain
  | _ ->
    to_path ?route mailbox
    >>| fun path -> Colombe.Forward_path.Forward_path path

let to_domain = cast_domain
