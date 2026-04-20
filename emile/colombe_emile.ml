let some x = Some x

let concatene local =
  String.concat "." (List.map (function `Atom x -> x | `String x -> x) local)

let is_string = function `String _ -> true | `Atom _ -> false

let to_domain = function
  | `Literal str ->
      let err =
        Format.asprintf "Impossible to make a path with a literal domain: %s"
          str in
      invalid_arg err
  | `Domain l -> Colombe.Domain.Domain l
  | `Addr (Emile.IPv4 v) -> Colombe.Domain.IPv4 v
  | `Addr (Emile.IPv6 v) -> Colombe.Domain.IPv6 v
  | `Addr (Emile.Ext (k, v)) -> Colombe.Domain.Extension (k, v)

let of_domain = function
  | Colombe.Domain.Domain l -> `Domain l
  | Colombe.Domain.IPv4 v -> `Addr (Emile.IPv4 v)
  | Colombe.Domain.IPv6 v -> `Addr (Emile.IPv6 v)
  | Colombe.Domain.Extension (k, v) -> `Addr (Emile.Ext (k, v))

let of_local = function
  | `Dot_string vs -> List.map (fun x -> `Atom x) vs
  | `String v -> [ `String v ]

let to_local local =
  if List.exists is_string local
  then `String (concatene local)
  else `Dot_string (List.map (function `Atom x -> x | `String x -> x) local)

let to_path ?route mailbox =
  let local = mailbox.Emile.local in
  let domain, domains = mailbox.Emile.domain in
  let local = to_local local in
  let rest =
    match route with Some route -> route | None -> List.map to_domain domains
  in
  let domain = to_domain domain in
  { Colombe.Path.local; domain; rest }

let to_reverse_path ?route mailbox = some (to_path ?route mailbox)

let to_forward_path ?route mailbox =
  match mailbox.Emile.local with
  | [ `Atom local ] when String.lowercase_ascii local = "postmaster" ->
      Colombe.Forward_path.Domain (to_domain (fst mailbox.Emile.domain))
  | _ -> Colombe.Forward_path.Forward_path (to_path ?route mailbox)

let of_path path =
  let local = of_local path.Colombe.Path.local in
  let domain = of_domain path.Colombe.Path.domain in
  let domains = List.map of_domain path.Colombe.Path.rest in
  { Emile.name = None; local; domain = (domain, domains) }

let of_forward_path = function
  | Colombe.Forward_path.Postmaster -> None
  | Colombe.Forward_path.Domain domain ->
      let domain = of_domain domain in
      Some
        {
          Emile.name = None;
          local = [ `Atom "Postmaster" ];
          domain = (domain, []);
        }
  | Colombe.Forward_path.Forward_path path -> Some (of_path path)

let of_reverse_path = function None -> None | Some path -> Some (of_path path)
