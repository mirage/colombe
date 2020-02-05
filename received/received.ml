open Colombe
open Mrmime

module Option = struct
  let map f = function
    | Some x -> Some (f x)
    | None -> None
end

type 'a stream = unit -> 'a option
type protocol = [ `ESMTP | `SMTP | `Atom of string ]
type link = [ `TCP | `Atom of string ]

type 'a with_info =
  | Only of 'a
  | With of 'a * info
and info =
  [ `Address of Domain.t
  | `Domain_and_address of Domain.t * Domain.t ]

type t =
  { from : Domain.t with_info option
  ; by : Domain.t with_info option
  ; via : link option
  ; _with : protocol option
  ; id : [ `Local of Emile.local
         | `MsgID of Mrmime.MessageID.t
         | `Atom of string ] option
  ; _for : Path.t option
  ; date_time : Mrmime.Date.t }

let map_domain = function
  | Domain.IPv4 v -> `Addr (Emile.IPv4 v)
  | Domain.IPv6 v -> `Addr (Emile.IPv6 v)
  | Domain.Extension (ldh, v) -> `Addr (Emile.Ext (ldh, v))
  | Domain.Domain v -> `Domain v

let msg_id t =
  let domain = match t.by with
    | Some (Only domain) -> Some domain
    | Some (With (domain, _)) -> Some domain
    | None -> None in
  let domain = Option.map map_domain domain in
  match t.id, domain with
  | Some (`Local local), Some domain -> Some (local, domain)
  | Some (`Atom atom), Some domain -> Some ([ `Atom atom ], domain)
  | Some (`MsgID v), _ -> Some (v :> (Emile.local * Emile.domain))
  | _ -> None

let equal t0 t1 =
  let msg_id0 = msg_id t0 in
  let msg_id1 = msg_id t1 in
  match msg_id0, msg_id1 with
  | Some (local0, domain0), Some (local1, domain1) ->
    Emile.equal_local local0 local1
    && Emile.equal_domain domain0 domain1
  | Some _, None | None, Some _
  | None, None -> false

let compare t0 t1 =
  let sup = 1 and inf = (-1) in
  let msg_id0 = msg_id t0 in
  let msg_id1 = msg_id t1 in
  match msg_id0, msg_id1 with
  | Some (local0, domain0), Some (local1, domain1) ->
    let res = Emile.compare_local local0 local1 in
    if res = 0 then Emile.compare_domain domain0 domain1 else res
  | Some _, None -> sup
  | None, Some _ -> inf
  | None, None -> 0

let received_by { by; _ } = by
let received_from { from; _ } = from
let received_for { _for; _ } = _for

let tcp = `TCP
let link v = `Atom v
let smtp = `SMTP
let esmtp = `ESMTP
let protocol v = `Atom v

let make ?from ?by ?via ?protocol ?id _for ~zone ptime =
  let date_time = Date.of_ptime ~zone ptime in
  let id = match id with Some id -> Some (`MsgID id) | None -> None in
  { from; by; via; id; _with= protocol; _for; date_time; }

let ( <.> ) f g = fun x -> f (g x)

let pp_extended_domain ppf = function
  | Only v -> Domain.pp ppf v
  | With (v, `Address addr) ->
    Fmt.pf ppf "%a (%a)" Domain.pp v Domain.pp addr
  | With (v, `Domain_and_address (domain, addr)) ->
    Fmt.pf ppf "%a (%a %a)" Domain.pp v Domain.pp domain Domain.pp addr

let pp_via ppf = function
  | `Atom v -> Fmt.string ppf v
  | `TCP -> Fmt.string ppf "tcp"

let pp_with ppf = function
  | `ESMTP -> Fmt.string ppf "esmtp"
  | `SMTP -> Fmt.string ppf "smtp"
  | `Atom v -> Fmt.string ppf v

let pp_id ppf = function
  | `MsgID v -> MessageID.pp ppf v
  | `Local v -> Emile.pp_local ppf v
  | `Atom v -> Fmt.string ppf v

let pp_for = Path.pp

let pp ppf t =
  Fmt.pf ppf "from: %a@\n\
              by:   %a@\n\
              via:  %a@\n\
              with: %a@\n\
              id:   %a@\n\
              for:  %a@\n\
              date: %a@\n"
    Fmt.(option pp_extended_domain) t.from
    Fmt.(option pp_extended_domain) t.by
    Fmt.(option pp_via) t.via
    Fmt.(option pp_with) t._with
    Fmt.(option pp_id) t.id
    Fmt.(option pp_for) t._for
    Fmt.(using (Rresult.R.get_ok <.> Date.to_ptime) (Ptime.pp_rfc3339 ())) t.date_time

let some x : _ option = Some x

module Decoder = struct
  open Angstrom

  let is_wsp = function ' ' | '\t' -> true | _ -> false

  let domain =
        (Domain.Decoder.ipv4_address_literal >>| fun v -> Domain.IPv4 v)
    <|> (Domain.Decoder.ipv6_addr >>| fun v -> Domain.IPv6 v)
    <|> Domain.Decoder.domain
  let address_literal = Domain.Decoder.address_literal
  let atom = lift (fun x -> `Atom x) Path.Decoder.atom
  let date_time = Date.Decoder.date_time
  let fws = skip_while is_wsp
  let cfws = Emile.Parser.cfws

  let tcp_info =
    (address_literal >>= fun v -> return (`Address v))
    <|> (domain >>= fun domain -> skip_while is_wsp *> address_literal >>= fun v ->
         return (`Domain_and_address (domain, v)))

  let via = string_ci "via"
  let _with = string_ci "with"
  let id = string_ci "id"
  let _for = string_ci "for"

  let link =
    atom >>= function
    | `Atom "TCP" -> return `TCP
    | v -> return v

  let protocol =
    atom >>= function
    | `Atom "ESMTP" -> return `ESMTP
    | `Atom "SMTP" -> return `SMTP
    | v -> return v

  let via = option () cfws *> via *> fws *> link
  let via = lift some via

  let _with = option () cfws *> _with *> fws *> protocol
  let _with = lift some _with

  let id =
    let msg_id = MessageID.Decoder.message_id >>| fun v -> `MsgID v in
    let local = Emile.Parser.local_part >>| fun v -> `Local v in
    option () cfws *> id *> fws
    *> (msg_id <|> local <|> atom)
  let id = lift some id

  let _for =
    option () cfws *> _for *> fws *>
    (    (Path.Decoder.path >>= fun v -> return v)
     <|> (Path.Decoder.mailbox >>= fun (local, domain) -> return { Path.local; domain; rest= [] }))
  let _for = lift some _for

  let extended_domain =
        (domain >>= fun domain ->
         skip_while is_wsp *> char '(' *> tcp_info <* char ')' >>= fun tcp_info ->
         return (With (domain, tcp_info)))
    <|> (address_literal >>= fun domain ->
         skip_while is_wsp *> char '(' *> tcp_info <* char ')' >>= fun tcp_info ->
         return (With (domain, tcp_info)))
    <|> (domain >>= fun domain -> return (Only domain))

  let from_domain = string_ci "from" *> skip_while is_wsp *> extended_domain
  let by_domain = option () cfws *> string_ci "by" *> skip_while is_wsp *> extended_domain

  let stamp =
    fws *> option None (lift some from_domain) >>= fun from_domain ->
    fws *> option None (lift some by_domain)   >>= fun by_domain ->
    fws *> option None via   >>= fun via ->
    fws *> option None _with >>= fun _with ->
    fws *> option None id    >>= fun id ->
    fws *> option None _for  >>= fun _for ->
    option () cfws *> char ';' *> skip_while is_wsp *>
    date_time >>= fun date_time ->
    return { from= from_domain
           ; by= by_domain
           ; via; _with; id; _for
           ; date_time }
end

module Encoder = struct
  open Prettym

  let extended_domain ppf = function
    | Only domain -> string ppf (Domain.to_string domain)
    | With (domain, `Address addr) ->
      eval ppf [ !!string; spaces 1; char $ '('; !!string; char $ ')' ]
        (Domain.to_string domain) (Domain.to_string addr)
    | With (domain, `Domain_and_address (v, addr)) ->
      eval ppf [ !!string; spaces 1; char $ '('; !!string; spaces 1; !!string; char $ ')' ]
        (Domain.to_string domain) (Domain.to_string v) (Domain.to_string addr)

  let via ppf = function
    | `TCP -> string ppf "tcp"
    | `Atom v -> string ppf v

  let via ppf = function
    | None -> ppf
    | Some v -> eval ppf [ string $ "via"; spaces 1; !!via ] v

  let _with ppf = function
    | `ESMTP -> string ppf "esmtp"
    | `SMTP -> string ppf "smtp"
    | `Atom v -> string ppf v

  let _with ppf = function
    | None -> ppf
    | Some v -> eval ppf [ string $ "with"; spaces 1; !!_with ] v

  let id ppf = function
    | `MsgID v -> Mrmime.MessageID.Encoder.message_id ppf v
    | `Local v -> string ppf (Fmt.strf "%a" Emile.pp_local v)
    | `Atom v -> string ppf v

  let id ppf = function
    | None -> ppf
    | Some v -> eval ppf [ box; string $ "id"; spaces 1; !!id; close ] v

  let _for ppf v = string ppf (Path.Encoder.to_string v)

  let _for ppf = function
    | None -> ppf
    | Some v -> eval ppf [ box; string $ "for"; spaces 1; !!_for; close ] v

  let opt_info ppf (v_via, v_with, v_id, v_for) =
    eval ppf [ box; !!via; fws; !!_with; fws; !!id; fws; !!_for; char $ ';'; close; new_line ]
      v_via v_with v_id v_for

  let from_domain ppf = function
    | Some v ->
      eval ppf [ box; string $ "from"; fws; !!extended_domain; close; new_line ] v
    | None -> ppf

  let by_domain ppf = function
    | Some v ->
      eval ppf [ box; string $ "by"; fws; !!extended_domain; close; new_line ] v
    | None -> ppf

  let received ppf { from; by; via; _with; id; _for; date_time; } =
    eval ppf [ box
             ; !!from_domain
             ; !!by_domain
             ; !!opt_info
             ; !!Mrmime.Date.Encoder.date
             ; close ]
      from by (via, _with, id, _for) date_time

  let as_field ppf v =
    eval ppf [ string $ "Received"; char $ ':'; tbox 1; spaces 1; !!received; close; new_line ] v
end

let received = Field_name.v "Received"

let of_unstructured v =
  let v = List.fold_left (fun a -> function #Unstrctrd.elt as x -> x :: a | _ -> a) [] v in
  let v = List.rev v in
  match Unstrctrd.of_list v with
  | Ok v ->
    let v = Unstrctrd.fold_fws v in
    Unstrctrd.to_utf_8_string v
  | Error _ -> assert false (* TODO *)

let of_stream stream =
  let raw = Bigstringaf.create (2 * 0x1000) in
  let decoder = Hd.decoder raw in
  let rec go acc = match Hd.decode decoder with
    | `Field field ->
      let Field.Field (field_name, w, v) = Location.prj field in
      ( match w with
        | Field.Unstructured ->
          if Field_name.equal field_name received
          then match Angstrom.parse_string Decoder.stamp (of_unstructured v) with
            | Ok v -> go (v :: acc)
            | Error _ -> go acc
          else go acc
        | _ -> go acc )
    | `Malformed err ->
      Rresult.R.error_msg err
    | `End rest -> Rresult.R.ok (rest, List.rev acc)
    | `Await ->
      let buf, off, len = match stream () with 
        | Some (buf, off, len) -> buf, off, len
        | None -> "", 0, 0 in
      match Hd.src decoder buf off len with
      | Ok () -> go acc
      | Error #Rresult.R.msg as err -> err in
  go []
