type t =
  { local : [ `String of string | `Dot_string of string list ]
  ; domain : Domain.t
  ; rest : Domain.t list }

let equal_local a b = match a, b with
  | (`String a | `Dot_string [ a ]),
    (`String b | `Dot_string [ b ]) ->
    String.(equal (lowercase_ascii a) (lowercase_ascii b))
  | `Dot_string a, `Dot_string b ->
    (try List.for_all2 (fun a b -> String.(equal (lowercase_ascii a) (lowercase_ascii b))) a b
     with _ -> false)
  | _, _ -> false

let equal a b =
  equal_local a.local b.local
  && Domain.equal a.domain b.domain
  && (try List.for_all2 Domain.equal a.rest b.rest with _ -> false)

let pp_local ppf = function
  | `String x -> Fmt.(quote string) ppf x
  | `Dot_string l -> Fmt.(list ~sep:(const string ".") string) ppf l

let pp ppf { local; domain; rest; } =
  match rest with
  | [] -> Fmt.pf ppf "<%a@%a>" pp_local local Domain.pp domain
  | rest ->
    Fmt.pf ppf "<%a:%a@%a>"
      Fmt.(list ~sep:(const string ",") (prefix (const string "@") Domain.pp)) rest
      pp_local local Domain.pp domain

module Parser = struct
  open Angstrom

  let at_domain = char '@' *> Domain.Parser.domain

  let a_d_l = at_domain >>= fun x -> many (char ',' *> at_domain) >>| fun r -> x :: r

  let is_atext = function
    | 'a' .. 'z'
    |'A' .. 'Z'
    |'0' .. '9'
    |'!' | '#' | '$' | '%' | '&' | '\'' | '*' | '+' | '-' | '/' | '=' | '?'
    |'^' | '_' | '`' | '{' | '}' | '|' | '~' ->
      true
    | _ -> false

  let is_qtextSMTP = function
    | '\032' | '\033' | '\035' .. '\091' | '\093' .. '\126' -> true
    | _ -> false

  let atom = take_while1 is_atext

  let dot_string = atom >>= fun x -> many (char '.' *> atom) >>| fun r -> `Dot_string (x :: r)

  let quoted_pairSMTP =
    char '\\' *> satisfy (function '\032' .. '\126' -> true | _ -> false) >>| String.make 1

  let qcontentSMTP = quoted_pairSMTP <|> take_while1 is_qtextSMTP

  let quoted_string =
    char '"' *> many qcontentSMTP <* char '"' >>| String.concat "" >>| fun x -> `String x

  let local_part = dot_string <|> quoted_string

  let mailbox =
    local_part
    >>= fun local -> char '@' *> (Domain.Parser.domain <|> Domain.Parser.address_literal)
    >>| fun domain -> (local, domain)

  let path =
    char '<' *> option [] (a_d_l <* char ':')
    >>= fun rest -> mailbox <* char '>'
    >>= fun (local, domain) -> return { local; domain; rest; }
end

module Encoder = struct
  let need_to_escape, escape_char =
    (* See [Mrmime.Rfc822.of_escaped_character] but totally arbitrary. *)
    let bindings = [('\000', '\000')
                   ;('\\',   '\\')
                   ;('\x07', 'a')
                   ;('\b',   'b')
                   ;('\t',   't')
                   ;('\n',   'n')
                   ;('\x0b', 'v')
                   ;('\x0c', 'f')
                   ;('\r',   'r')
                   ;('"',    '"')] in
    ( (fun chr -> List.mem_assoc chr bindings)
    , fun chr -> List.assoc chr bindings )

  let escape x =
    let len = String.length x in
    let res = Buffer.create (len * 2) in
    let pos = ref 0 in
    while !pos < len do
      if need_to_escape x.[!pos] then (
        Buffer.add_char res '\\' ;
        Buffer.add_char res (escape_char x.[!pos]) )
      else Buffer.add_char res x.[!pos] ;
      incr pos
    done ;
    Buffer.contents res

  let local_to_string = function
    | `String x ->
      Fmt.strf "%a" Fmt.(using escape string) x
    | `Dot_string l ->
      Fmt.strf "%a" Fmt.(list ~sep:(const string ".") string) l

  let to_string x = match x.rest with
    | [] ->
      Fmt.strf "<%s@%s>" (local_to_string x.local) (Domain.to_string x.domain)
    | rest ->
      Fmt.strf "<%a:%s@%s>"
        Fmt.(list ~sep:(const string ",")
               (prefix
                  (const string "@")
                  (using Domain.to_string string))) rest
        (local_to_string x.local)
        (Domain.to_string x.domain)
end
