module Domain = struct
  type t =
    | IPv4 of Ipaddr.V4.t
    | IPv6 of Ipaddr.V6.t
    | Extension of string * string
    | Domain of string list

  let pp ppf = function
    | IPv4 ipv4 -> Ipaddr.V4.pp ppf ipv4
    | IPv6 ipv6 -> Ipaddr.V6.pp ppf ipv6
    | Extension (k, v) -> Fmt.pf ppf "%s:%s" k v
    | Domain l -> Fmt.pf ppf "%a" Fmt.(list ~sep:(const string ".") string) l

  open Angstrom

  let ( or ) a b = fun x -> a x || b x
  let is_alpha = function 'a' .. 'z' | 'A' .. 'Z' -> true | _ -> false
  let is_digit = function '0' .. '9' -> true | _ -> false
  let is_dash = (=) '-'

  let let_dig = satisfy (is_alpha or is_digit)

  let ldh_str =
    take_while (is_alpha or is_digit or is_dash)
    >>= fun pre -> let_dig
    >>| fun lst -> String.concat "" [ pre ; String.make 1 lst ]

  let sub_domain =
    let_dig
    >>= fun pre -> option "" ldh_str
    >>| fun lst -> String.concat "" [ String.make 1 pre; lst ]

  let domain =
    sub_domain
    >>= fun x -> many (char '.' *> sub_domain)
    >>| fun r -> Domain (x :: r)

  (* From Mr. MIME. *)

  let is_dcontent = function
    | '\033' .. '\090' | '\094' .. '\126' -> true
    | _ -> false

  let ipv4_address_literal =
    Unsafe.take_while1 is_dcontent (fun buf ~off ~len ->
        let raw = Bigstringaf.substring buf ~off ~len in
        let pos = ref 0 in
        try
          let res = Ipaddr.V4.of_string_raw raw pos in
          if !pos = len then Some res else None
        with Ipaddr.Parse_error _ -> None )
    >>= function Some v -> return v | None -> fail "ipv4_address_literal"

  let ipv6_addr =
    Unsafe.take_while1 is_dcontent (fun buf ~off ~len ->
        let raw = Bigstringaf.substring buf ~off ~len in
        let pos = ref 0 in
        try
          let res = Ipaddr.V6.of_string_raw raw pos in
          if !pos = len then Some res else None
        with Ipaddr.Parse_error _ -> None )
    >>= function Some v -> return v | None -> fail "ipv6_addr"

  let ipv6_address_literal = string "IPv6:" *> ipv6_addr

  let failf fmt = Fmt.kstrf fail fmt

  let ldh_str =
    take_while1 (function
        | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '-' -> true
        | _ -> false )
    >>= fun ldh ->
    if String.unsafe_get ldh (String.length ldh - 1) = '-'
    then failf "ldh_str: %s is invalid" ldh
    else return ldh

  let general_address_literal =
    ldh_str <* char ':'
    >>= fun ldh -> take_while1 is_dcontent
    >>| fun value -> Extension (ldh, value)

  let address_literal =
    char '[' *>
    (ipv4_address_literal >>| (fun v -> IPv4 v))
    <|> (ipv6_address_literal >>| fun v -> IPv6 v)
    <|> general_address_literal
    <* char ']'

  let of_string x =
    match parse_string (domain <|> address_literal) x with
    | Ok v -> v
    | Error _ -> Fmt.invalid_arg "Invalid domain: %s" x
end

module Reverse_path = struct
  open Angstrom

  let at_domain = char '@' *> Domain.domain
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
    >>= fun local -> char '@' *> (Domain.domain <|> Domain.address_literal)
    >>| fun domain -> (local, domain)

  type path =
    { local : [ `String of string | `Dot_string of string list ]
    ; domain : Domain.t
    ; rest : Domain.t list }
  and t = path option

  let pp_local ppf = function
    | `String x -> Fmt.(quote string) ppf x
    | `Dot_string l -> Fmt.(list ~sep:(const string ".") string) ppf l

  let pp_path ppf { local; domain; rest; } =
    match rest with
    | [] -> Fmt.pf ppf "<%a@%a>" pp_local local Domain.pp domain
    | rest ->
      Fmt.pf ppf "<%a:%a@%a>"
        Fmt.(list ~sep:(const string ",") (prefix (const string "@") Domain.pp)) rest
        pp_local local Domain.pp domain

  let pp = Fmt.option pp_path

  let path =
    char '<' *> option [] (a_d_l <* char ':')
    >>= fun rest -> mailbox
    >>| fun (local, domain) -> { local; domain; rest; }

  let reverse_path = (path >>| fun t -> Some t) <|> (string "<>" *> return None)

  let esmtp_keyword =
    satisfy Domain.(is_alpha or is_digit)
    >>= fun pre -> take_while Domain.(is_alpha or is_digit or is_dash)
    >>| fun lst -> String.concat "" [ String.make 1 pre ; lst ]

  let esmtp_value = take_while1 (function '\033' .. '\060' | '\062' .. '\126' -> true | _ -> false)

  let esmtp_param =
    esmtp_keyword
    >>= fun key -> (option None (char '=' *> esmtp_value >>| fun x -> Some x))
    >>| fun value -> (key, value)

  let mail_parameters = esmtp_param >>= fun x -> many (char ' ' *> esmtp_param) >>| fun r -> x :: r

  let of_string x =
    let p =
      reverse_path
      >>= fun reverse_path -> option [] (char ' ' *> mail_parameters)
      >>| fun parameters -> (reverse_path, parameters) in
    match parse_string p x with
    | Ok v -> v
    | Error _ -> Fmt.invalid_arg "Invalid reverse-path: %s" x
end

module Forward_path = struct
  open Angstrom

  type t =
    | Postmaster
    | Domain of Domain.t
    | Forward_path of forward_path
  and forward_path = Reverse_path.path

  let forward_path = Reverse_path.path
  let mail_parameters = Reverse_path.mail_parameters

  let pp ppf = function
    | Postmaster -> Fmt.string ppf "<Postmaster>"
    | Domain domain -> Fmt.pf ppf "<Postmaster@%a>" Domain.pp domain
    | Forward_path path -> Reverse_path.pp_path ppf path

  let of_string x =
    let p =
      (string "<Postmaster@" *> Domain.domain >>| fun domain -> Domain domain)
      <|> (string "Postmaster>" *> return Postmaster)
      <|> (forward_path >>| fun path -> Forward_path path) in
    let p = p
      >>= fun forward_path -> option [] (char ' ' *> mail_parameters)
      >>| fun parameters -> (forward_path, parameters) in
    match parse_string p x with
    | Ok v -> v
    | Error _ -> Fmt.invalid_arg "Invalid forward-path: %s" x
end

module Request = struct
  type t =
    [ `Hello of Domain.t
    | `Mail of Reverse_path.t * (string * string option) list
    | `Recipient of Forward_path.t * (string * string option) list
    | `Expand of string
    | `Data of string
    | `Help of string option
    | `Noop of string option
    | `Verify of string
    | `Reset
    | `Quit ]

  let pp ppf = function
    | `Hello domain ->
      Fmt.pf ppf "(Hello @[<hov>%a@])" Domain.pp domain
    | `Mail (reverse_path, parameters) ->
      Fmt.pf ppf "(Mail { @[<hov>path= %a;@ parameters= %a;@] })"
        Reverse_path.pp reverse_path
        Fmt.(Dump.list (pair string (option string))) parameters
    | `Recipient (forward_path, parameters) ->
      Fmt.pf ppf "(Recipient { @[<hov>path= %a;@ parameters= %a;@] })"
        Forward_path.pp forward_path
        Fmt.(Dump.list (pair string (option string))) parameters
    | `Expand data -> Fmt.pf ppf "(Expand %s)" data
    | `Data data -> Fmt.pf ppf "(Data %s)" data
    | `Help data -> Fmt.pf ppf "(Help %a)" Fmt.(option string) data
    | `Noop data -> Fmt.pf ppf "(Noop %a)" Fmt.(option string) data
    | `Verify data -> Fmt.pf ppf "(Verify %s)" data
    | `Reset -> Fmt.string ppf "Reset"
    | `Quit -> Fmt.string ppf "Quit"
end

module Decoder = struct
  type decoder =
    { buffer : Bytes.t
    ; mutable pos : int
    ; mutable max : int }

  let io_buffer_size = 65536

  let decoder () =
    { buffer= Bytes.create io_buffer_size
    ; pos= 0
    ; max= 0 }

  let decoder_from x =
    let max = String.length x in
    let buffer = Bytes.of_string x in
    { buffer; pos= 0; max; }

  type error =
    | End_of_input
    | Expected_char of char
    | Unexpected_char of char
    | Expected_string of string
    | Invalid_command of string
    | Expected_eol
    | No_enough_space
    | Assert_predicate of (char -> bool)

  let pp_error ppf = function
    | End_of_input -> Fmt.string ppf "End_of_input"
    | Expected_char chr -> Fmt.pf ppf "(Expected_char %02x)" (Char.code chr)
    | Unexpected_char chr -> Fmt.pf ppf "(Unexpected_char %02x)" (Char.code chr)
    | Expected_string s -> Fmt.pf ppf "(Expected_string %s)" s
    | Invalid_command s -> Fmt.pf ppf "(Invalid_command %s)" s
    | Expected_eol -> Fmt.string ppf "Expected_eol"
    | No_enough_space -> Fmt.string ppf "No_enough_space"
    | Assert_predicate _ -> Fmt.string ppf "(Assert_predicate #predicate)"

  type 'v state =
    | Ok of 'v
    | Read of { buffer : Bytes.t; off : int; len : int; continue : int -> 'v state }
    | Error of info
  and info = { error : error; buffer : Bytes.t; committed : int }

  exception Leave of info

  let return (type v) (v : v) _ : v state = Ok v

  let safe k decoder : 'v state =
    try k decoder with Leave info -> Error info

  let end_of_input decoder = decoder.max

  let peek_char decoder =
    if decoder.pos < end_of_input decoder
    then Some (Bytes.unsafe_get decoder.buffer decoder.pos)
    else None
    (* XXX(dinosaure): in [agnstrom] world, [peek_char] should try to read input
       again. However, SMTP is a line-directed protocol where we can ensure to
       have the full line at the top (with a queue) instead to have a
       systematic check (which slow-down the process). *)

  let leave_with (decoder : decoder) error =
    raise (Leave { error; buffer= decoder.buffer; committed= decoder.pos; })

  let junk_char decoder =
    if decoder.pos < end_of_input decoder
    then decoder.pos <- decoder.pos + 1
    else leave_with decoder End_of_input

  let char chr decoder =
    match peek_char decoder with
    | Some chr' ->
      if not (Char.equal chr chr') then leave_with decoder (Expected_char chr) ;
      junk_char decoder
    | None -> leave_with decoder End_of_input

  let string str decoder =
    let idx = ref 0 in
    let len = String.length str in
    while decoder.pos + !idx < end_of_input decoder
          && !idx < len
          && Char.equal
            (Bytes.unsafe_get decoder.buffer (decoder.pos + !idx))
            (String.unsafe_get str !idx)
    do incr idx done ;
    if !idx = len then () else leave_with decoder (Expected_string str)

  (* According to RFC 5321. *)

  let trie = Trie.empty
  let trie = Trie.add "EHLO" `Hello trie
  let trie = Trie.add "HELO" `Hello trie
  let trie = Trie.add "MAIL" `Mail trie
  let trie = Trie.add "RCPT" `Recipient trie
  let trie = Trie.add "DATA" `Data trie
  let trie = Trie.add "RSET" `Reset trie
  let trie = Trie.add "VRFY" `Verify trie
  let trie = Trie.add "EXPN" `Expand trie
  let trie = Trie.add "HELP" `Help trie
  let trie = Trie.add "NOOP" `Noop trie
  let trie = Trie.add "QUIT" `Quit trie

  let take_while_eol decoder =
    let idx = ref decoder.pos in
    let has_cr = ref false in
    while !idx < end_of_input decoder
          && not (Char.equal '\n' (Bytes.unsafe_get decoder.buffer !idx) && !has_cr)
    do has_cr := Char.equal '\r' (Bytes.unsafe_get decoder.buffer !idx) ; incr idx done ;
    if !idx < end_of_input decoder
    && Char.equal '\n' (Bytes.unsafe_get decoder.buffer !idx)
    && !has_cr
    then ( assert (!idx - decoder.pos >= 2) ; decoder.buffer, decoder.pos, !idx - decoder.pos )
    else leave_with decoder Expected_eol

  let command decoder =
    let pos = decoder.pos in
    let len = ref 0 in
    let advance n =
      let rec go rest =
        if rest = 0 then Some (decoder.buffer, pos, !len)
        else match peek_char decoder with
          | Some _ ->
            junk_char decoder ;
            incr len ;
            go (n - 1)
          | None -> None in
      if n <= 0 then None else go n in
    match Trie.find advance trie with
    | command -> command
    | exception Not_found ->
      let command = Bytes.sub_string decoder.buffer pos !len in
      leave_with decoder (Invalid_command command)

  let hello decoder =
    char ' ' decoder ;
    let raw_crlf, off, len = take_while_eol decoder in
    let domain = Domain.of_string (Bytes.sub_string raw_crlf off (len - 2)) in
    return (`Hello domain) decoder

  let mail decoder =
    let raw_crlf, off, len = take_while_eol decoder in
    let reverse_path = Reverse_path.of_string (Bytes.sub_string raw_crlf off (len - 2)) in
    return (`Mail reverse_path) decoder

  let recipient decoder =
    let raw_crlf, off, len = take_while_eol decoder in
    let forward_path = Forward_path.of_string (Bytes.sub_string raw_crlf off (len - 2)) in
    return (`Recipient forward_path) decoder

  let data decoder =
    let raw_crlf, off, len = take_while_eol decoder in
    let data = Bytes.sub_string raw_crlf off (len - 2) in
    return (`Data data) decoder

  let crlf = fun decoder -> string "\r\n" decoder

  let help decoder =
    match peek_char decoder with
    | Some ' ' ->
      junk_char decoder ;
      let raw_crlf, off, len = take_while_eol decoder in
      let v = `Help (Some (Bytes.sub_string raw_crlf off (len - 2))) in
      return v decoder
    | Some chr -> leave_with decoder (Unexpected_char chr)
    | None -> return (`Help None) decoder

  let noop decoder =
    match peek_char decoder with
    | Some ' ' ->
      junk_char decoder ;
      let raw_crlf, off, len = take_while_eol decoder in
      let v = `Noop (Some (Bytes.sub_string raw_crlf off (len - 2))) in
      return v decoder
    | Some chr -> leave_with decoder (Unexpected_char chr)
    | None -> return (`Noop None) decoder

  let request decoder =
    match command decoder with
    | `Hello -> hello decoder
    | `Mail ->
      string " FROM:" decoder ;
      mail decoder
    | `Recipient ->
      string " TO:" decoder ;
      recipient decoder
    | `Data -> data decoder
    | `Reset -> crlf decoder ; return `Reset decoder
    | `Verify ->
      char ' ' decoder ;
      let raw_crlf, off, len = take_while_eol decoder in
      let v = `Verify (Bytes.sub_string raw_crlf off (len - 2)) in
      return v decoder
    | `Expand ->
      char ' ' decoder ;
      let raw_crlf, off, len = take_while_eol decoder in
      let v = `Expand (Bytes.sub_string raw_crlf off (len - 2)) in
      return v decoder
    | `Help -> help decoder
    | `Noop -> noop decoder
    | `Quit -> crlf decoder ; return `Quit decoder

  let at_least_one_line decoder =
    let pos = ref decoder.pos in
    let has_cr = ref false in
    while !pos < decoder.max
          && not (Char.equal '\n' (Bytes.unsafe_get decoder.buffer !pos) && !has_cr)
    do has_cr := Char.equal '\r' (Bytes.unsafe_get decoder.buffer !pos) ; incr pos done ;
    (!pos < decoder.max
     && Char.equal '\n' (Bytes.unsafe_get decoder.buffer !pos)
     && !has_cr)

  let prompt k decoder =
    if decoder.pos > 0
    then (* XXX(dinosaure): compress *)
      (let rest = decoder.max - decoder.pos in
       Bytes.unsafe_blit decoder.buffer decoder.pos decoder.buffer 0 rest ;
       decoder.max <- rest ;
       decoder.pos <- 0 ) ;
    let rec go off =
      if off = Bytes.length decoder.buffer
      then Error { error= No_enough_space; buffer= decoder.buffer; committed= decoder.pos; }
      else if not (at_least_one_line decoder)
      then Read { buffer= decoder.buffer
                ; off
                ; len= Bytes.length decoder.buffer - off
                ; continue= (fun len -> go (off + len)) }
      else
        ( decoder.max <- off ;
          safe k decoder ) in
    go decoder.max

  let request decoder = prompt request decoder

  let of_string x =
    let decoder = decoder_from x in
    let go x : (Request.t, error) result = match x with
      | Read _ -> Error End_of_input
      | Error { error; _ } ->  Error error
      | Ok v -> Ok v in
    go (request decoder)
end
