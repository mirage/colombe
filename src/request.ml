let () = Printexc.record_backtrace true

module Option = struct
  let equal eq a b =
    match a, b with
    | Some a, Some b -> eq a b
    | None, None -> true
    | _, _ -> false
end

module Request = struct
  type t =
    [ `Hello of Domain.t
    | `Mail of Reverse_path.t * (string * string option) list
    | `Recipient of Forward_path.t * (string * string option) list
    | `Expand of string
    | `Data
    | `Help of string option
    | `Noop of string option
    | `Verify of string
    | `Reset
    | `Quit ]

  let equal_parameters a b =
    let a = List.sort (fun (ka, _) (kb, _) -> String.compare ka kb) a in
    let b = List.sort (fun (ka, _) (kb, _) -> String.compare ka kb) b in
    let equal_values a b = match a, b with
      | Some a, Some b -> String.equal a b
      | None, None -> true
      | _, _ -> false in
    try
      List.for_all2 (fun (ka, va) (kb, vb) -> String.equal ka kb && equal_values va vb)
        a b
    with _ -> false

  let equal a b = match a, b with
    | `Hello a, `Hello b -> Domain.equal a b
    | `Mail (a, pa), `Mail (b, pb) ->
      Reverse_path.equal a b && equal_parameters pa pb
    | `Recipient (a, pa), `Recipient (b, pb) ->
      Forward_path.equal a b && equal_parameters pa pb
    | `Expand a, `Expand b -> String.equal a b
    | `Data, `Data -> true
    | `Help a, `Help b -> Option.equal String.equal a b
    | `Noop a, `Noop b -> Option.equal String.equal a b
    | `Verify a, `Verify b -> String.equal a b
    | `Reset, `Reset -> true
    | `Quit, `Quit -> true
    | _, _ -> false


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
    | `Data -> Fmt.string ppf "Data"
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
    | Expected_eol_or_space
    | No_enough_space
    | Assert_predicate of (char -> bool)

  let pp_error ppf = function
    | End_of_input -> Fmt.string ppf "End_of_input"
    | Expected_char chr -> Fmt.pf ppf "(Expected_char %02x)" (Char.code chr)
    | Unexpected_char chr -> Fmt.pf ppf "(Unexpected_char %02x)" (Char.code chr)
    | Expected_string s -> Fmt.pf ppf "(Expected_string %s)" s
    | Invalid_command s -> Fmt.pf ppf "(Invalid_command %s)" s
    | Expected_eol -> Fmt.string ppf "Expected_eol"
    | Expected_eol_or_space -> Fmt.string ppf "Expected_eol_or_space"
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
    (* XXX(dinosaure): in [angstrom] world, [peek_char] should try to read input
       again. However, SMTP is a line-directed protocol where we can ensure to
       have the full line at the top (with a queue) instead to have a
       systematic check (which slow-down the process). *)

  let leave_with (decoder : decoder) error =
    raise (Leave { error; buffer= decoder.buffer; committed= decoder.pos; })

  let string str decoder =
    let idx = ref 0 in
    let len = String.length str in
    while decoder.pos + !idx < end_of_input decoder
          && !idx < len
          && Char.equal
            (Bytes.unsafe_get decoder.buffer (decoder.pos + !idx))
            (String.unsafe_get str !idx)
    do incr idx done ;
    if !idx = len then decoder.pos <- decoder.pos + len else leave_with decoder (Expected_string str)

  (* According to RFC 5321. *)

  let trie = Hashtbl.create 16
  let () = Hashtbl.add trie "EHLO" `Hello
  let () = Hashtbl.add trie "HELO" `Hello
  let () = Hashtbl.add trie "MAIL" `Mail
  let () = Hashtbl.add trie "RCPT" `Recipient
  let () = Hashtbl.add trie "DATA" `Data
  let () = Hashtbl.add trie "RSET" `Reset
  let () = Hashtbl.add trie "VRFY" `Verify
  let () = Hashtbl.add trie "EXPN" `Expand
  let () = Hashtbl.add trie "HELP" `Help
  let () = Hashtbl.add trie "NOOP" `Noop
  let () = Hashtbl.add trie "QUIT" `Quit

  let peek_while_eol decoder =
    let idx = ref decoder.pos in
    let chr = ref '\000' in
    let has_cr = ref false in

    while !idx < end_of_input decoder
          && ( chr := Bytes.unsafe_get decoder.buffer !idx
             ; not (!chr == '\n' && !has_cr) )
    do has_cr := !chr == '\r' ; incr idx done ;

    if !idx < end_of_input decoder && !chr == '\n' && !has_cr
    then ( assert (!idx + 1 - decoder.pos > 1) ; decoder.buffer, decoder.pos, !idx + 1 - decoder.pos )
    else leave_with decoder Expected_eol

  let peek_while_eol_or_space decoder =
    let idx = ref decoder.pos in
    let chr = ref '\000' in
    let has_cr = ref false in

    while !idx < end_of_input decoder
          && ( chr := Bytes.unsafe_get decoder.buffer !idx
             ; not (!chr = '\n' && !has_cr) && !chr <> ' ')
    do has_cr := !chr = '\r' ; incr idx done ;

    if !idx < end_of_input decoder && ((!chr = '\n' && !has_cr) || (!chr = ' '))
    then ( decoder.buffer, decoder.pos, !idx + 1 - decoder.pos )
    else leave_with decoder Expected_eol_or_space

  let command decoder =
    let raw, off, len = peek_while_eol_or_space decoder in
    let command = match Bytes.unsafe_get raw (off + len - 1) with
      | ' ' -> Bytes.sub_string raw off (len - 1)
      | '\n' -> Bytes.sub_string raw off (len - 2)
      | _ -> assert false (* end with LF or SPACE *)in
    match Hashtbl.find trie command with
    | command ->
      decoder.pos <- decoder.pos + len ; command
    | exception Not_found ->
      leave_with decoder (Invalid_command command)

  let hello (decoder : decoder) =
    let raw_crlf, off, len = peek_while_eol decoder in
    let domain = Domain.Parser.of_string (Bytes.sub_string raw_crlf off (len - 2)) in
    decoder.pos <- decoder.pos + len ; return (`Hello domain) decoder

  let mail decoder =
    let raw_crlf, off, len = peek_while_eol decoder in
    let reverse_path =
      Reverse_path.Parser.of_string (Bytes.sub_string raw_crlf off (len - 2)) in
    decoder.pos <- decoder.pos + len ; return (`Mail reverse_path) decoder

  let recipient decoder =
    let raw_crlf, off, len = peek_while_eol decoder in
    let forward_path =
      Forward_path.Parser.of_string (Bytes.sub_string raw_crlf off (len - 2)) in
    decoder.pos <- decoder.pos + len ; return (`Recipient forward_path) decoder

  let help decoder =
    match peek_char decoder with
    | Some _ ->
      let raw_crlf, off, len = peek_while_eol decoder in
      let v = `Help (Some (Bytes.sub_string raw_crlf off (len - 2))) in
      decoder.pos <- decoder.pos + len ; return v decoder
    | None -> return (`Help None) decoder

  let noop decoder =
    match peek_char decoder with
    | Some _ ->
      let raw_crlf, off, len = peek_while_eol decoder in
      let v = `Noop (Some (Bytes.sub_string raw_crlf off (len - 2))) in
      decoder.pos <- decoder.pos + len ; return v decoder
    | None -> return (`Noop None) decoder

  let request decoder =
    match command decoder with
    | `Hello -> hello decoder
    | `Mail ->
      string "FROM:" decoder ;
      mail decoder
    | `Recipient ->
      string "TO:" decoder ;
      recipient decoder
    | `Data -> (* assert (decoder.pos = end_of_input decoder) ; *) return `Data decoder
    | `Reset -> (* assert (decoder.pos = end_of_input decoder) ; *) return `Reset decoder
    | `Verify ->
      let raw_crlf, off, len = peek_while_eol decoder in
      let v = `Verify (Bytes.sub_string raw_crlf off (len - 2)) in
      decoder.pos <- decoder.pos + len ; return v decoder
    | `Expand ->
      let raw_crlf, off, len = peek_while_eol decoder in
      let v = `Expand (Bytes.sub_string raw_crlf off (len - 2)) in
      decoder.pos <- decoder.pos + len ; return v decoder
    | `Help -> help decoder
    | `Noop -> noop decoder
    | `Quit -> (* assert (decoder.pos = end_of_input decoder) ; *) return `Quit decoder

  let at_least_one_line decoder =
    let pos = ref decoder.pos in
    let chr = ref '\000' in
    let has_cr = ref false in
    while !pos < decoder.max
          &&  ( chr := Bytes.unsafe_get decoder.buffer !pos
              ; not (!chr = '\n' && !has_cr) )
    do has_cr := !chr = '\r' ; incr pos done ;
    !pos < decoder.max
    && !chr = '\n'
    && !has_cr

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

  let request decoder =
    if at_least_one_line decoder
    then safe request decoder
    else prompt request decoder

  let of_string x =
    let decoder = decoder_from x in
    let go x : (Request.t, error) result = match x with
      | Read _ -> Error End_of_input
      | Error { error; _ } ->  Error error
      | Ok v -> Ok v in
    go (request decoder)
end

module Encoder = struct
  type encoder =
    { payload : Bytes.t
    ; mutable pos : int }

  type error = No_enough_space

  let pp_error ppf No_enough_space = Fmt.string ppf "No_enough_space"

  type state =
    | Write of { buffer : Bytes.t
               ; off : int
               ; len : int
               ; continue : int -> state }
    | Error of error
    | Ok

  let io_buffer_size = 65536

  let encoder () =
    { payload= Bytes.create io_buffer_size
    ; pos= 0 }

  exception Leave of error

  let leave_with (_ : encoder) error =
    raise (Leave error)

  let safe k encoder : state =
    try k encoder with Leave error -> Error error

  let flush k0 encoder =
    if encoder.pos > 0
    then
      let rec k1 n =
        if n < encoder.pos
        then Write { buffer= encoder.payload
                   ; off= n
                   ; len= encoder.pos - n
                   ; continue= (fun m -> k1 (n + m)) }
        else ( encoder.pos <- 0 ; k0 encoder ) in
      k1 0
    else k0 encoder

  let write s encoder =
    let max = Bytes.length encoder.payload in
    let go j l encoder =
      let rem = max - encoder.pos in
      let len = if l > rem then rem else l in
      Bytes.blit_string s j encoder.payload encoder.pos len ;
      encoder.pos <- encoder.pos + len ;
      if len < l then leave_with encoder No_enough_space in
    (* XXX(dinosaure): should never appear, but avoid continuation allocation. *)
    go 0 (String.length s) encoder

  let crlf encoder = write "\r\n" encoder

  let hello domain encoder =
    write "EHLO" encoder ; (* TODO: can write HELO. *)
    write " " encoder ;
    write (Domain.Encoder.to_string domain) encoder ;
    crlf encoder

  let write_parameters parameters encoder =
    let rec go = function
      | [] -> ()
      | (k, Some v) :: r ->
        write " " encoder ;
        write k encoder ;
        write "=" encoder ;
        write v encoder ;
        go r
      | (k, None) :: r ->
        write " " encoder ;
        write k encoder ;
        go r in
    go parameters

  let mail reverse_path parameters encoder =
    write "MAIL FROM:" encoder ;
    write (Reverse_path.Encoder.to_string reverse_path) encoder ;
    match parameters with
    | [] -> crlf encoder
    | parameters ->
      write_parameters parameters encoder ; crlf encoder

  let recipient forward_path parameters encoder =
    write "RCPT TO:" encoder ;
    write (Forward_path.Encoder.to_string forward_path) encoder ;
    match parameters with
    | [] -> crlf encoder
    | parameters ->
      write_parameters parameters encoder ; crlf encoder

  let verify argument encoder =
    write "VRFY " encoder ;
    write argument encoder ;
    crlf encoder

  let expand argument encoder =
    write "EXPN " encoder ;
    write argument encoder ;
    crlf encoder

  let help argument encoder =
    write "HELP" encoder ;
    match argument with
    | Some argument ->
      write " " encoder ;
      write argument encoder ;
      crlf encoder
    | None -> crlf encoder

  let noop argument encoder =
    write "NOOP" encoder ;
    match argument with
    | Some argument ->
      write " " encoder ;
      write argument encoder ;
      crlf encoder
    | None -> crlf encoder

  let request command encoder =
    match command with
    | `Hello domain -> hello domain encoder
    | `Mail (reverse_path, parameters) ->
      mail reverse_path parameters encoder
    | `Recipient (forward_path, parameters) ->
      recipient forward_path parameters encoder
    | `Data -> write "DATA" encoder ; crlf encoder
    | `Reset -> write "RSET" encoder ; crlf encoder
    | `Verify argument ->
      verify argument encoder
    | `Expand argument ->
      expand argument encoder
    | `Help argument ->
      help argument encoder
    | `Noop argument ->
      noop argument encoder
    | `Quit -> write "QUIT" encoder ; crlf encoder

  let request command encoder =
    let k encoder = request command encoder ; Ok in
    match safe k encoder with
    | Write _ -> assert false (* XXX(dinosaure): request never calls [flush]. *)
    | v -> flush (fun _ -> v) encoder

  let to_string x =
    let encoder = encoder () in
    let res = Buffer.create 16 in
    let rec go x : (string, error) result = match x with
      | Write { buffer; off; len; continue } ->
        Buffer.add_subbytes res buffer off len ;
        go (continue len)
      | Error error -> Error error
      | Ok -> Ok (Buffer.contents res) in
    go (request x encoder)
end
