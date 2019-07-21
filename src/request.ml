module Option = struct
  let equal eq a b =
    match a, b with
    | Some a, Some b -> eq a b
    | None, None -> true
    | _, _ -> false
end

type t =
  [ `Hello of Domain.t
  | `Mail of Reverse_path.t * (string * string option) list
  | `Recipient of Forward_path.t * (string * string option) list
  | `Expand of string
  | `Data
  | `Data_end
  | `Help of string option
  | `Noop of string option
  | `Verify of string
  | `Reset
  | `Quit
  | `Extension of Rfc1869.t ]

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
  | `Data_end, `Data_end -> true
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
  | `Data_end -> Fmt.string ppf "<dot>"
  | `Help data -> Fmt.pf ppf "(Help %a)" Fmt.(option string) data
  | `Noop data -> Fmt.pf ppf "(Noop %a)" Fmt.(option string) data
  | `Verify data -> Fmt.pf ppf "(Verify %s)" data
  | `Reset -> Fmt.string ppf "Reset"
  | `Quit -> Fmt.string ppf "Quit"
  | `Extension _ -> Fmt.pf ppf "(Extension)"

module Decoder = struct
  include Decoder

  (* According to RFC 5321. *)

  let trie = Hashtbl.create 16
  let () = Hashtbl.add trie "EHLO" `Hello
  let () = Hashtbl.add trie "HELO" `Hello
  let () = Hashtbl.add trie "MAIL" `Mail
  let () = Hashtbl.add trie "RCPT" `Recipient
  let () = Hashtbl.add trie "DATA" `Data
  let () = Hashtbl.add trie "." `Data_end
  let () = Hashtbl.add trie "RSET" `Reset
  let () = Hashtbl.add trie "VRFY" `Verify
  let () = Hashtbl.add trie "EXPN" `Expand
  let () = Hashtbl.add trie "HELP" `Help
  let () = Hashtbl.add trie "NOOP" `Noop
  let () = Hashtbl.add trie "QUIT" `Quit

  let extensions : (string, Rfc1869.t) Hashtbl.t = Hashtbl.create 16
  (* XXX(dinosaure): when we do [Rfc1869.inj], we must populate this hashtbl. *)

  let command decoder =
    let raw, off, len = peek_while_eol_or_space decoder in
    let command = match Bytes.unsafe_get raw (off + len - 1) with
      | ' ' -> Bytes.sub_string raw off (len - 1)
      | '\n' -> Bytes.sub_string raw off (len - 2)
      | _ -> leave_with decoder Expected_eol_or_space in
    match Hashtbl.find trie command with
    | command ->
      decoder.pos <- decoder.pos + len ; command
    | exception Not_found -> match Hashtbl.find extensions command with
      | ext ->
        decoder.pos <- decoder.pos + len
      ; `Extension (command, ext)
      | exception Not_found -> leave_with decoder (Invalid_command command)

  let hello (decoder : decoder) =
    let raw_crlf, off, len = peek_while_eol decoder in
    let domain = Domain.of_string_exn (Bytes.sub_string raw_crlf off (len - 2)) in
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
    | `Data_end -> (* assert (decoder.pos = end_of_input decoder) ; *) return `Data_end decoder
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
    | `Extension (_, _) ->
      let raw_crlf, off, len = peek_while_eol decoder in
      let _ = if len - 2 > 0 then Some (Bytes.sub_string raw_crlf off (len - 2)) else None in
      decoder.pos <- decoder.pos + len ;

      assert false

  let request decoder =
    if at_least_one_line decoder
    then safe request decoder
    else prompt request decoder

  let of_string x =
    let decoder = decoder_from x in
    let go x : (t, error) result = match x with
      | Read _ -> Error End_of_input
      | Error { error; _ } ->  Error error
      | Ok v -> Ok v in
    go (request decoder)

  let of_string_raw x r =
    let decoder = decoder_from x in
    let go x : (t, error) result = match x with
      | Read _ -> Error End_of_input
      | Error { error; _ } ->  Error error
      | Ok v -> r := decoder.pos ; Ok v in
    go (request decoder)
end

module Encoder = struct
  include Encoder

  let crlf encoder = write "\r\n" encoder

  let hello domain encoder =
    write "EHLO" encoder ; (* TODO: can write HELO. *)
    write " " encoder ;
    write (Domain.to_string domain) encoder ;
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
    | `Data_end -> write "." encoder ; crlf encoder
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
    | `Extension ext ->
      match Rfc1869.prj ext with
      | Rfc1869.V (v, (module Ext), _) ->
        ( match Ext.encode v with
          | Rfc1869.Payload { buf; off; len; } ->
            write (Bytes.sub_string buf off len) encoder (* TODO: optimize! *)
          | Rfc1869.Request { verb; args= [] } ->
            write verb encoder ; crlf encoder
          | Rfc1869.Request { verb; args; } ->
            write verb encoder
          ; List.iter (fun arg -> write " " encoder ; write arg encoder) args
          ; crlf encoder )

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
        Buffer.add_substring res buffer off len ;
        go (continue len)
      | Error error -> Error error
      | Ok -> Ok (Buffer.contents res) in
    go (request x encoder)
end
