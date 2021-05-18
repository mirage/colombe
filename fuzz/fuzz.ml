open Crowbar

let ( <.> ) f g x = f (g x)

let char_from_alphabet alphabet =
  map [ range (String.length alphabet) ] (String.make 1 <.> String.get alphabet)

let string_from_alphabet alphabet len =
  let rec go acc = function
    | 0 -> concat_gen_list (const "") acc
    | n -> go (char_from_alphabet alphabet :: acc) (pred n) in
  go [] len

let alphabet_from_predicate predicate =
  let len =
    let rec go acc = function
      | 0 -> if predicate (Char.unsafe_chr 0) then acc + 1 else acc
      | n ->
          let acc = if predicate (Char.unsafe_chr n) then acc + 1 else acc in
          go acc (n - 1) in
    go 0 255 in
  let res = Bytes.create len in
  let rec go idx = function
    | 0 ->
        if predicate (Char.unsafe_chr 0)
        then Bytes.unsafe_set res idx (Char.unsafe_chr 0)
    | n ->
        if predicate (Char.unsafe_chr n)
        then Bytes.unsafe_set res idx (Char.unsafe_chr n) ;
        let idx = if predicate (Char.unsafe_chr n) then idx + 1 else idx in
        go idx (n - 1) in
  go 0 255 ;
  Bytes.unsafe_to_string res

let ldh_str =
  alphabet_from_predicate
    Colombe.Domain.Decoder.(is_alpha or is_digit or is_dash)

let ldh_str =
  map [ dynamic_bind (range ~min:1 78) (string_from_alphabet ldh_str) ]
  @@ fun ldh_str ->
  if ldh_str.[String.length ldh_str - 1] = '-' then bad_test () else ldh_str

let sub_domain =
  map [ ldh_str ] @@ fun sub_domain ->
  if sub_domain.[0] = '-' then bad_test () else sub_domain

let domain =
  map [ list1 sub_domain ] @@ fun domain -> Colombe.Domain.Domain domain

let let_dig =
  alphabet_from_predicate Colombe.Domain.Decoder.(is_alpha or is_digit)

let dcontent = alphabet_from_predicate Colombe.Domain.Decoder.is_dcontent

let extension =
  map
    [ ldh_str; dynamic_bind (range ~min:1 78) (string_from_alphabet dcontent) ]
  @@ fun ldh_str value -> Colombe.Domain.Extension (ldh_str, value)

let ipv4 =
  map [ bytes ] (fun input ->
      match Ipaddr.V4.of_string input with
      | Ok v -> Colombe.Domain.IPv4 v
      | Error _ -> bad_test ())

let ipv6 =
  map [ bytes ] (fun input ->
      match Ipaddr.V6.of_string input with
      | Ok v -> Colombe.Domain.IPv6 v
      | Error _ -> bad_test ())

let domain_and_address_literal = choose [ ipv4; ipv6; extension; domain ]

let esmtp_keyword =
  alphabet_from_predicate Colombe.Domain.Decoder.(is_alpha or is_digit)

let esmtp_keyword =
  dynamic_bind (range ~min:1 78) (string_from_alphabet esmtp_keyword)

let esmtp_param =
  alphabet_from_predicate (function
    | '\033' .. '\060' | '\062' .. '\126' -> true
    | _ -> false)

let esmtp_param =
  dynamic_bind (range ~min:1 78) (string_from_alphabet esmtp_param)

let mail_parameter =
  map [ esmtp_keyword; option esmtp_param ] @@ fun k v -> (k, v)

let mail_parameters = list mail_parameter

let atext = alphabet_from_predicate Colombe.Path.Decoder.is_atext

let atext = dynamic_bind (range ~min:1 78) (string_from_alphabet atext)

let local = map [ list1 atext ] @@ fun lst -> `Dot_string lst
(* TODO: [`String] *)

let path =
  map [ local; domain_and_address_literal; list domain ]
  @@ fun local domain rest -> { Colombe.Path.local; domain; rest }

let reverse_path = option path

let forward_path =
  choose
    [
      const Colombe.Forward_path.Postmaster;
      map [ domain ] (fun domain -> Colombe.Forward_path.Domain domain);
      map [ path ] (fun path -> Colombe.Forward_path.Forward_path path);
    ]

let everything_expect_crlf = function '\r' | '\n' -> false | _ -> true

let everything_expect_crlf = alphabet_from_predicate everything_expect_crlf

let everything_expect_crlf =
  dynamic_bind (range ~min:1 78) (string_from_alphabet everything_expect_crlf)

let command =
  choose
    [
      map [ domain_and_address_literal ] (fun domain -> `Hello domain);
      map [ reverse_path; mail_parameters ] (fun reverse_path mail_parameters ->
          `Mail (reverse_path, mail_parameters));
      map [ forward_path; mail_parameters ] (fun forward_path mail_parameters ->
          `Recipient (forward_path, mail_parameters));
      map [ everything_expect_crlf ] (fun arg -> `Expand arg);
      const `Data;
      map [ option everything_expect_crlf ] (fun arg -> `Help arg);
      map [ option everything_expect_crlf ] (fun arg -> `Noop arg);
      map [ everything_expect_crlf ] (fun arg -> `Verify arg);
      const `Reset;
      const `Quit;
    ]

let code = range ~min:200 600

let line = everything_expect_crlf

let response = map [ code; list1 line ] @@ Colombe.Reply.v

let failf fmt = Fmt.kstrf fail fmt

let pp_chr =
  Fmt.using (function '\032' .. '\126' as x -> x | _ -> '.') Fmt.char

let pp_scalar :
    type buffer.
    get:(buffer -> int -> char) -> length:(buffer -> int) -> buffer Fmt.t =
 fun ~get ~length ppf b ->
  let l = length b in
  for i = 0 to l / 16 do
    Fmt.pf ppf "%08x: " (i * 16) ;
    let j = ref 0 in
    while !j < 16 do
      if (i * 16) + !j < l
      then Fmt.pf ppf "%02x" (Char.code @@ get b ((i * 16) + !j))
      else Fmt.pf ppf "  " ;
      if !j mod 2 <> 0 then Fmt.pf ppf " " ;
      incr j
    done ;
    Fmt.pf ppf "  " ;
    j := 0 ;
    while !j < 16 do
      if (i * 16) + !j < l
      then Fmt.pf ppf "%a" pp_chr (get b ((i * 16) + !j))
      else Fmt.pf ppf " " ;
      incr j
    done ;
    Fmt.pf ppf "@\n"
  done

let pp_string = pp_scalar ~get:String.get ~length:String.length

let never_raise0 () =
  add_test ~name:"never raise (request)" [ bytes ] @@ fun input ->
  match Colombe.Request.Decoder.of_string input with
  | Ok _ | Error _ -> ()
  | exception exn -> failf "Got %s." (Printexc.to_string exn)

let never_raise1 () =
  add_test ~name:"never raise (reply)" [ bytes ] @@ fun input ->
  match Colombe.Reply.Decoder.of_string input with
  | Ok _ | Error _ -> ()
  | exception exn -> failf "Got %s." (Printexc.to_string exn)

let iso0_request () =
  add_test ~name:"x = encoder(decoder(x)) (request)" [ bytes ] @@ fun input ->
  let pos = ref 0 in
  match Colombe.Request.Decoder.of_string_raw input pos with
  | Error _ -> ()
  | Ok v ->
  match Colombe.Request.Encoder.to_string v with
  | Ok raw ->
      check_eq ~pp:pp_string ~eq:String.equal ~cmp:String.compare raw
        (String.sub input 0 !pos)
  | Error err -> failf "Got an error: %a." Colombe.Encoder.pp_error err

let iso0_reply () =
  add_test ~name:"x = encoder(decoder(x)) (reply)" [ bytes ] @@ fun input ->
  let pos = ref 0 in
  match Colombe.Reply.Decoder.of_string_raw input pos with
  | Error _ -> ()
  | Ok v ->
  match Colombe.Reply.Encoder.to_string v with
  | Ok raw ->
      check_eq ~pp:pp_string ~eq:String.equal ~cmp:String.compare raw
        (String.sub input 0 !pos)
  | Error err -> failf "Got an error: %a." Colombe.Encoder.pp_error err

let iso1_request () =
  add_test ~name:"x = decoder(encoder(x)) (request)" [ command ] @@ fun v ->
  match Colombe.Request.Encoder.to_string v with
  | Error err ->
      failf "Got an error while encoding: %a." Colombe.Encoder.pp_error err
  | Ok raw ->
  match Colombe.Request.Decoder.of_string raw with
  | Ok v' -> check_eq ~pp:Colombe.Request.pp ~eq:Colombe.Request.equal v v'
  | Error err ->
      failf "Got an error while decoding: %a." Colombe.Request.Decoder.pp_error
        err

let iso1_reply () =
  add_test ~name:"x = decoder(encoder(x)) (reply)" [ response ] @@ fun v ->
  match Colombe.Reply.Encoder.to_string v with
  | Error err ->
      failf "Got an error while encoding: %a." Colombe.Encoder.pp_error err
  | Ok raw ->
  match Colombe.Reply.Decoder.of_string raw with
  | Ok v' -> check_eq ~pp:Colombe.Reply.pp ~eq:Colombe.Reply.equal v v'
  | Error err ->
      failf "Got an error while decoding: %a." Colombe.Reply.Decoder.pp_error
        err

let () =
  never_raise0 () ;
  never_raise1 () ;
  iso0_request () ;
  iso1_request () ;
  iso0_reply () ;
  iso1_reply ()
