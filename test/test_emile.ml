let emile_mailbox_of_string str =
  match Emile.of_string str with
  | Ok mailbox -> mailbox
  | Error (`Invalid (_, msg)) -> Alcotest.failf "Invalid emile mailbox: %s" msg

let colombe_path_of_string str =
  match Colombe.Path.of_string str with
  | Ok path -> path
  | Error (`Msg msg) -> Alcotest.failf "Invalid colombe path: %s" msg

let colombe_domain_of_string str = Colombe.Domain.of_string_exn str

let emile_local =
  Alcotest.testable Emile.pp_local (Emile.equal_local ~case_sensitive:true)

let colombe_local =
  let pp ppf = function
    | `Dot_string l ->
        Format.fprintf ppf "%a"
          Format.(
            pp_print_list
              ~pp_sep:(fun ppf () -> pp_print_char ppf '.')
              pp_print_string)
          l
    | `String s -> Format.fprintf ppf "%S" s in
  let equal a b =
    match (a, b) with
    | `Dot_string a, `Dot_string b -> (
        try List.for_all2 String.equal a b with _ -> false)
    | `String a, `String b -> String.equal a b
    | _, _ -> false in
  Alcotest.testable pp equal

let colombe_domain = Alcotest.testable Colombe.Domain.pp Colombe.Domain.equal
let emile_domain = Alcotest.testable Emile.pp_domain Emile.equal_domain
let colombe_path = Alcotest.testable Colombe.Path.pp Colombe.Path.equal

let emile_mailbox =
  Alcotest.testable Emile.pp_mailbox (Emile.equal_mailbox ~case_sensitive:true)

let colombe_forward_path =
  Alcotest.testable Colombe.Forward_path.pp Colombe.Forward_path.equal

let colombe_reverse_path =
  Alcotest.testable Colombe.Reverse_path.pp Colombe.Reverse_path.equal

let opt t = Alcotest.option t

let test_local_dot_string () =
  let colombe_l = `Dot_string [ "foo"; "bar" ] in
  let emile_l = Colombe_emile.of_local colombe_l in
  Alcotest.(check emile_local)
    "of_local dot_string"
    [ `Atom "foo"; `Atom "bar" ]
    emile_l ;
  let back = Colombe_emile.to_local emile_l in
  Alcotest.(check colombe_local)
    "to_local dot_string"
    (`Dot_string [ "foo"; "bar" ])
    back

let test_local_string () =
  let colombe_l = `String "foo bar" in
  let emile_l = Colombe_emile.of_local colombe_l in
  Alcotest.(check emile_local) "of_local string" [ `String "foo bar" ] emile_l ;
  let back = Colombe_emile.to_local emile_l in
  Alcotest.(check colombe_local) "to_local string" (`String "foo bar") back

let test_local_single_atom () =
  let colombe_l = `Dot_string [ "user" ] in
  let emile_l = Colombe_emile.of_local colombe_l in
  Alcotest.(check emile_local) "of_local single atom" [ `Atom "user" ] emile_l ;
  let back = Colombe_emile.to_local emile_l in
  Alcotest.(check colombe_local)
    "to_local single atom" (`Dot_string [ "user" ]) back

let test_domain_hostname () =
  let emile_d : Emile.domain = `Domain [ "example"; "com" ] in
  let colombe_d = Colombe_emile.to_domain emile_d in
  Alcotest.(check colombe_domain)
    "to_domain hostname"
    (Colombe.Domain.Domain [ "example"; "com" ])
    colombe_d ;
  let back = Colombe_emile.of_domain colombe_d in
  Alcotest.(check emile_domain)
    "of_domain hostname"
    (`Domain [ "example"; "com" ])
    back

let test_domain_ipv4 () =
  let ip = Ipaddr.V4.of_string_exn "127.0.0.1" in
  let emile_d : Emile.domain = `Addr (Emile.IPv4 ip) in
  let colombe_d = Colombe_emile.to_domain emile_d in
  Alcotest.(check colombe_domain)
    "to_domain ipv4" (Colombe.Domain.IPv4 ip) colombe_d ;
  let back = Colombe_emile.of_domain colombe_d in
  Alcotest.(check emile_domain) "of_domain ipv4" (`Addr (Emile.IPv4 ip)) back

let test_domain_ipv6 () =
  let ip = Ipaddr.V6.of_string_exn "::1" in
  let emile_d : Emile.domain = `Addr (Emile.IPv6 ip) in
  let colombe_d = Colombe_emile.to_domain emile_d in
  Alcotest.(check colombe_domain)
    "to_domain ipv6" (Colombe.Domain.IPv6 ip) colombe_d ;
  let back = Colombe_emile.of_domain colombe_d in
  Alcotest.(check emile_domain) "of_domain ipv6" (`Addr (Emile.IPv6 ip)) back

let test_domain_extension () =
  let emile_d : Emile.domain = `Addr (Emile.Ext ("X-CUSTOM", "value")) in
  let colombe_d = Colombe_emile.to_domain emile_d in
  Alcotest.(check colombe_domain)
    "to_domain extension"
    (Colombe.Domain.Extension ("X-CUSTOM", "value"))
    colombe_d ;
  let back = Colombe_emile.of_domain colombe_d in
  Alcotest.(check emile_domain)
    "of_domain extension"
    (`Addr (Emile.Ext ("X-CUSTOM", "value")))
    back

let test_domain_literal_rejected () =
  let emile_d : Emile.domain = `Literal "some literal" in
  match Colombe_emile.to_domain emile_d with
  | exception Invalid_argument _ -> ()
  | _ -> Alcotest.fail "Expected Invalid_argument for literal domain"

let test_path_simple () =
  let mailbox = emile_mailbox_of_string "user@example.com" in
  let path = Colombe_emile.to_path mailbox in
  let expected = colombe_path_of_string "<user@example.com>" in
  Alcotest.(check colombe_path) "to_path simple" expected path ;
  let back = Colombe_emile.of_path path in
  Alcotest.(check emile_mailbox) "of_path simple" mailbox back

let test_path_dot_local () =
  let mailbox = emile_mailbox_of_string "first.last@example.com" in
  let path = Colombe_emile.to_path mailbox in
  let expected = colombe_path_of_string "<first.last@example.com>" in
  Alcotest.(check colombe_path) "to_path dot local" expected path ;
  let back = Colombe_emile.of_path path in
  Alcotest.(check emile_mailbox) "of_path dot local" mailbox back

let test_path_ipv4_domain () =
  let ip = Ipaddr.V4.of_string_exn "192.168.1.1" in
  let mailbox =
    {
      Emile.name = None;
      local = [ `Atom "user" ];
      domain = (`Addr (Emile.IPv4 ip), []);
    } in
  let path = Colombe_emile.to_path mailbox in
  Alcotest.(check colombe_domain)
    "path ipv4 domain" (Colombe.Domain.IPv4 ip) path.Colombe.Path.domain ;
  let back = Colombe_emile.of_path path in
  Alcotest.(check emile_mailbox) "of_path ipv4" mailbox back

let test_reverse_path_some () =
  let mailbox = emile_mailbox_of_string "sender@example.com" in
  let rp = Colombe_emile.to_reverse_path mailbox in
  match rp with
  | Some path ->
      let back = Colombe_emile.of_reverse_path rp in
      Alcotest.(check (opt emile_mailbox))
        "of_reverse_path some" (Some mailbox) back ;
      (* Colombe -> Emile -> Colombe *)
      let rp2 = Colombe_emile.to_reverse_path (Colombe_emile.of_path path) in
      Alcotest.(check colombe_reverse_path) "reverse_path isomorphic" rp rp2
  | None -> Alcotest.fail "Expected Some reverse_path"

let test_reverse_path_none () =
  let back = Colombe_emile.of_reverse_path None in
  Alcotest.(check (opt emile_mailbox)) "of_reverse_path none" None back

let test_forward_path_regular () =
  let mailbox = emile_mailbox_of_string "user@example.com" in
  let fp = Colombe_emile.to_forward_path mailbox in
  match fp with
  | Colombe.Forward_path.Forward_path path ->
      let back = Colombe_emile.of_forward_path fp in
      Alcotest.(check (opt emile_mailbox))
        "of_forward_path regular" (Some mailbox) back ;
      (* Colombe -> Emile -> Colombe *)
      let fp2 = Colombe_emile.to_forward_path (Colombe_emile.of_path path) in
      Alcotest.(check colombe_forward_path) "forward_path isomorphic" fp fp2
  | _ -> Alcotest.fail "Expected Forward_path"

let test_forward_path_postmaster_domain () =
  let mailbox =
    {
      Emile.name = None;
      local = [ `Atom "Postmaster" ];
      domain = (`Domain [ "example"; "com" ], []);
    } in
  let fp = Colombe_emile.to_forward_path mailbox in
  match fp with
  | Colombe.Forward_path.Domain domain ->
      Alcotest.(check colombe_domain)
        "forward_path postmaster domain"
        (Colombe.Domain.Domain [ "example"; "com" ])
        domain ;
      let back = Colombe_emile.of_forward_path fp in
      Alcotest.(check (opt emile_mailbox))
        "of_forward_path domain" (Some mailbox) back
  | _ -> Alcotest.fail "Expected Domain forward_path for Postmaster"

let test_forward_path_postmaster_case_insensitive () =
  let mailbox =
    {
      Emile.name = None;
      local = [ `Atom "postmaster" ];
      domain = (`Domain [ "example"; "com" ], []);
    } in
  match Colombe_emile.to_forward_path mailbox with
  | Colombe.Forward_path.Domain _ -> ()
  | _ -> Alcotest.fail "Expected Domain for lowercase postmaster"

let test_forward_path_postmaster_bare () =
  let back = Colombe_emile.of_forward_path Colombe.Forward_path.Postmaster in
  Alcotest.(check (opt emile_mailbox)) "of_forward_path Postmaster" None back

(* local part: mixed Atom/String in Emile (obs-local-part)

   RFC 5322 obs-local-part: word *("." word) allows a."b c"
   RFC 5321 only has dot-string / quoted-string
   So Emile [`Atom "a"; `String "b c"] -> Colombe `String "a.b c"
   -> Emile [`String "a.b c"] — NOT isomorphic.
   This is an inherent limitation of the RFC 5321 representation. *)

let test_local_mixed_atom_string () =
  let emile_l : Emile.local = [ `Atom "a"; `String "b c" ] in
  let colombe_l = Colombe_emile.to_local emile_l in
  (* Must collapse to a single String because of the `String *)
  Alcotest.(check colombe_local) "mixed -> String" (`String "a.b c") colombe_l ;
  let back = Colombe_emile.of_local colombe_l in
  (* NOT the original: single `String, not mixed *)
  Alcotest.(check emile_local) "mixed not isomorphic" [ `String "a.b c" ] back

(* local part: multiple Strings collapse

   [`String "a"; `String "b"] -> `String "a.b" -> [`String "a.b"] *)

let test_local_multiple_strings () =
  let emile_l : Emile.local = [ `String "a"; `String "b" ] in
  let colombe_l = Colombe_emile.to_local emile_l in
  Alcotest.(check colombe_local)
    "multi string -> String" (`String "a.b") colombe_l ;
  let back = Colombe_emile.of_local colombe_l in
  Alcotest.(check emile_local) "multi string collapsed" [ `String "a.b" ] back

(* local part: Dot_string with many segments

   first.middle.last should roundtrip *)

let test_local_many_dots () =
  let colombe_l = `Dot_string [ "a"; "b"; "c"; "d" ] in
  let emile_l = Colombe_emile.of_local colombe_l in
  Alcotest.(check emile_local)
    "many dots -> atoms"
    [ `Atom "a"; `Atom "b"; `Atom "c"; `Atom "d" ]
    emile_l ;
  let back = Colombe_emile.to_local emile_l in
  Alcotest.(check colombe_local) "many dots roundtrip" colombe_l back

(* local part: String with dots inside

   `String "a.b.c" -> [`String "a.b.c"] -> `String "a.b.c" — isomorphic *)

let test_local_string_with_dots () =
  let colombe_l = `String "a.b.c" in
  let emile_l = Colombe_emile.of_local colombe_l in
  let back = Colombe_emile.to_local emile_l in
  Alcotest.(check colombe_local) "string with dots roundtrip" colombe_l back

(* local part: String with spaces

   `String "foo bar" should roundtrip *)

let test_local_string_with_spaces () =
  let colombe_l = `String "foo bar baz" in
  let emile_l = Colombe_emile.of_local colombe_l in
  Alcotest.(check emile_local)
    "string with spaces"
    [ `String "foo bar baz" ]
    emile_l ;
  let back = Colombe_emile.to_local emile_l in
  Alcotest.(check colombe_local) "string with spaces roundtrip" colombe_l back

(* route / extra domains isomorphism

   Colombe Path with route -> of_path -> Emile with extra domains
   -> to_path (no ?route) -> Colombe Path with route again *)

let test_path_route_roundtrip () =
  let route =
    [
      colombe_domain_of_string "relay1.net";
      colombe_domain_of_string "relay2.org";
    ] in
  let path =
    {
      Colombe.Path.local = `Dot_string [ "user" ];
      domain = Colombe.Domain.Domain [ "example"; "com" ];
      rest = route;
    } in
  let emile_mb = Colombe_emile.of_path path in
  let _, domains = emile_mb.Emile.domain in
  Alcotest.(check int) "extra domains count" 2 (List.length domains) ;
  (* Now roundtrip back without explicit ?route *)
  let path2 = Colombe_emile.to_path emile_mb in
  Alcotest.(check colombe_path) "route roundtrip" path path2

(* route: Emile mailbox with domains -> Colombe -> Emile *)

let test_path_emile_domains_roundtrip () =
  let mailbox =
    {
      Emile.name = None;
      local = [ `Atom "user" ];
      domain =
        ( `Domain [ "final"; "com" ],
          [ `Domain [ "relay1"; "net" ]; `Domain [ "relay2"; "org" ] ] );
    } in
  let path = Colombe_emile.to_path mailbox in
  Alcotest.(check (list colombe_domain))
    "emile domains -> rest"
    [
      Colombe.Domain.Domain [ "relay1"; "net" ];
      Colombe.Domain.Domain [ "relay2"; "org" ];
    ]
    path.Colombe.Path.rest ;
  let back = Colombe_emile.of_path path in
  Alcotest.(check emile_mailbox) "emile domains roundtrip" mailbox back

(* route: explicit ?route overrides Emile domains *)

let test_path_explicit_route_overrides () =
  let mailbox =
    {
      Emile.name = None;
      local = [ `Atom "user" ];
      domain = (`Domain [ "final"; "com" ], [ `Domain [ "emile-relay"; "net" ] ]);
    } in
  let explicit_route = [ Colombe.Domain.Domain [ "explicit"; "net" ] ] in
  let path = Colombe_emile.to_path ~route:explicit_route mailbox in
  Alcotest.(check (list colombe_domain))
    "explicit route overrides" explicit_route path.Colombe.Path.rest

(* name is discarded: not isomorphic for named mailboxes

   Emile -> Colombe -> Emile drops the name field *)

let test_name_discarded () =
  let mailbox = emile_mailbox_of_string "John Doe <john@example.com>" in
  let path = Colombe_emile.to_path mailbox in
  let back = Colombe_emile.of_path path in
  Alcotest.(check (opt (testable Emile.pp_phrase Emile.equal_phrase)))
    "name discarded" None back.Emile.name ;
  (* But the address part is preserved *)
  Alcotest.(check emile_local)
    "local preserved" mailbox.Emile.local back.Emile.local

(* Postmaster case normalization

   "postmaster" -> Domain -> of_forward_path -> "Postmaster" (capital P)
   Not case-isomorphic, but semantically correct per RFC 5321 *)

let test_forward_path_postmaster_case_roundtrip () =
  let mailbox_lower =
    {
      Emile.name = None;
      local = [ `Atom "postmaster" ];
      domain = (`Domain [ "example"; "com" ], []);
    } in
  let fp = Colombe_emile.to_forward_path mailbox_lower in
  let back = Colombe_emile.of_forward_path fp in
  match back with
  | Some mb ->
      (* The local part is normalized to "Postmaster" *)
      Alcotest.(check emile_local)
        "postmaster normalized"
        [ `Atom "Postmaster" ]
        mb.Emile.local
  | None -> Alcotest.fail "Expected Some mailbox"

(* IPv6 mapped IPv4 *)

let test_domain_ipv6_mapped_v4 () =
  let ip = Ipaddr.V6.of_string_exn "::ffff:192.168.1.1" in
  let emile_d : Emile.domain = `Addr (Emile.IPv6 ip) in
  let colombe_d = Colombe_emile.to_domain emile_d in
  let back = Colombe_emile.of_domain colombe_d in
  Alcotest.(check emile_domain) "ipv6 mapped v4 roundtrip" emile_d back

(* domain with single label (e.g., localhost) *)

let test_domain_single_label () =
  let emile_d : Emile.domain = `Domain [ "localhost" ] in
  let colombe_d = Colombe_emile.to_domain emile_d in
  Alcotest.(check colombe_domain)
    "single label" (Colombe.Domain.Domain [ "localhost" ]) colombe_d ;
  let back = Colombe_emile.of_domain colombe_d in
  Alcotest.(check emile_domain) "single label roundtrip" emile_d back

(* domain with many labels *)

let test_domain_many_labels () =
  let emile_d : Emile.domain =
    `Domain [ "mail"; "sub"; "example"; "co"; "uk" ] in
  let colombe_d = Colombe_emile.to_domain emile_d in
  let back = Colombe_emile.of_domain colombe_d in
  Alcotest.(check emile_domain) "many labels roundtrip" emile_d back

(* full Colombe Path -> Emile -> Colombe isomorphism *)

let test_full_colombe_path_isomorphism () =
  let path = colombe_path_of_string "<user@example.com>" in
  let emile_mb = Colombe_emile.of_path path in
  let path2 = Colombe_emile.to_path emile_mb in
  Alcotest.(check colombe_path) "colombe path iso" path path2

let test_full_colombe_dotted_path_isomorphism () =
  let path = colombe_path_of_string "<first.last@example.com>" in
  let emile_mb = Colombe_emile.of_path path in
  let path2 = Colombe_emile.to_path emile_mb in
  Alcotest.(check colombe_path) "colombe dotted path iso" path path2

(* full Emile -> Colombe -> Emile isomorphism
   (only for mailboxes without name and without `Literal domains) *)

let test_full_emile_mailbox_isomorphism () =
  let addrs = [ "user@example.com"; "first.last@example.com"; "a@b.c.d.e.f" ] in
  List.iter
    (fun addr ->
      let mailbox = emile_mailbox_of_string addr in
      let path = Colombe_emile.to_path mailbox in
      let back = Colombe_emile.of_path path in
      Alcotest.(check emile_mailbox)
        (Printf.sprintf "emile iso: %s" addr)
        mailbox back)
    addrs

(* forward_path isomorphism for Colombe values *)

let test_colombe_forward_path_isomorphism () =
  let path = colombe_path_of_string "<user@example.com>" in
  let fp = Colombe.Forward_path.Forward_path path in
  let emile_mb = Colombe_emile.of_forward_path fp in
  match emile_mb with
  | Some mb ->
      let fp2 = Colombe_emile.to_forward_path mb in
      Alcotest.(check colombe_forward_path) "colombe forward_path iso" fp fp2
  | None -> Alcotest.fail "Expected Some"

let test_colombe_forward_path_domain_isomorphism () =
  let domain = Colombe.Domain.Domain [ "example"; "com" ] in
  let fp = Colombe.Forward_path.Domain domain in
  let emile_mb = Colombe_emile.of_forward_path fp in
  match emile_mb with
  | Some mb ->
      let fp2 = Colombe_emile.to_forward_path mb in
      Alcotest.(check colombe_forward_path)
        "colombe forward_path domain iso" fp fp2
  | None -> Alcotest.fail "Expected Some"

(* reverse_path isomorphism for Colombe values *)

let test_colombe_reverse_path_isomorphism () =
  let path = colombe_path_of_string "<user@example.com>" in
  let rp : Colombe.Reverse_path.t = Some path in
  let emile_mb = Colombe_emile.of_reverse_path rp in
  match emile_mb with
  | Some mb ->
      let rp2 = Colombe_emile.to_reverse_path mb in
      Alcotest.(check colombe_reverse_path) "colombe reverse_path iso" rp rp2
  | None -> Alcotest.fail "Expected Some"

let test_colombe_reverse_path_none_isomorphism () =
  let rp : Colombe.Reverse_path.t = None in
  let emile_mb = Colombe_emile.of_reverse_path rp in
  Alcotest.(check (opt emile_mailbox)) "reverse_path none iso" None emile_mb

let () =
  Alcotest.run "colombe_emile"
    [
      ( "local",
        [
          Alcotest.test_case "dot_string" `Quick test_local_dot_string;
          Alcotest.test_case "string" `Quick test_local_string;
          Alcotest.test_case "single atom" `Quick test_local_single_atom;
        ] );
      ( "local edge cases",
        [
          Alcotest.test_case "mixed atom/string" `Quick
            test_local_mixed_atom_string;
          Alcotest.test_case "multiple strings" `Quick
            test_local_multiple_strings;
          Alcotest.test_case "many dots" `Quick test_local_many_dots;
          Alcotest.test_case "string with dots" `Quick
            test_local_string_with_dots;
          Alcotest.test_case "string with spaces" `Quick
            test_local_string_with_spaces;
        ] );
      ( "domain",
        [
          Alcotest.test_case "hostname" `Quick test_domain_hostname;
          Alcotest.test_case "ipv4" `Quick test_domain_ipv4;
          Alcotest.test_case "ipv6" `Quick test_domain_ipv6;
          Alcotest.test_case "extension" `Quick test_domain_extension;
          Alcotest.test_case "literal rejected" `Quick
            test_domain_literal_rejected;
        ] );
      ( "domain edge cases",
        [
          Alcotest.test_case "ipv6 mapped v4" `Quick test_domain_ipv6_mapped_v4;
          Alcotest.test_case "single label" `Quick test_domain_single_label;
          Alcotest.test_case "many labels" `Quick test_domain_many_labels;
        ] );
      ( "path",
        [
          Alcotest.test_case "simple" `Quick test_path_simple;
          Alcotest.test_case "dot local" `Quick test_path_dot_local;
          Alcotest.test_case "ipv4 domain" `Quick test_path_ipv4_domain;
          Alcotest.test_case "name discarded" `Quick test_name_discarded;
        ] );
      ( "route",
        [
          Alcotest.test_case "colombe route roundtrip" `Quick
            test_path_route_roundtrip;
          Alcotest.test_case "emile domains roundtrip" `Quick
            test_path_emile_domains_roundtrip;
          Alcotest.test_case "explicit route overrides" `Quick
            test_path_explicit_route_overrides;
        ] );
      ( "reverse_path",
        [
          Alcotest.test_case "some" `Quick test_reverse_path_some;
          Alcotest.test_case "none" `Quick test_reverse_path_none;
        ] );
      ( "forward_path",
        [
          Alcotest.test_case "regular" `Quick test_forward_path_regular;
          Alcotest.test_case "postmaster domain" `Quick
            test_forward_path_postmaster_domain;
          Alcotest.test_case "postmaster case insensitive" `Quick
            test_forward_path_postmaster_case_insensitive;
          Alcotest.test_case "postmaster bare" `Quick
            test_forward_path_postmaster_bare;
          Alcotest.test_case "postmaster case roundtrip" `Quick
            test_forward_path_postmaster_case_roundtrip;
        ] );
      ( "isomorphism: Colombe -> Emile -> Colombe",
        [
          Alcotest.test_case "path" `Quick test_full_colombe_path_isomorphism;
          Alcotest.test_case "dotted path" `Quick
            test_full_colombe_dotted_path_isomorphism;
          Alcotest.test_case "forward_path" `Quick
            test_colombe_forward_path_isomorphism;
          Alcotest.test_case "forward_path domain" `Quick
            test_colombe_forward_path_domain_isomorphism;
          Alcotest.test_case "reverse_path" `Quick
            test_colombe_reverse_path_isomorphism;
          Alcotest.test_case "reverse_path none" `Quick
            test_colombe_reverse_path_none_isomorphism;
          Alcotest.test_case "route roundtrip" `Quick test_path_route_roundtrip;
        ] );
      ( "isomorphism: Emile -> Colombe -> Emile",
        [
          Alcotest.test_case "various addrs" `Quick
            test_full_emile_mailbox_isomorphism;
          Alcotest.test_case "emile domains" `Quick
            test_path_emile_domains_roundtrip;
        ] );
    ]
