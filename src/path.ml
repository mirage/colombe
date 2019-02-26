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
