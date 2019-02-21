type 'a node =
  | Tree of string * 'a * 'a node
  | Leaf of string * 'a
  | Node of 'a node * 'a node * int * int

type 'a t = 'a node option

let empty = None
let singleton k v = Some (Leaf (k, v))

external code : string -> int -> int = "%string_unsafe_get" [@@noalloc]

type result =
  | Equal
  | Prefix
  | Contain
  | Inferior of int * int * int
  | Superior of int * int * int

exception Diff

let diff a b off len =
  if off == len then Equal
  else
    let idx = ref off in
    let c1 = ref 0 in
    let c2 = ref 0 in

    try
      while !idx < len &&
            ( c1 := code a !idx ;
              c2 := code b !idx ;
              if !c1 != !c2 then raise Diff ;
              !c1 == !c2 )
      do incr idx done ;
      Equal
    with Diff ->
      if !c1 < !c2
      then Inferior (!idx, !c1, !c2)
      else Superior (!idx, !c1, !c2)

let compare (a, alen) (b, blen) off =
  if alen == blen
  then diff a b off alen
  else if alen < blen
  then match diff a b off alen with
    | Equal -> Prefix | v -> v
  else (* alen > blen *) match diff a b off blen with
    | Equal -> Contain | v -> v

let table =
  [| 0;1;2;2;3;3;3;3;4;4;4;4;4;4;4;4;5;5;5;5;5;5;5;5;5;5;5;5;5;5;5;5;
      6;6;6;6;6;6;6;6;6;6;6;6;6;6;6;6;6;6;6;6;6;6;6;6;6;6;6;6;6;6;6;6;
      7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;
      7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;7;
      8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;
      8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;
      8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;
      8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8;8 |]

let ffs byte = Array.unsafe_get table byte
[@@inlined]

(* assert (c1 <> c2) *)
let critical_bit c1 c2 = ffs (c1 lxor c2) - 1
[@@inlined]

let rec first = function
  | Leaf (key, _) -> key
  | Tree (key, _, _) -> key
  | Node (l, _, _, _) -> first l
[@@inlined]

let rec add (key, off, len) value tree = match tree with
  | Leaf (key', value') ->
    let len' = String.length key' in
    (match compare (key, len) (key', len') off with
      | Equal -> Leaf (key', value)
      | Prefix -> Tree (key, value, tree)
      | Contain -> Tree (key', value', Leaf (key, value))
      | Inferior (p, a, b) ->
        let b = critical_bit a b in
        Node (Leaf (key, value), tree, p, b)
      | Superior (p, a, b) ->
        let b = critical_bit a b in
        Node (Leaf (key, value), tree, p, b))
  | Tree (key', value', sub) ->
    let len' = String.length key' in
    (match compare (key, len) (key', len') off with
      | Equal -> Tree (key, value, sub)
      | Prefix -> Tree (key, value, tree)
      | Contain -> Tree (key', value', add (key, len', len) value sub)
      | Inferior (p, a, b) ->
        let bit = critical_bit a b in
        Node (Leaf (key, value), tree, p, bit)
      | Superior (p, a, b) ->
        let bit = critical_bit a b in
        Node (tree, Leaf (key, value), p, bit))
  | Node (l, r, p, bit) ->
    if len > p
    then if (code key p) land (1 lsl bit) == 0
      then Node (add (key, p, len) value l, r, p, bit)
      else Node (l, add (key, p, len) value r, p, bit)
    else
      let key' = first l in
      match compare (key, len) (key', len) off with
      | Equal | Prefix -> Tree (key, value, tree)
      | Contain -> Node (add (key, p, len) value l, r, p, bit)
      | Inferior (p', a', b') ->
        if p' == p
        then Node (add (key, p, len) value l, r, p, bit)
        else let bit' = critical_bit a' b' in
          Node (Leaf (key, value), tree, p', bit')
      | Superior (p', a', b') ->
        if p' == p
        then Node (l, add (key, p, len) value r, p, bit)
        else
          let bit' = critical_bit a' b' in
          Node (tree, Leaf (key, value), p', bit')

let add key value = function
  | None -> Some (Leaf (key, value))
  | Some tree ->
    Some (add (key, 0, String.length key) value tree)

let equal (a, aoff, alen) (b, boff, blen) =
  try let idx = ref 0 in
    while aoff + !idx < alen
          && boff + !idx < blen
          && ( if code a (aoff + !idx) != code b (boff + !idx) then raise Diff ; true )
    do incr idx done ; aoff + !idx == alen && boff + !idx == blen
  with Diff -> false

(* XXX(dinosaure): assumption, key need to be prefix-free. *)

let rec find advance off = function
  | Leaf (key, value) ->
    let len = String.length key in
    (match advance (len - off) with
      | None -> raise Not_found
      | Some (raw, off', len') ->
        if not (equal (key, off, len - off) (Bytes.unsafe_to_string raw, off' + off, len' - off))
        then raise Not_found ;
        value)
  | Node (l, r, p, bit) ->
    (match advance (p - off) with
      | Some (raw, off', _) ->
        if code (Bytes.unsafe_to_string raw) (off' + p) land (1 lsl bit) == 0
        then find advance p l
        else find advance p r
      | None -> raise Not_found)
  | Tree (key, value, sub) ->
    let len = String.length key in
    (match advance (len - off) with
      | Some (raw, off', len') ->
        if not (equal (key, off, len - off) (Bytes.unsafe_to_string raw, off' + off, len' - off))
        then let key' = Bytes.sub_string raw off' len' in
          match compare (key', len') (key, len) off with
          | Equal | Prefix -> value
          | Contain -> find advance len sub
          | Inferior _ | Superior _ -> raise Not_found
        else value
      | None -> raise Not_found)

let find advance = function
  | None -> raise Not_found
  | Some tree -> find advance 0 tree
