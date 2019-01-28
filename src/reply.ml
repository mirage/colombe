open Angstrom

module Reply = struct
  (* Permanent positive response *)
  type pos_completion = [
    | `PP_211 of string list
    | `PP_214 of string list
    | `PP_220 of string list
    | `PP_221 of string list
    | `PP_250 of string list
    | `PP_251 of string list
    | `PP_252 of string list
    ]

  (* Temporary positive response *)
  type pos_intermediate = [
    | `TP_354 of string list
    ]

  (* Temporary negativ response *)
  type transient_neg_completion = [
    | `TN_421 of string list
    | `TN_450 of string list
    | `TN_451 of string list
    | `TN_452 of string list
    | `TN_455 of string list
    ]

  (* Permanent negativ response *)
  type permanent_neg_completion = [
    | `PN_500 of string list
    | `PN_501 of string list
    | `PN_502 of string list
    | `PN_503 of string list
    | `PN_504 of string list
    | `PN_550 of string list
    | `PN_551 of string list
    | `PN_552 of string list
    | `PN_553 of string list
    | `PN_554 of string list
    | `PN_555 of string list
    ]

  type t = [
    | pos_completion
    | pos_intermediate
    | transient_neg_completion
    | permanent_neg_completion
    | `Other of int * string list
    ]

  exception MultilineInvalidCode of int list

  let pp : Format.formatter -> t -> unit =
    fun ppf -> function
      | `PP_211 (texts) -> Fmt.pf ppf "Standard code: '211' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `PP_214 (texts) -> Fmt.pf ppf "Standard code: '214' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `PP_220 (texts) -> Fmt.pf ppf "Standard code: '220' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `PP_221 (texts) -> Fmt.pf ppf "Standard code: '221' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `PP_250 (texts) -> Fmt.pf ppf "Standard code: '250' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `PP_251 (texts) -> Fmt.pf ppf "Standard code: '251' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `PP_252 (texts) -> Fmt.pf ppf "Standard code: '252' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `TP_354 (texts) -> Fmt.pf ppf "Standard code: '354' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `TN_421 (texts) -> Fmt.pf ppf "Standard code: '421' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `TN_450 (texts) -> Fmt.pf ppf "Standard code: '450' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `TN_451 (texts) -> Fmt.pf ppf "Standard code: '451' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `TN_452 (texts) -> Fmt.pf ppf "Standard code: '452' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `TN_455 (texts) -> Fmt.pf ppf "Standard code: '455' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `PN_500 (texts) -> Fmt.pf ppf "Standard code: '500' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `PN_501 (texts) -> Fmt.pf ppf "Standard code: '501' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `PN_502 (texts) -> Fmt.pf ppf "Standard code: '502' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `PN_503 (texts) -> Fmt.pf ppf "Standard code: '503' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `PN_504 (texts) -> Fmt.pf ppf "Standard code: '504' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `PN_550 (texts) -> Fmt.pf ppf "Standard code: '550' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `PN_551 (texts) -> Fmt.pf ppf "Standard code: '551' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `PN_552 (texts) -> Fmt.pf ppf "Standard code: '552' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `PN_553 (texts) -> Fmt.pf ppf "Standard code: '553' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `PN_554 (texts) -> Fmt.pf ppf "Standard code: '554' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `PN_555 (texts) -> Fmt.pf ppf "Standard code: '555' - ["; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"
      | `Other (code, texts) -> Fmt.pf ppf "Other code: '%d' - [" code; List.iter (Fmt.pf ppf "'%s', ") texts; Fmt.pf ppf "]\n%!"

  let compare t1 t2 =
    let cmp_str_list l1 l2 = List.length l1 = List.length l2 && List.for_all (fun (s1, s2) -> String.compare s1 s2 = 0) (List.combine l1 l2) in
    match (t1, t2) with
    | (`PP_211 (texts1), `PP_211 (texts2))
    | (`PP_214 (texts1), `PP_214 (texts2))
    | (`PP_220 (texts1), `PP_220 (texts2))
    | (`PP_221 (texts1), `PP_221 (texts2))
    | (`PP_250 (texts1), `PP_250 (texts2))
    | (`PP_251 (texts1), `PP_251 (texts2))
    | (`PP_252 (texts1), `PP_252 (texts2))
    | (`TP_354 (texts1), `TP_354 (texts2))
    | (`TN_421 (texts1), `TN_421 (texts2))
    | (`TN_450 (texts1), `TN_450 (texts2))
    | (`TN_451 (texts1), `TN_451 (texts2))
    | (`TN_452 (texts1), `TN_452 (texts2))
    | (`TN_455 (texts1), `TN_455 (texts2))
    | (`PN_500 (texts1), `PN_500 (texts2))
    | (`PN_501 (texts1), `PN_501 (texts2))
    | (`PN_502 (texts1), `PN_502 (texts2))
    | (`PN_503 (texts1), `PN_503 (texts2))
    | (`PN_504 (texts1), `PN_504 (texts2))
    | (`PN_550 (texts1), `PN_550 (texts2))
    | (`PN_551 (texts1), `PN_551 (texts2))
    | (`PN_552 (texts1), `PN_552 (texts2))
    | (`PN_553 (texts1), `PN_553 (texts2))
    | (`PN_554 (texts1), `PN_554 (texts2))
    | (`PN_555 (texts1), `PN_555 (texts2)) -> cmp_str_list texts1 texts2
    | (`Other (code1, texts1), `Other (code2, texts2)) when code1 = code2 -> cmp_str_list texts1 texts2
    | _ -> false

  let is_space = function | ' ' | '\t' -> true | _ -> false
  let is_eol = function | '\r' | '\n' -> true | _ -> false
  let is_not_eol c = not (is_eol c)
  let crlf = string "\r\n" <?> "crlf" >>= fun _ -> return ()
  let integer =
    take_while1 (function '0' .. '9' -> true | _ -> false) >>| int_of_string

  let create_code code texts =
    match code with
      | 211 -> `PP_211 (texts)
      | 214 -> `PP_214 (texts)
      | 220 -> `PP_220 (texts)
      | 221 -> `PP_221 (texts)
      | 250 -> `PP_250 (texts) 
      | 251 -> `PP_251 (texts)
      | 252 -> `PP_252 (texts)
      | 354 -> `TP_354 (texts)
      | 421 -> `TN_421 (texts)
      | 450 -> `TN_450 (texts)
      | 451 -> `TN_451 (texts)
      | 452 -> `TN_452 (texts)
      | 455 -> `TN_455 (texts)
      | 500 -> `PN_500 (texts)
      | 501 -> `PN_501 (texts)
      | 502 -> `PN_502 (texts)
      | 503 -> `PN_503 (texts)
      | 504 -> `PN_504 (texts)
      | 550 -> `PN_550 (texts)
      | 551 -> `PN_551 (texts)
      | 552 -> `PN_552 (texts)
      | 553 -> `PN_553 (texts)
      | 554 -> `PN_554 (texts)
      | 555 -> `PN_555 (texts)
      | _ -> `Other (code, texts)

  let parse_text =
    lift (fun text -> text)
      (take_while is_not_eol) <* crlf

  let parse_text_l =
    lift (fun text -> [text])
      (take_while is_not_eol) <* crlf

  let parse_multiline =
    lift2 (fun code text -> (code, text))
      (integer)
      ((string "-") *> parse_text)

  let parse_reply =
    lift3 (fun multilines code text ->
            let (codes, texts) = List.split multilines in
            if (List.for_all (fun nbr -> nbr = code) codes)
            then
              create_code code (texts @ text)
            else
              raise (MultilineInvalidCode (code::codes)))
      (many parse_multiline)
      (integer)
      ((crlf >>= fun () -> return []) <|> ((skip is_space) *> parse_text_l)) <* (end_of_input)

  let eval (str:string) : t =
    match parse_string parse_reply str with
    | Ok v      -> v
    | Error msg -> failwith msg

  let code : t -> int = function
    | `PP_211 _ -> 211
    | `PP_214 _ -> 214
    | `PP_220 _ -> 220
    | `PP_221 _ -> 221
    | `PP_250 _ -> 250
    | `PP_251 _ -> 251
    | `PP_252 _ -> 252
    | `TP_354 _ -> 354
    | `TN_421 _ -> 421
    | `TN_450 _ -> 450
    | `TN_451 _ -> 451
    | `TN_452 _ -> 452
    | `TN_455 _ -> 455
    | `PN_500 _ -> 500
    | `PN_501 _ -> 501
    | `PN_502 _ -> 502
    | `PN_503 _ -> 503
    | `PN_504 _ -> 504
    | `PN_550 _ -> 550
    | `PN_551 _ -> 551
    | `PN_552 _ -> 552
    | `PN_553 _ -> 553
    | `PN_554 _ -> 554
    | `PN_555 _ -> 555
    | `Other (code, _) -> code

  let texts : t -> string list = function
    | `PP_211 (texts) -> texts
    | `PP_214 (texts) -> texts
    | `PP_220 (texts) -> texts
    | `PP_221 (texts) -> texts
    | `PP_250 (texts) -> texts
    | `PP_251 (texts) -> texts
    | `PP_252 (texts) -> texts
    | `TP_354 (texts) -> texts
    | `TN_421 (texts) -> texts
    | `TN_450 (texts) -> texts
    | `TN_451 (texts) -> texts
    | `TN_452 (texts) -> texts
    | `TN_455 (texts) -> texts
    | `PN_500 (texts) -> texts
    | `PN_501 (texts) -> texts
    | `PN_502 (texts) -> texts
    | `PN_503 (texts) -> texts
    | `PN_504 (texts) -> texts
    | `PN_550 (texts) -> texts
    | `PN_551 (texts) -> texts
    | `PN_552 (texts) -> texts
    | `PN_553 (texts) -> texts
    | `PN_554 (texts) -> texts
    | `PN_555 (texts) -> texts
    | `Other (_, texts) -> texts
end
