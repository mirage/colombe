open Angstrom

module Request = struct
  (* Permanent positive response *)
  type standard_command = [
    | `EHLO of string
    | `HELO of string
    | `MAIL of string
    | `RCPT of string
    | `DATA
    | `RSET
    | `NOOP
    | `QUIT
    | `VRFY of string
  ]

  type t = [
    | standard_command
    | `Text of string
    | `TextEnd
  ]

  let pp : Format.formatter -> t -> unit =
    fun ppf -> function 
      | `EHLO txt -> Format.fprintf ppf "Command EHLO with arg %s\n%!" txt
      | `HELO txt -> Format.fprintf ppf "Command HELO with arg %s\n%!" txt
      | `MAIL txt -> Format.fprintf ppf "Command MAIL with arg %s\n%!" txt
      | `RCPT txt -> Format.fprintf ppf "Command RCPT with arg %s\n%!" txt
      | `DATA -> Format.fprintf ppf "Command DATA\n%!" 
      | `RSET -> Format.fprintf ppf "Command RSET\n%!"
      | `NOOP -> Format.fprintf ppf "Command NOOP\n%!"
      | `QUIT -> Format.fprintf ppf "Command QUIT\n%!"
      | `VRFY txt -> Format.fprintf ppf "Command VRFY with arg %s\n%!" txt
      | `Text txt -> Format.fprintf ppf "Text '%s'\n%!" txt
      | `TextEnd -> Format.fprintf ppf "Text end\n%!"

  let compare t1 t2 =
    match (t1, t2) with
    | (`DATA , `DATA)
    | (`RSET , `RSET)
    | (`NOOP , `NOOP)
    | (`QUIT , `QUIT)
    | (`TextEnd , `TextEnd) -> true
    | (`EHLO txt1, `EHLO txt2)
    | (`HELO txt1, `HELO txt2)
    | (`MAIL txt1, `MAIL txt2)
    | (`RCPT txt1, `RCPT txt2)
    | (`VRFY txt1, `VRFY txt2)
    | (`Text txt1, `Text txt2) -> String.compare txt1 txt2 = 0
    | _ -> false

  let dot c = c = '.'
  let is_space = function | ' ' | '\t' -> true | _ -> false
  let is_eol = function | '\r' | '\n' -> true | _ -> false
  let is_not_eol c = not (is_eol c)
  let crlf = string "\r\n" <?> "crlf" >>= fun _ -> return ()

  let parse_endline =
    lift (fun _ -> ())
      crlf <* end_of_input

  let parse_text =
    take_while is_not_eol

  let parse_ehlo =
    lift (fun txt -> `EHLO txt)
      (string_ci "EHLO" *> (skip is_space) *> (parse_text) <* parse_endline)

  let parse_helo =
    lift (fun txt -> `HELO txt)
      (string_ci "HELO" *> (skip is_space) *> (parse_text) <* parse_endline)

  let parse_mail =
    lift (fun txt -> `MAIL txt)
      (string_ci "MAIL FROM:" *> (parse_text) <* parse_endline)

  let parse_recipient =
    lift (fun txt -> `RCPT txt)
      (string_ci "RCPT TO:" *> (parse_text) <* parse_endline)

  let parse_noop =
    lift (fun _ -> `NOOP)
      (string_ci "NOOP" *> (option "" ((skip is_space) *> (parse_text))) <* parse_endline)

  let parse_data =
    lift (fun _ -> `DATA)
      (string_ci "DATA" <* parse_endline)

  let parse_reset =
    lift (fun _ -> `RSET)
      (string_ci "RSET" <* parse_endline)

  let parse_quit =
    lift (fun _ -> `QUIT)
      (string_ci "QUIT" <* parse_endline)

  let parse_verify =
    lift (fun txt -> `VRFY txt)
      (string_ci "VRFY" *> (skip is_space) *> (parse_text) <* parse_endline)

  let parse_text =
    lift (fun cmd -> `Text cmd)
      ((parse_text) <* parse_endline)

  let parse_text_end =
    lift (fun _ -> `TextEnd)
      ((skip dot) <* parse_endline)

  let parse_request =
    lift (fun cmd -> cmd)
      (choice [parse_ehlo; parse_helo; parse_mail; parse_recipient; parse_noop; parse_data; parse_reset; parse_quit; parse_verify; parse_text_end; parse_text])

  let eval (str:string) : t =
    match parse_string parse_request str with
    | Ok v      -> v
    | Error msg -> failwith msg

  let command : t -> string = function
    | `EHLO _ -> "EHLO"
    | `HELO _ -> "HELO"
    | `MAIL _ -> "MAIL"
    | `RCPT _ -> "RCPT"
    | `DATA -> "DATA"
    | `RSET -> "RSET"
    | `NOOP -> "NOOP"
    | `QUIT -> "QUIT"
    | `VRFY _ -> "VRFY"
    | `Text cmd -> cmd
    | `TextEnd -> "TextEnd"
end
