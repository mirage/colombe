open Angstrom

module Request = struct
  (* Permanent positive response *)
  type standard_command = [
    | `EHLO
    | `HELO
    | `MAIL
    | `RCPT
    | `DATA
    | `RSET
    | `NOOP
    | `QUIT
    | `VRFY
  ]

  type t = [
    | standard_command
    | `Other of string
  ]

  let pp : Format.formatter -> t -> unit =
    fun ppf -> function 
      | `EHLO -> Format.fprintf ppf "Command EHLO\n%!"
      | `HELO -> Format.fprintf ppf "Command HELO\n%!"
      | `MAIL -> Format.fprintf ppf "Command MAIL\n%!"
      | `RCPT -> Format.fprintf ppf "Command RCPT\n%!"
      | `DATA -> Format.fprintf ppf "Command DATA\n%!" 
      | `RSET -> Format.fprintf ppf "Command RSET\n%!"
      | `NOOP -> Format.fprintf ppf "Command NOOP\n%!"
      | `QUIT -> Format.fprintf ppf "Command QUIT\n%!"
      | `VRFY -> Format.fprintf ppf "Command VRFY\n%!"
      | `Other cmd -> Format.fprintf ppf "Other command %s\n%!" cmd

  let compare t1 t2 =
    match (t1, t2) with
    | (`EHLO , `EHLO)
    | (`HELO , `HELO)
    | (`MAIL , `MAIL)
    | (`RCPT , `RCPT)
    | (`DATA , `DATA)
    | (`RSET , `RSET)
    | (`NOOP , `NOOP)
    | (`QUIT , `QUIT)
    | (`VRFY , `VRFY) -> true
    | (`Other cmd1, `Other cmd2) -> String.compare cmd1 cmd2 == 0
    | _ -> false

  let is_eol = function | '\r' | '\n' -> true | _ -> false
  let is_not_eol c = not (is_eol c)
  let crlf = string "\r\n" <?> "crlf" >>= fun _ -> return ()

  let parse_text =
    lift (fun text -> text)
      (take_while is_not_eol) <* crlf

  let parse_noop =
    lift (fun _ -> `NOOP)
      (string "NOOP" <* (parse_text) <* (end_of_input))

  let parse_other =
    lift (fun cmd -> `Other cmd)
      ((parse_text) <* (end_of_input))

  let parse_request =
    lift (fun cmd -> cmd)
      (choice [parse_noop; parse_other])

  let eval (str:string) : t =
    match parse_string parse_request str with
    | Ok v      -> v
    | Error msg -> failwith msg

  let command : t -> string = function
    | `EHLO -> "EHLO"
    | `HELO -> "HELO"
    | `MAIL -> "MAIL"
    | `RCPT -> "RCPT"
    | `DATA -> "DATA"
    | `RSET -> "RSET"
    | `NOOP -> "NOOP"
    | `QUIT -> "QUIT"
    | `VRFY -> "VRFY"
    | `Other cmd -> cmd
end
