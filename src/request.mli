module Request: sig
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

  val pp: Format.formatter -> t -> unit

  val compare: t -> t -> bool

  val eval: string -> t

  val command: t -> string
end
