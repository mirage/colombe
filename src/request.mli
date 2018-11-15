module Request: sig
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

  val pp: Format.formatter -> t -> unit

  val compare: t -> t -> bool

  val eval: string -> t

  val command: t -> string
end
