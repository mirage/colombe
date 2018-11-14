SMTP standard reply codes:
==========================

Positive Completion reply:
--------------------------

* 211 - System status, or system help reply

* 214 - Help message (Information on how to use the receiver or the
    meaning of a particular non-standard command; this reply is useful
    only to the human user)

* 220 - <domain> Service ready

* 221 - <domain> Service closing transmission channel

* 250 - Requested mail action okay, completed

* 251 - User not local; will forward to <forward-path> (See Section 3.4)

* 252 - Cannot VRFY user, but will accept message and attempt delivery
    (See Section 3.5.3)


Positive Intermediate reply:
----------------------------

* 354 - Start mail input; end with <CRLF>.<CRLF>


Transient Negative Completion reply:
------------------------------------

* 421 - <domain> Service not available, closing transmission channel
    (This may be a reply to any command if the service knows it must
    shut down)

* 450 - Requested mail action not taken: mailbox unavailable (e.g.,
    mailbox busy or temporarily blocked for policy reasons)

* 451 - Requested action aborted: local error in processing

* 452 - Requested action not taken: insufficient system storage

* 455 - Server unable to accommodate parameters


Permanent Negative Completion reply:
------------------------------------

* 500 - Syntax error, command unrecognized (This may include errors such
    as command line too long)

* 501 - Syntax error in parameters or arguments

* 502 - Command not implemented (see Section 4.2.4)

* 503 - Bad sequence of commands

* 504 - Command parameter not implemented

* 550 - Requested action not taken: mailbox unavailable (e.g., mailbox
    not found, no access, or command rejected for policy reasons)

* 551 - User not local; please try <forward-path> (See Section 3.4)

* 552 - Requested mail action aborted: exceeded storage allocation

* 553 - Requested action not taken: mailbox name not allowed (e.g.,
    mailbox syntax incorrect)

* 554 - Transaction failed (Or, in the case of a connection-opening
    response, "No SMTP service here")

* 555 - MAIL FROM/RCPT TO parameters not recognized or not implemented
