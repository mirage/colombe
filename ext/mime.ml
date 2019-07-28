module Client = struct
  type error = Colombe.Rfc1869.error
  type t = [ `Bit8_MIME | `Bit7 ] option

  let pp_error = Colombe.Rfc1869.pp_error

  let ehlo t _ = Ok t

  let mail_from t mail_from = match t, mail_from with
    | None, _ -> []
    | Some `Bit8_MIME, _ -> [ "BODY", Some "8BITMIME" ]
    | Some `Bit7, _ -> [ "BODY", Some "7BIT" ]

  let encode _t = assert false
  let action _t = assert false
  let handle _t = assert false
  let decode _txts _t = Ok _t
  let rcpt_to _t _rcpt_to = []
end

type encoding = Client.t

let description : Colombe.Rfc1869.description =
  { name= "8bit-MIMEtransport"
  ; elho= "8BITMIME"
  ; verb= [] }

let none = None
let bit7 = Some `Bit7
let bit8 = Some `Bit8_MIME

let extension = Colombe.Rfc1869.inj (module Client)

let inj v =
  let module Ext = (val extension) in
  Ext.T v
