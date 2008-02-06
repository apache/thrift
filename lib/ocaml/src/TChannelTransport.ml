open Thrift
module T = Transport

class t (i,o) =
object (self)
  val mutable opened = true
  inherit Transport.t
  method isOpen = opened
  method opn = ()
  method close = close_in i; opened <- false
  method read buf off len =
    if opened then
      try
        really_input i buf off len; len
      with _ -> raise (T.E (T.UNKNOWN, ("TChannelTransport: Could not read "^(string_of_int len))))
    else
      raise (T.E (T.NOT_OPEN, "TChannelTransport: Channel was closed"))
  method write buf off len = output o buf off len
  method flush = flush o
end
