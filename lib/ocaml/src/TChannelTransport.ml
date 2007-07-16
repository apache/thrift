open Thrift
module T = Transport

class t (i,o) =
object (self)
  inherit Transport.t
  method isOpen = true
  method opn = ()
  method close = ()
  method read buf off len = 
    try 
      really_input i buf off len; len
    with _ -> T.raise_TTransportExn ("TChannelTransport: Could not read "^(string_of_int len)) T.UNKNOWN
  method write buf off len = output o buf off len
  method flush = flush o
end
