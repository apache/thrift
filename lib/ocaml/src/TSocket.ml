open Thrift

module T = Transport

class t host port=
object (self)
  inherit T.t
  val mutable chans = None
  method isOpen = chans != None
  method opn = 
    try
      chans <- Some(Unix.open_connection (Unix.ADDR_INET ((Unix.inet_addr_of_string host),port)))
    with _ -> 
      T.raise_TTransportExn 
        ("Could not connect to "^host^":"^(string_of_int port)) 
        T.NOT_OPEN
  method close = match chans with None -> () | Some(inc,_) -> (Unix.shutdown_connection inc; chans <- None)
  method read buf off len = match chans with
      None -> T.raise_TTransportExn "Socket not open" T.NOT_OPEN
    | Some(i,o) -> 
        try 
          really_input i buf off len; len
        with _ -> T.raise_TTransportExn ("TSocket: Could not read "^(string_of_int len)^" from "^host^":"^(string_of_int port)) T.UNKNOWN
  method write buf off len = match chans with 
      None -> T.raise_TTransportExn "Socket not open" T.NOT_OPEN
    | Some(i,o) -> output o buf off len
  method flush = match chans with
      None -> T.raise_TTransportExn "Socket not open" T.NOT_OPEN
    | Some(i,o) -> flush o
end
        
    
