open Thrift

module T = Transport

class t host port=
object (self)
  inherit T.t
  val mutable chans = None
  method isOpen = chans != None
  method opn =
    try
      let addr = (let {Unix.h_addr_list=x} = Unix.gethostbyname host in x.(0)) in
        chans <- Some(Unix.open_connection (Unix.ADDR_INET (addr,port)))
    with
        Unix.Unix_error (e,fn,_) -> raise (T.E (T.NOT_OPEN, ("TSocket: Could not connect to "^host^":"^(string_of_int port)^" because: "^fn^":"^(Unix.error_message e))))
      | _ -> raise (T.E (T.NOT_OPEN, ("TSocket: Could not connect to "^host^":"^(string_of_int port))))

  method close =
    match chans with
        None -> ()
      | Some(inc,out) -> (Unix.shutdown_connection inc;
                          close_in inc;
                          chans <- None)
  method read buf off len = match chans with
      None -> raise (T.E (T.NOT_OPEN, "TSocket: Socket not open"))
    | Some(i,o) ->
        try
          really_input i buf off len; len
        with
            Unix.Unix_error (e,fn,_) -> raise (T.E (T.UNKNOWN, ("TSocket: Could not read "^(string_of_int len)^" from "^host^":"^(string_of_int port)^" because: "^fn^":"^(Unix.error_message e))))
          | _ -> raise (T.E (T.UNKNOWN, ("TSocket: Could not read "^(string_of_int len)^" from "^host^":"^(string_of_int port))))
  method write buf off len = match chans with
      None -> raise (T.E (T.NOT_OPEN, "TSocket: Socket not open"))
    | Some(i,o) -> output o buf off len
  method flush = match chans with
      None -> raise (T.E (T.NOT_OPEN, "TSocket: Socket not open"))
    | Some(i,o) -> flush o
end


