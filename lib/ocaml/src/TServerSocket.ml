open Thrift

class t port =
object
  inherit Transport.server_t
  val mutable sock = None
  method listen =
    let s = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
      sock <- Some s;
      Unix.bind s (Unix.ADDR_INET (Unix.inet_addr_any, port));
      Unix.listen s 256
  method close =
    match sock with
        Some s -> Unix.shutdown s Unix.SHUTDOWN_ALL; Unix.close s;
          sock <- None
      | _ -> ()
  method acceptImpl =
    match sock with
        Some s -> let (fd,_) = Unix.accept s in
                    new TChannelTransport.t (Unix.in_channel_of_descr fd,Unix.out_channel_of_descr fd)
      | _ -> raise (Transport.E (Transport.NOT_OPEN,"TServerSocket: Not listening but tried to accept"))
end
