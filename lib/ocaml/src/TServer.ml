open Thrift

class virtual t
    (pf : Processor.t)
    (st : Transport.server_t)
    (tf : Transport.factory)
    (ipf : Protocol.factory)
    (opf : Protocol.factory)=
object
  method virtual serve : unit
end;;



let run_basic_server proc port =
  Unix.establish_server (fun inp -> fun out ->
                           let trans = new TChannelTransport.t (inp,out) in
                           let proto = new TBinaryProtocol.t (trans :> Transport.t) in
                             try
                               while proc#process proto proto do () done; ()
                             with e -> ()) (Unix.ADDR_INET (Unix.inet_addr_of_string "127.0.0.1",port))


