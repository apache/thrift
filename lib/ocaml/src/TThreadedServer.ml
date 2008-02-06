open Thrift

class t
  (pf : Processor.t)
  (st : Transport.server_t)
  (tf : Transport.factory)
  (ipf : Protocol.factory)
  (opf : Protocol.factory)=
object
  inherit TServer.t pf st tf ipf opf
  method serve =
    st#listen;
    while true do
      let tr = tf#getTransport (st#accept) in
        ignore (Thread.create
          (fun _ ->
             let ip = ipf#getProtocol tr in
             let op = opf#getProtocol tr in
               try
                 while pf#process ip op do
                   ()
                 done
               with _ -> ()) ())
    done
end

