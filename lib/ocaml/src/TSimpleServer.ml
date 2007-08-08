open Thrift
module S = TServer

class t pf st tf ipf opf =
object
  inherit S.t pf st tf ipf opf
  method serve =
    try
      st#listen;
      let c = st#accept in
      let trans = tf#getTransport c in
      let inp = ipf#getProtocol trans in
      let op = opf#getProtocol trans in
        try
          while (pf#process inp op) do () done;
          trans#close
        with e -> trans#close; raise e
    with _ -> ()
end
