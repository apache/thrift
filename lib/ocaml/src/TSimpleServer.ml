open Thrift
module S = TServer

class t pf st itf otf ipf opf =
object
  inherit S.t pf st itf otf ipf opf
  method serve =
    try
      st#listen;
      let c = st#accept in
      let proc = pf#getProcessor c in
      let itrans = itf#getTransport c in
      let otrans = try
          otf#getTransport c
        with e -> itrans#close; raise e
      in
      let inp = ipf#getProtocol itrans in
      let op = opf#getProtocol otrans in
        try
          while (proc#process inp op) do () done;
          itrans#close; otrans#close
        with e -> itrans#close; otrans#close; raise e
    with _ -> ()
end
