SOURCES = Thrift.ml TBinaryProtocol.ml TSocket.ml TChannelTransport.ml TServer.ml TSimpleServer.ml TServerSocket.ml TThreadedServer.ml
RESULT = thrift
LIBS = unix threads
THREADS = yes
all: native-code-library debug-code-library top
OCAMLMAKEFILE = ../OCamlMakefile
include $(OCAMLMAKEFILE)
