THRIFTCODE= \
            src/Collections/THashSet.cs \
            src/Protocol/TBase.cs \
            src/Protocol/TProtocolException.cs \
            src/Protocol/TProtocolFactory.cs \
            src/Protocol/TList.cs \
            src/Protocol/TSet.cs \
            src/Protocol/TMap.cs \
            src/Protocol/TProtocolUtil.cs \
            src/Protocol/TMessageType.cs \
            src/Protocol/TProtocol.cs \
            src/Protocol/TType.cs \
            src/Protocol/TField.cs \
            src/Protocol/TMessage.cs \
            src/Protocol/TStruct.cs \
            src/Protocol/TBinaryProtocol.cs \
            src/Server/TThreadedServer.cs \
            src/Server/TThreadPoolServer.cs \
            src/Server/TSimpleServer.cs \
            src/Server/TServer.cs \
            src/Transport/TBufferedTransport.cs \
            src/Transport/TTransport.cs \
            src/Transport/TSocket.cs \
            src/Transport/TTransportException.cs \
            src/Transport/TStreamTransport.cs \
            src/Transport/TServerTransport.cs \
            src/Transport/TServerSocket.cs \
            src/Transport/TTransportFactory.cs \
            src/TProcessor.cs \
            src/TApplicationException.cs


CSC=gmcs

if NET_2_0
MONO_DEFINES=/d:NET_2_0
endif

all-local: Thrift.dll

Thrift.dll: $(THRIFTCODE)
	$(CSC) $(THRIFTCODE) /out:Thrift.dll /target:library $(MONO_DEFINES)

clean-local:
	$(RM) Thrift.dll

EXTRA_DIST = \
             $(THRIFTCODE) \
             ThriftMSBuildTask \
             src/Thrift.csproj \
             src/Thrift.sln
