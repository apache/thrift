

../../../../compiler/cpp/thrift --gen csharp -o . ../../../../test/ThriftTest.thrift
gmcs /t:library /out:./ThriftImpl.dll /recurse:./gen-csharp/* /reference:../../Thrift.dll
gmcs /out:TestStreamServer.exe /reference:../../Thrift.dll /reference:ThriftImpl.dll TestHandler.cs Program.cs

timeout 120 ../ThriftTest/TestClientServer.exe ioStreamClient