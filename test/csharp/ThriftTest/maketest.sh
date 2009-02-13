#!/bin/sh
../../../compiler/cpp/thrift --gen csharp -o . ../../ThriftTest.thrift
gmcs /t:library /out:./ThriftImpl.dll /recurse:./gen-csharp/* /reference:../../../lib/csharp/Thrift.dll
