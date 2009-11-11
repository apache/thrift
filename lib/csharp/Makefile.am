#
# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements. See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership. The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License. You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.
#

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
            src/Transport/TFramedTransport.cs \
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
