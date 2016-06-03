/// Licensed to the Apache Software Foundation (ASF) under one
/// or more contributor license agreements. See the NOTICE file
/// distributed with this work for additional information
/// regarding copyright ownership. The ASF licenses this file
/// to you under the Apache License, Version 2.0 (the
/// "License"); you may not use this file except in compliance
/// with the License. You may obtain a copy of the License at
///
/// http://www.apache.org/licenses/LICENSE-2.0
///
/// Unless required by applicable law or agreed to in writing,
/// software distributed under the License is distributed on an
/// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
/// KIND, either express or implied. See the License for the
/// specific language governing permissions and limitations
/// under the License.

part of thrift;

class TSerializer {
  final message = new TMessage('Serializer', TMessageType.ONEWAY, 1);
  TTransport transport;
  TProtocol protocol;

  TSerializer() {
    this.transport = new TBufferedTransport();
    this.protocol = new TBinaryProtocolFactory().getProtocol(transport);
  }

  Uint8List write(TBase msg) {
//    protocol.writeMessageBegin(message);
    
    msg.write(protocol);
    
//    protocol.writeMessageEnd();
    
    protocol.transport.flush();

    return protocol.readBinary();
  }

  String writeString(TBase msg) {
    protocol.writeMessageBegin(message);

    msg.write(protocol);
    
    protocol.writeMessageEnd();

    protocol.transport.flush();

    return protocol.readString();
  }
}
