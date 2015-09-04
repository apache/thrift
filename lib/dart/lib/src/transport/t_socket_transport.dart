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

/// Socket implementation of [TTransport].
///
/// For example:
///
///     var transport = new TSocketTransport(new TWebSocket(url));
///     var protocol = new TBinaryProtocol(transport);
///     var client = new MyThriftServiceClient(protocol);
///     var result = client.myMethod();
///
/// Adapted from the JS WebSocket transport.
class TSocketTransport extends TBufferedTransport {
  final TSocket socket;
  final Logger log = new Logger('thrift.TSocketTransport');

  TSocketTransport(this.socket) {
    if (socket == null) {
      throw new ArgumentError.notNull("socket");
    }

    socket.onError.listen((String e) => log.warning(e));
  }

  bool get isOpen => socket.isOpen;

  void open() {
    super.open();
    socket.open();
  }

  void close() {
    super.close();
    socket.close();
  }

  Future flush() async {
    List<int> result = await socket.send(_consumeWriteBuffer());
    _setReadBuffer(result);
  }
}
