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

  final bool isListening;

  /// A transport using the provided [socket].  If [isListening] is true, then
  /// messages received from [socket] will be written to the transport and made
  /// available for reading.
  TSocketTransport(this.socket, {isListening: false})
      : this.isListening = isListening {
    if (socket == null) {
      throw new ArgumentError.notNull('socket');
    }

    socket.onError.listen((String e) => log.warning(e));

    if (isListening) {
      socket.onMessage.listen(_onMessage);
    }
  }

  bool get isOpen => socket.isOpen;

  Future open() {
    _reset(isOpen: true);
    return socket.open();
  }

  Future close() {
    _reset(isOpen: false);
    return socket.close();
  }

  Future flush() async {
    if (isListening) {
      _setReadBuffer(_consumeWriteBuffer());
    } else {
      Uint8List result = await socket.send(_consumeWriteBuffer());
      _setReadBuffer(result);
    }
  }

  void _onMessage(Uint8List message) {
    writeAll(message);
    _setReadBuffer(_consumeWriteBuffer());
  }
}
