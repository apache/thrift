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
///     var transport = new TClientSocketTransport(new TWebSocket(url));
///     var protocol = new TBinaryProtocol(transport);
///     var client = new MyThriftServiceClient(protocol);
///     var result = client.myMethod();
///
/// Adapted from the JS WebSocket transport.
abstract class TSocketTransport extends TBufferedTransport {
  final Logger log = new Logger('thrift.TSocketTransport');

  final TSocket socket;

  /// A transport using the provided [socket].
  TSocketTransport(this.socket) {
    if (socket == null) {
      throw new ArgumentError.notNull('socket');
    }

    socket.onError.listen((String e) => log.warning(e));
    socket.onMessage.listen(handleIncomingMessage);
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

  /// Make an incoming message available to read from the transport.
  void handleIncomingMessage(Uint8List message) {
    _setReadBuffer(message);
  }

  /// Send the bytes in the write buffer to the socket
  void sendMessage() {
    Uint8List message = _consumeWriteBuffer();
    socket.send(message);
  }
}

/// [TClientSocketTransport] sends outgoing messages and expects a response
///
/// NOTE:  Currently this assumes serialized responses from a single threaded
/// server.
///
/// TODO Give [TClientSocketTransport] more information so it can correlate
/// requests and responses, e.g. a protocol-aware function that can read the
/// sequence id from the message header.
class TClientSocketTransport extends TSocketTransport {
  final List<Completer<Uint8List>> _completers = [];

  TClientSocketTransport(TSocket socket) : super(socket);

  Future flush() {
    Completer completer = new Completer();
    _completers.add(completer);

    sendMessage();

    return completer.future;
  }

  void handleIncomingMessage(Uint8List message) {
    super.handleIncomingMessage(message);

    if (_completers.isNotEmpty) {
      _completers.removeAt(0).complete();
    }
  }
}

/// [TServerSocketTransport] listens for incoming messages.  When it sends a
/// response, it does not expect an acknowledgement.
class TServerSocketTransport extends TSocketTransport {
  final StreamController _onIncomingMessageController;
  Stream get onIncomingMessage => _onIncomingMessageController.stream;

  TServerSocketTransport(TSocket socket)
      : _onIncomingMessageController = new StreamController.broadcast(),
        super(socket);

  Future flush() async {
    sendMessage();
  }

  void handleIncomingMessage(Uint8List message) {
    super.handleIncomingMessage(message);

    _onIncomingMessageController.add(null);
  }
}
