/// Licensed to the Apache Software Foundation (ASF) under one
/// or more contributor license agreements. See the NOTICE file
/// distributed with this work for additional information
/// regarding copyright ownership. The ASF licenses this file
/// to you under the Apache License, Version 2.0 (the
/// 'License'); you may not use this file except in compliance
/// with the License. You may obtain a copy of the License at
///
/// http://www.apache.org/licenses/LICENSE-2.0
///
/// Unless required by applicable law or agreed to in writing,
/// software distributed under the License is distributed on an
/// 'AS IS' BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
/// KIND, either express or implied. See the License for the
/// specific language governing permissions and limitations
/// under the License.

library thrift.src.console.t_web_socket;

import 'dart:async';
import 'dart:convert' show base64;
import 'dart:io';
import 'dart:typed_data' show Uint8List;

import 'package:thrift/thrift.dart';

/// A [TSocket] backed by a [WebSocket] from dart:io
class TWebSocket implements TSocket {
  final StreamController<TSocketState> _onStateController =
      StreamController.broadcast();
  @override
  Stream<TSocketState> get onState => _onStateController.stream;

  final StreamController<Object> _onErrorController =
      StreamController.broadcast();
  @override
  Stream<Object> get onError => _onErrorController.stream;

  final StreamController<Uint8List> _onMessageController =
      StreamController.broadcast();
  @override
  Stream<Uint8List> get onMessage => _onMessageController.stream;

  final WebSocket _socket;

  TWebSocket(this._socket) {
    _socket.listen(
      (dynamic message) => _onMessage(message as String),
      onError: (dynamic error) => _onError(error),
      onDone: () => close(),
    );
  }

  @override
  bool get isOpen =>
      true; // Since _socket is not nullable, it's always considered open

  @override
  bool get isClosed => false; // Similarly, it's never considered closed

  @override
  Future<void> open() async {
    _onStateController.add(TSocketState.OPEN);
  }

  @override
  Future<void> close() async {
    await _socket.close();
    _onStateController.add(TSocketState.CLOSED);
  }

  @override
  void send(Uint8List data) {
    _socket.add(base64.encode(data));
  }

  void _onMessage(String message) {
    try {
      Uint8List data = Uint8List.fromList(base64.decode(message));
      _onMessageController.add(data);
    } on FormatException catch (_) {
      var error = TProtocolError(TProtocolErrorType.INVALID_DATA,
          "Expected a Base 64 encoded string.");
      _onErrorController.add(error);
    }
  }

  void _onError(dynamic error) {
    _onErrorController.add('$error');
  }
}
