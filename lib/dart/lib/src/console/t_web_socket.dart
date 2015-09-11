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

library thrift.src.console;

import 'dart:async';
import 'dart:io';
import 'dart:typed_data' show Uint8List;

import 'package:crypto/crypto.dart' show CryptoUtils;
import 'package:thrift/thrift.dart';

/// A [TSocket] backed by a [WebSocket] from dart:io
class TWebSocket implements TSocket {
  final StreamController<TSocketState> _onStateController;
  Stream<TSocketState> get onState => _onStateController.stream;

  final StreamController<Object> _onErrorController;
  Stream<Object> get onError => _onErrorController.stream;

  final StreamController<Uint8List> _onMessageController;
  Stream<Uint8List> get onMessage => _onMessageController.stream;

  final List<Completer<Uint8List>> _completers = [];

  TWebSocket(WebSocket socket, {this.isServer: true})
      : _onStateController = new StreamController.broadcast(),
        _onErrorController = new StreamController.broadcast(),
        _onMessageController = new StreamController.broadcast() {
    if (socket == null) {
      throw new ArgumentError.notNull('socket');
    }

    _socket = socket;
    _socket.listen(_onMessage, onError: _onError, onDone: close);
  }

  final bool isServer;

  WebSocket _socket;

  bool get isOpen => _socket != null;

  bool get isClosed => _socket == null;

  Future open() async {
    _onStateController.add(TSocketState.OPEN);
  }

  Future close() async {
    if (_socket != null) {
      await _socket.close();
      _socket = null;
    }

    _onStateController.add(TSocketState.CLOSED);
  }

  Future<Uint8List> send(Uint8List data) async {
    Future result;
    if (isServer) {
      result = new Future.value();
    } else {
      // if we are a client, then we expect a result
      Completer<Uint8List> completer = new Completer();
      _completers.add(completer);
      result = completer.future;
    }

    _socket.add(CryptoUtils.bytesToBase64(data));

    return result;
  }

  void _onMessage(Object message) {
    Uint8List data;

    try {
      data = new Uint8List.fromList(CryptoUtils.base64StringToBytes(message));
    } on FormatException catch (_) {
      _onErrorController
          .add(new UnsupportedError("Expected a Base 64 encoded string."));
    }

    if (!_completers.isEmpty) {
      _completers.removeAt(0).complete(data);
    }

    _onMessageController.add(data);
  }

  void _onError(Object error) {
    close();
    _onErrorController.add("$error");
  }
}
