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

library thrift.src.console.t_tcp_socket;

import 'dart:async';
import 'dart:io';
import 'dart:typed_data' show Uint8List;

import 'package:thrift/thrift.dart';

/// A [TSocket] backed by a [Socket] from dart:io
class TTcpSocket implements TSocket {
  final StreamController<TSocketState> _onStateController;
  @override
  Stream<TSocketState> get onState => _onStateController.stream;

  final StreamController<Object> _onErrorController;
  @override
  Stream<Object> get onError => _onErrorController.stream;

  final StreamController<Uint8List> _onMessageController;
  @override
  Stream<Uint8List> get onMessage => _onMessageController.stream;

  TTcpSocket(Socket socket)
      : _onStateController = StreamController.broadcast(),
        _onErrorController = StreamController.broadcast(),
        _onMessageController = StreamController.broadcast() {
    if (socket == null) {
      throw ArgumentError.notNull('socket');
    }

    _socket = socket;
    _socket.listen(_onMessage, onError: _onError, onDone: close);
  }

  Socket _socket;

  @override
  bool get isOpen => _socket != null;

  @override
  bool get isClosed => _socket == null;

  @override
  Future open() async {
    _onStateController.add(TSocketState.OPEN);
  }

  @override
  Future close() async {
    if (_socket != null) {
      await _socket.close();
      _socket = null;
    }

    _onStateController.add(TSocketState.CLOSED);
  }

  @override
  void send(Uint8List data) {
    _socket.add(data);
  }

  void _onMessage(List<int> message) {
    Uint8List data = Uint8List.fromList(message);
    _onMessageController.add(data);
  }

  void _onError(Object error) {
    close();
    _onErrorController.add('$error');
  }
}
