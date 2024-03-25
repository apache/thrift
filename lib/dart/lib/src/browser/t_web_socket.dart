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

library thrift.src.browser;

import 'dart:async';
import 'dart:convert' show base64;
import 'dart:html' show CloseEvent, Event, MessageEvent, WebSocket;
import 'dart:typed_data' show Uint8List;

import 'package:thrift/thrift.dart';

/// A [TSocket] backed by a [WebSocket] from dart:html
class TWebSocket implements TSocket {
  final Uri url;

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

  final List<Uint8List> _requests = [];

  TWebSocket(this.url) {
    if (!url.hasAuthority || !url.hasPort) {
      throw ArgumentError('Invalid url');
    }
  }

  late WebSocket _socket;

  @override
  bool get isOpen => _socket.readyState == WebSocket.OPEN;

  @override
  bool get isClosed => _socket.readyState == WebSocket.CLOSED;

  @override
  Future<void> open() async {
    if (!isClosed) {
      throw TTransportError(
          TTransportErrorType.ALREADY_OPEN, 'Socket already connected');
    }

    _socket = WebSocket(url.toString());
    _socket.onError.listen(_onError);
    await _socket.onOpen.first;
    _socket.onClose.listen(_onClose);
    _socket.onMessage.listen(_onMessage);

    _onStateController.add(TSocketState.OPEN);
    _sendRequests();
  }

  @override
  Future<void> close() async {
    if (_socket.readyState == WebSocket.OPEN) {
      _socket.close();
      await _socket.onClose.first;
    }
    _onStateController.add(TSocketState.CLOSED);
  }

  @override
  void send(Uint8List data) {
    _requests.add(data);
    _sendRequests();
  }

  void _sendRequests() {
    while (isOpen && _requests.isNotEmpty) {
      Uint8List data = _requests.removeAt(0);
      _socket.sendString(base64.encode(data));
    }
  }

  void _onClose(CloseEvent event) {
    _socket = WebSocket('');
    if (_requests.isNotEmpty) {
      _onErrorController
          .add(StateError('Socket was closed with pending requests'));
    }
    _requests.clear();
    _onStateController.add(TSocketState.CLOSED);
  }

  void _onMessage(MessageEvent message) {
    try {
      Uint8List data =
          Uint8List.fromList(base64.decode(message.data as String));
      _onMessageController.add(data);
    } on FormatException catch (_) {
      var error = TProtocolError(TProtocolErrorType.INVALID_DATA,
          "Expected a Base 64 encoded string.");
      _onErrorController.add(error);
    }
  }

  void _onError(Event event) {
    close();
    _onErrorController.add(event.toString());
  }
}
