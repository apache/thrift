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

library thrift.src.html;

import 'dart:async';
import 'dart:convert' show Utf8Codec;
import 'dart:html' show CloseEvent;
import 'dart:html' show Event;
import 'dart:html' show MessageEvent;
import 'dart:html' show WebSocket;

import 'package:thrift/thrift.dart';


/// A [TSocket] backed by a [WebSocket] from dart:html
class TWebSocket implements TSocket {

  static const utf8Codec = const Utf8Codec();

  final Uri url;

  final StreamController<TSocketState> _onStateController;
  Stream<TSocketState> get onState => _onStateController.stream;

  final StreamController<Object> _onErrorController;
  Stream<Object> get onError => _onErrorController.stream;

  final List<Completer<List<int>>> _completers = [];
  final List<_Request> _requests = [];

  TWebSocket(this.url) :
    _onStateController = new StreamController.broadcast(),
    _onErrorController = new StreamController.broadcast() {
    if (url == null || !url.hasAuthority || !url.hasPort) {
      throw new ArgumentError("Invalid url");
    }
  }

  WebSocket _socket;

  bool get isOpen => _socket != null && _socket.readyState == WebSocket.OPEN;

  bool get isClosed => _socket == null || _socket.readyState == WebSocket.CLOSED;

  void open() {
    if (!isClosed) {
      throw new TTransportError(TTransportErrorType.ALREADY_OPEN,
                                "Socket already connected");
    }

    _socket = new WebSocket(url.toString());
    _socket.onError.listen(_onError);
    _socket.onOpen.listen(_onOpen);
    _socket.onClose.listen(_onClose);
    _socket.onMessage.listen(_onMessage);
  }

  void close() {
    if (_socket != null) {
      _socket.close();
    }
  }

  Future<List<int>> send(List<int> data) {
    Completer<List<int>> completer = new Completer();

    _requests.add(new _Request(data, completer));
    _sendRequests();

    return completer.future;
  }

  void _sendRequests() {
    while (isOpen && _requests.length > 0) {
      _Request request = _requests.removeAt(0);
      _completers.add(request.completer);
      _socket.sendString(utf8Codec.decode(request.data));
    }
  }

  void _onOpen(Event event) {
    _onStateController.add(TSocketState.OPEN);
    _sendRequests();
  }

  void _onClose(CloseEvent event) {
    _socket = null;
    _completers.clear();
    _requests.clear();

    _onStateController.add(TSocketState.CLOSED);
  }

  void _onMessage(MessageEvent event) {
    List<int> data;

    if (event.data is String) {
      data = utf8Codec.encode(event.data);
    } else {
      data = event.data;
    }

    if (!_completers.isEmpty) {
      _completers.removeAt(0).complete(data);
    }
  }

  void _onError(Event event) {
    close();
    _onErrorController.add(event.toString());
  }
}

class _Request {
  final List<int> data;
  final Completer<List<int>> completer;

  _Request(this.data, this.completer);
}
