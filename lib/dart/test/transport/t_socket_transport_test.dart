library thrift.test.transport.t_socket_transport_test;

import 'dart:async';
import 'dart:convert' show Utf8Codec;
import 'dart:typed_data' show Uint8List;

import 'package:test/test.dart';
import 'package:thrift/thrift.dart';

void main() {

  const utf8Codec = const Utf8Codec();

  final expectedText = 'my test data';
  final expectedBytes = new Uint8List.fromList(utf8Codec.encode(expectedText));

  test('Test transport listens to socket', () async {
    var socket = new FakeSocket(isServer: true);
    await socket.open();
    expect(socket.isOpen, isTrue);

    var transport = new TSocketTransport(socket, isListening: true);
    expect(transport.hasReadData, isFalse);

    socket.receiveFakeMessage(expectedBytes);

    // allow microtask events to finish
    await new Future.value();

    expect(transport.hasReadData, isTrue);

    var buffer = new Uint8List(expectedBytes.length);
    transport.readAll(buffer, 0, expectedBytes.length);

    var bufferText = utf8Codec.decode(buffer);

    expect(bufferText, expectedText);
  });

  test('Test transport does not listen to socket', () async {
    var socket = new FakeSocket();
    await socket.open();

    var transport = new TSocketTransport(socket, isListening: false);

    socket.receiveFakeMessage(expectedBytes);

    // allow microtask events to finish
    await new Future.value();

    expect(transport.hasReadData, isFalse);
  });

  test('Test sending data over transport', () async {
    var socket = new FakeSocket(isServer: true);
    await socket.open();

    var transport = new TSocketTransport(socket, isListening: false);

    transport.writeAll(expectedBytes);
    expect(socket.sendPayload, isNull);

    transport.flush();

    // allow microtask events to finish
    await new Future.value();

    expect(socket.sendPayload, isNotNull);
    expect(utf8Codec.decode(socket.sendPayload), expectedText);
  });

}


class FakeSocket extends TSocket {

  final StreamController<TSocketState> _onStateController;
  Stream<TSocketState> get onState => _onStateController.stream;

  final StreamController<Object> _onErrorController;
  Stream<Object> get onError => _onErrorController.stream;

  final StreamController<Uint8List> _onMessageController;
  Stream<Uint8List> get onMessage => _onMessageController.stream;

  final List<Completer<Uint8List>> _completers = [];

  FakeSocket({this.isServer: false})
      : _onStateController = new StreamController.broadcast(),
        _onErrorController = new StreamController.broadcast(),
        _onMessageController = new StreamController.broadcast();

  final bool isServer;

  bool _isOpen;

  bool get isOpen => _isOpen;

  bool get isClosed => !isOpen;

  Future open() async {
    _isOpen = true;
    _onStateController.add(TSocketState.OPEN);
  }

  Future close() async {
    _isOpen = false;
    for (var completer in _completers) {
      completer.completeError(new StateError('The socked has closed'));
    }
    _onStateController.add(TSocketState.CLOSED);
  }

  Uint8List _sendPayload;

  Uint8List get sendPayload => _sendPayload;

  Future<Uint8List> send(Uint8List data) {
    if (!isOpen) throw new StateError("The socket is not open");

    Future<Uint8List> result;
    if (isServer) {
      result = new Future.value();
    } else {
      Completer<Uint8List> completer = new Completer();
      _completers.add(completer);
      result = completer.future;
    }

    _sendPayload = data;

    return result;
  }

  void receiveFakeMessage(Uint8List message) {
    if (!isOpen) throw new StateError("The socket is not open");

    if (!_completers.isEmpty) {
      _completers.removeAt(0).complete(message);
    }

    _onMessageController.add(message);
  }

}
