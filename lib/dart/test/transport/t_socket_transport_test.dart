library thrift.test.transport.t_socket_transport_test;

import 'dart:async';
import 'dart:convert' show Utf8Codec;
import 'dart:typed_data' show Uint8List;

import 'package:crypto/crypto.dart' show CryptoUtils;
import 'package:mockito/mockito.dart';
import 'package:test/test.dart';
import 'package:thrift/thrift.dart';

void main() {

  const utf8Codec = const Utf8Codec();

  final requestText = 'my test request';
  final requestBytes = new Uint8List.fromList(utf8Codec.encode(requestText));
  final requestBase64 = CryptoUtils.bytesToBase64(requestBytes);

  final responseText = 'a response!';
  final responseBytes = new Uint8List.fromList(utf8Codec.encode(responseText));
  final responseBase64 = CryptoUtils.bytesToBase64(responseBytes);

  group('TClientTransport', () {
    test('Test client sending data over transport', () async {
      var socket = new FakeSocket();
      await socket.open();

      FakeProtocolFactory protocolFactory = new FakeProtocolFactory();
      protocolFactory.message = new TMessage("foo", TMessageType.CALL, 123);
      var transport = new TClientSocketTransport(socket, protocolFactory);

      transport.writeAll(requestBytes);
      expect(socket.sendPayload, isNull);

      Future responseReady = transport.flush();

      // allow microtask events to finish
      await new Future.value();

      expect(socket.sendPayload, isNotNull);
      expect(socket.sendPayload, requestBytes);

      // simulate a response
      protocolFactory.message = new TMessage("foo", TMessageType.REPLY, 123);
      socket.receiveFakeMessage(responseBase64);

      await responseReady;
      var buffer = new Uint8List(responseBytes.length);
      transport.readAll(buffer, 0, responseBytes.length);
      var bufferText = utf8Codec.decode(buffer);

      expect(bufferText, responseText);
    });

    test('Test response timeout', () async {
      var socket = new FakeSocket();
      await socket.open();

      FakeProtocolFactory protocolFactory = new FakeProtocolFactory();
      protocolFactory.message = new TMessage("foo", TMessageType.CALL, 123);
      var transport = new TClientSocketTransport(socket, protocolFactory, responseTimeout: Duration.ZERO);
      transport.writeAll(requestBytes);

      Future responseReady = transport.flush();

      expect(responseReady, throwsA(new isInstanceOf<TimeoutException>()));
    });
  }, timeout: new Timeout(new Duration(seconds: 1)));

  group('TServerTransport', () {
    test('Test server transport listens to socket', () async {
      var socket = new FakeSocket();
      await socket.open();
      expect(socket.isOpen, isTrue);

      var transport = new TServerSocketTransport(socket);
      expect(transport.hasReadData, isFalse);

      socket.receiveFakeMessage(requestBase64);

      // allow microtask events to finish
      await new Future.value();

      expect(transport.hasReadData, isTrue);

      var buffer = new Uint8List(requestBytes.length);
      transport.readAll(buffer, 0, requestBytes.length);

      var bufferText = utf8Codec.decode(buffer);
      expect(bufferText, requestText);
    });

    test('Test server sending data over transport', () async {
      var socket = new FakeSocket();
      await socket.open();

      var transport = new TServerSocketTransport(socket);

      transport.writeAll(responseBytes);
      expect(socket.sendPayload, isNull);

      transport.flush();

      // allow microtask events to finish
      await new Future.value();

      expect(socket.sendPayload, isNotNull);
      expect(socket.sendPayload, responseBytes);
    });
  }, timeout: new Timeout(new Duration(seconds: 1)));

}


class FakeSocket extends TSocket {

  final StreamController<TSocketState> _onStateController;
  Stream<TSocketState> get onState => _onStateController.stream;

  final StreamController<Object> _onErrorController;
  Stream<Object> get onError => _onErrorController.stream;

  final StreamController<Uint8List> _onMessageController;
  Stream<Uint8List> get onMessage => _onMessageController.stream;

  FakeSocket()
      : _onStateController = new StreamController.broadcast(),
        _onErrorController = new StreamController.broadcast(),
        _onMessageController = new StreamController.broadcast();

  bool _isOpen;

  bool get isOpen => _isOpen;

  bool get isClosed => !isOpen;

  Future open() async {
    _isOpen = true;
    _onStateController.add(TSocketState.OPEN);
  }

  Future close() async {
    _isOpen = false;
    _onStateController.add(TSocketState.CLOSED);
  }

  Uint8List _sendPayload;
  Uint8List get sendPayload => _sendPayload;

  void send(Uint8List data) {
    if (!isOpen) throw new StateError("The socket is not open");

    _sendPayload = data;
  }

  void receiveFakeMessage(String base64) {
    if (!isOpen) throw new StateError("The socket is not open");

    var message =
        new Uint8List.fromList(CryptoUtils.base64StringToBytes(base64));
    _onMessageController.add(message);
  }

}


class FakeProtocolFactory implements TProtocolFactory {

  FakeProtocolFactory();

  TMessage message;

  getProtocol(TTransport transport) => new FakeProtocol(message);

}

class FakeProtocol extends Mock implements TProtocol {

  FakeProtocol(this._message);

  TMessage _message;

  readMessageBegin() => _message;

}
