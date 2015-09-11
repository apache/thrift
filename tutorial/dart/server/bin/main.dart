import 'dart:async';
import 'dart:io';

import 'package:thrift/thrift.dart';
import 'package:thrift/thrift_console.dart';
import 'package:tutorial/tutorial.dart';
import 'package:shared/shared.dart';

TProtocol _inputProtocol;
TProtocol _outputProtocol;
TProcessor _processor;
WebSocket _webSocket;

main(List<String> args) {
  int port = 9090;
  if (!args.isEmpty) {
    port = int.parse(args[0]);
  }

  _runServer(port);
}

Future _runServer(int port) async {
  var httpServer = await HttpServer.bind('127.0.0.1', port);
  httpServer.listen((HttpRequest request) => _handleRequest(request));

  print('listening for connections on $port');
}

Future _handleRequest(HttpRequest request) async {
  if (request.uri.path == '/ws') {
    await _initWebSocket(request);
  } else {
    print('Invalid path: ${request.uri.path}');
  }
}

Future _initWebSocket(HttpRequest request) async {
  _webSocket = await WebSocketTransformer.upgrade(request);

  TWebSocket socket = new TWebSocket(_webSocket);

  _processor = new CalculatorProcessor(new CalculatorServer());
  _inputProtocol =
      new TJsonProtocol(new TSocketTransport(socket, isListening: true));
  await _inputProtocol.transport.open();

  _outputProtocol = new TJsonProtocol(new TSocketTransport(socket));
  await _outputProtocol.transport.open();

  socket.onError.listen((error) => print('Error: $error'));
  socket.onMessage.listen(_onMessage);
}

Future _onMessage(List<int> data) async {
  _processor.process(_inputProtocol, _outputProtocol);
}

class CalculatorServer implements Calculator {
  final Map<int, SharedStruct> _log = {};

  Future ping() async {
    print('ping()');
  }

  Future<int> add(int num1, int num2) async {
    print('add($num1, $num2)');

    return num1 + num2;
  }

  Future<int> calculate(int logid, Work work) async {
    print('calulate($logid, ${work.toString()})');

    int val;

    switch (work.op) {
      case Operation.ADD:
        val = work.num1 + work.num2;
        break;

      case Operation.SUBTRACT:
        val = work.num1 - work.num2;
        break;

      case Operation.MULTIPLY:
        val = work.num1 * work.num2;
        break;

      case Operation.DIVIDE:
        if (work.num2 == 0) {
          var x = new InvalidOperation();
          x.whatOp = work.op;
          x.why = 'Cannot divide by 0';
          throw x;
        }
        val = (work.num1 / work.num2).floor();
        break;
    }

    var log = new SharedStruct();
    log.key = logid;
    log.value = '$val "${work.comment}"';
    this._log[logid] = log;

    return val;
  }

  Future zip() async {
    print('zip()');
  }

  Future<SharedStruct> getStruct(int key) async {
    print('getStruct($key)');

    return _log[key];
  }
}
