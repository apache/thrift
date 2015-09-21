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

import 'dart:async';
import 'dart:io';

import 'package:logging/logging.dart';
import 'package:thrift/thrift.dart';
import 'package:thrift/thrift_console.dart';
import 'package:tutorial/tutorial.dart';
import 'package:shared/shared.dart';

TProtocol _protocol;
TProcessor _processor;
WebSocket _webSocket;

main(List<String> args) {
  Logger.root.level = Level.ALL;
  Logger.root.onRecord.listen((LogRecord rec) {
    print('${rec.level.name}: ${rec.time}: ${rec.message}');
  });

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
  TServerSocketTransport transport = new TServerSocketTransport(socket);
  transport.onIncomingMessage.listen(_processMessage);

  _processor = new CalculatorProcessor(new CalculatorServer());
  _protocol = new TJsonProtocol(transport);
  await _protocol.transport.open();
}

Future _processMessage(_) async {
  _processor.process(_protocol, _protocol);
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
