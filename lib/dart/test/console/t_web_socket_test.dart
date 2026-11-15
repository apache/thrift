// Licensed to the Apache Software Foundation (ASF) under one
// or more contributor license agreements. See the NOTICE file
// distributed with this work for additional information
// regarding copyright ownership. The ASF licenses this file
// to you under the Apache License, Version 2.0 (the
// "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied. See the License for the
// specific language governing permissions and limitations
// under the License.

@TestOn('vm')
library thrift.test.console.t_web_socket_test;

import 'dart:io';
import 'dart:typed_data' show Uint8List;

import 'package:test/test.dart';
import 'package:thrift/thrift.dart';
import 'package:thrift/thrift_console.dart';

void main() {
  late HttpServer server;
  final serverSockets = <WebSocket>[];

  setUp(() async {
    server = await HttpServer.bind(InternetAddress.loopbackIPv4, 0);
  });

  tearDown(() async {
    for (var ws in serverSockets) {
      await ws.close();
    }
    serverSockets.clear();
    await server.close(force: true);
  });

  // The server answers the client's first message with [frame], so the test
  // listens on the socket's streams before the frame can arrive.
  Future<TWebSocket> connect(Object frame) async {
    server.listen((request) async {
      var ws = await WebSocketTransformer.upgrade(request);
      serverSockets.add(ws);
      ws.listen((_) => ws.add(frame));
    });
    return TWebSocket(
        await WebSocket.connect('ws://127.0.0.1:${server.port}/'));
  }

  test('A text frame is delivered as a message', () async {
    var socket = await connect('AQID'); // base64 of [1, 2, 3]
    var message = socket.onMessage.first.timeout(Duration(seconds: 5));
    socket.send(Uint8List.fromList([0]));
    expect(await message, [1, 2, 3]);
    await socket.close();
  });

  // A frame the socket cannot use has to be reported on onError.
  test('A binary frame is reported on onError', () async {
    var socket = await connect([1, 2, 3]);
    var error = socket.onError.first.timeout(Duration(seconds: 5));
    socket.send(Uint8List.fromList([0]));
    expect(await error, isA<TProtocolError>());
    await socket.close();
  });
}
