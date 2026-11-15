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

@TestOn('browser')
library thrift.test.browser.t_web_socket_test;

import 'dart:typed_data' show Uint8List;

import 'package:test/test.dart';
import 'package:thrift/thrift.dart';
import 'package:thrift/thrift_browser.dart';

void main() {
  // A dart:io server in a hybrid isolate answers the client's first message
  // with one frame, so the test listens on the socket's streams before the
  // frame can arrive.
  Future<TWebSocket> connect(String frameKind) async {
    var server = spawnHybridUri('t_web_socket_server.dart', message: frameKind);
    var port = await server.stream.first as int;
    var socket = TWebSocket(Uri.parse('ws://127.0.0.1:$port/'));
    await socket.open();
    addTearDown(() async {
      await socket.close();
      await server.sink.close();
    });
    return socket;
  }

  test('A text frame is delivered as a message', () async {
    var socket = await connect('text');
    var message = socket.onMessage.first.timeout(Duration(seconds: 5));
    socket.send(Uint8List.fromList([0]));
    expect(await message, [1, 2, 3]);
  });

  // A frame the socket cannot use has to be reported on onError.
  test('A binary frame is reported on onError', () async {
    var socket = await connect('binary');
    var error = socket.onError.first.timeout(Duration(seconds: 5));
    socket.send(Uint8List.fromList([0]));
    expect(await error, isA<TProtocolError>());
  });
}
