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

/// The server side of t_web_socket_test.dart, run with spawnHybridUri in a
/// dart:io isolate.
library thrift.test.browser.t_web_socket_server;

import 'dart:io';

/// Sends the server's port on [channel], then answers the first message of
/// each WebSocket client with one frame: base64 text when [message] is 'text',
/// the bytes themselves when it is 'binary'.
Future<void> hybridMain(dynamic channel, Object? message) async {
  var server = await HttpServer.bind(InternetAddress.loopbackIPv4, 0);
  var sockets = <WebSocket>[];
  server.listen((request) async {
    var ws = await WebSocketTransformer.upgrade(request);
    sockets.add(ws);
    ws.listen((_) => ws.add(message == 'binary' ? [1, 2, 3] : 'AQID'));
  });
  channel.stream.listen((_) {}, onDone: () async {
    for (var ws in sockets) {
      await ws.close();
    }
    await server.close(force: true);
  });
  channel.sink.add(server.port);
}
