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

library thrift.test.transport.t_socket_transport_test;

import 'dart:async';
import 'dart:convert' show Encoding;
import 'dart:convert' show Utf8Codec;
import 'dart:convert' show base64;
import 'dart:typed_data' show Uint8List;

import 'package:http/http.dart' show BaseRequest;
import 'package:http/http.dart' show Client;
import 'package:http/http.dart' show ClientException;
import 'package:http/http.dart' show Response;
import 'package:http/http.dart' show StreamedResponse;
import 'package:test/test.dart';
import 'package:thrift/thrift.dart';

void main() {
  const utf8Codec = Utf8Codec();

  group('THttpClientTransport', () {
    late FakeHttpClient client;
    late THttpClientTransport transport;

    setUp(() {
      client = FakeHttpClient(sync: false);
      var config = THttpConfig(Uri.parse('http://localhost'), {});
      transport = THttpClientTransport(client, config);
    });

    test('Test transport sends body', () async {
      var expectedText = 'my request';
      transport.writeAll(utf8Codec.encode(expectedText));

      expect(client.postRequest, isEmpty);

      await transport.flush();

      expect(client.postRequest, isNotEmpty);

      var requestText = utf8Codec.decode(base64.decode(client.postRequest));
      expect(requestText, expectedText);
    });

    test('Test transport receives response', () async {
      var expectedText = 'my response';
      var expectedBytes = utf8Codec.encode(expectedText);
      client.postResponse = base64.encode(expectedBytes);

      transport.writeAll(utf8Codec.encode('my request'));
      expect(transport.hasReadData, isFalse);

      await transport.flush();

      expect(transport.hasReadData, isTrue);

      var buffer = Uint8List(expectedBytes.length);
      transport.readAll(buffer, 0, expectedBytes.length);

      var bufferText = utf8Codec.decode(buffer);
      expect(bufferText, expectedText);
    });
  });

  group('THttpClientTransport with multiple messages', () {
    late FakeHttpClient client;
    late THttpClientTransport transport;

    setUp(() {
      client = FakeHttpClient(sync: true);
      var config = THttpConfig(Uri.parse('http://localhost'), {});
      transport = THttpClientTransport(client, config);
    });

    test('Test read correct buffer after flush', () async {
      String bufferText= "";
      var expectedText = 'response 1';
      var expectedBytes = utf8Codec.encode(expectedText);

      // prepare a response
      transport.writeAll(utf8Codec.encode('request 1'));
      client.postResponse = base64.encode(expectedBytes);

      Future responseReady = transport.flush().then((_) {
        var buffer = Uint8List(expectedBytes.length);
        transport.readAll(buffer, 0, expectedBytes.length);
        bufferText = utf8Codec.decode(buffer);
      });

      // prepare a second response
      transport.writeAll(utf8Codec.encode('request 2'));
      var response2Bytes = utf8Codec.encode('response 2');
      client.postResponse = base64.encode(response2Bytes);
      await transport.flush();

      await responseReady;
      expect(bufferText, expectedText);
    });
  });

  // A failure has to reach the caller of flush(), through the future that
  // flush() returned.
  group('THttpClientTransport errors', () {
    for (var sync in [false, true]) {
      test(
          'flush fails with TProtocolError on a reply that is not base64 '
          '(sync: $sync)', () async {
        var client = FakeHttpClient(sync: sync)
          ..postResponse = '%%% not base64 %%%';
        var config = THttpConfig(Uri.parse('http://localhost'), {});
        var transport = THttpClientTransport(client, config);
        transport.writeAll(utf8Codec.encode('my request'));
        await expectLater(transport.flush(), throwsA(isA<TProtocolError>()));
      });

      test('flush fails when the request fails (sync: $sync)', () async {
        var client = FailingHttpClient(sync: sync);
        var config = THttpConfig(Uri.parse('http://localhost'), {});
        var transport = THttpClientTransport(client, config);
        transport.writeAll(utf8Codec.encode('my request'));
        await expectLater(transport.flush(), throwsA(isA<ClientException>()));
      });
    }
  }, timeout: Timeout(Duration(seconds: 5)));
}

class FailingHttpClient extends FakeHttpClient {
  FailingHttpClient({bool sync = false}) : super(sync: sync);

  @override
  Future<Response> post(url,
      {Map<String, String>? headers, body, Encoding? encoding}) {
    var error = ClientException('connection refused');
    return sync ? Future.sync(() => throw error) : Future.error(error);
  }
}

class FakeHttpClient implements Client {
  String postResponse = '';
  String postRequest = '';

  final bool sync;

  FakeHttpClient({this.sync = false});

  @override
  Future<Response> post(url,
      {Map<String, String>? headers, body, Encoding? encoding}) {
    postRequest = body.toString();
    var response = Response(postResponse, 200);

    if (sync) {
      return Future.sync(() => response);
    } else {
      return Future.value(response);
    }
  }

  @override
  Future<Response> head(url, {Map<String, String>? headers}) =>
      throw UnimplementedError();

  @override
  Future<Response> get(url, {Map<String, String>? headers}) =>
      throw UnimplementedError();

  @override
  Future<Response> put(url,
          {Map<String, String>? headers, body, Encoding? encoding}) =>
      throw UnimplementedError();

  @override
  Future<Response> patch(url,
          {Map<String, String>? headers, body, Encoding? encoding}) =>
      throw UnimplementedError();

  @override
  Future<Response> delete(Uri url,
          {Map<String, String>? headers, Object? body, Encoding? encoding}) =>
      throw UnimplementedError();

  @override
  Future<String> read(url, {Map<String, String>? headers}) =>
      throw UnimplementedError();

  @override
  Future<Uint8List> readBytes(url, {Map<String, String>? headers}) =>
      throw UnimplementedError();

  @override
  Future<StreamedResponse> send(BaseRequest request) =>
      throw UnimplementedError();

  @override
  void close() => throw UnimplementedError();
}
