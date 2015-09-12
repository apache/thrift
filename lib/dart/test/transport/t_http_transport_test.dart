library thrift.test.transport.t_socket_transport_test;

import 'dart:async';
import 'dart:convert' show Encoding;
import 'dart:convert' show Utf8Codec;
import 'dart:typed_data' show Uint8List;

import 'package:crypto/crypto.dart' show CryptoUtils;
import 'package:http/http.dart' show BaseRequest;
import 'package:http/http.dart' show Client;
import 'package:http/http.dart' show Response;
import 'package:http/http.dart' show StreamedResponse;
import 'package:test/test.dart';
import 'package:thrift/thrift.dart';

void main() {

  const utf8Codec = const Utf8Codec();

  FakeHttpClient client;
  THttpClientTransport transport;

  setUp(() {
    client = new FakeHttpClient();
    var config = new THttpConfig(Uri.parse("http://localhost"), {});
    transport = new THttpClientTransport(client, config);
  });

  test('Test transport sends body', () async {
    var expectedText = "my request";
    transport.writeAll(utf8Codec.encode(expectedText));

    expect(client.postRequest, isEmpty);

    await transport.flush();

    expect(client.postRequest, isNotEmpty);

    var requestText = utf8Codec.decode(
        CryptoUtils.base64StringToBytes(client.postRequest));
    expect(requestText, expectedText);
  });

  test('Test transport receives response', () async {
    var expectedText = "my response";
    var expectedBytes = utf8Codec.encode(expectedText);
    client.postResponse = CryptoUtils.bytesToBase64(expectedBytes);

    transport.writeAll(utf8Codec.encode("my request"));
    expect(transport.hasReadData, isFalse);

    await transport.flush();

    expect(transport.hasReadData, isTrue);

    var buffer = new Uint8List(expectedBytes.length);
    transport.readAll(buffer, 0, expectedBytes.length);

    var bufferText = utf8Codec.decode(buffer);
    expect(bufferText, expectedText);
  });

}


class FakeHttpClient implements Client {

  String postResponse = "";
  String postRequest = "";

  Future<Response> post(url, {Map<String, String> headers, body,
      Encoding encoding}) async {
    postRequest = body;
    return new Response(postResponse, 200);
  }

  Future<Response> head(url, {Map<String, String> headers}) =>
      throw new UnimplementedError();

  Future<Response> get(url, {Map<String, String> headers}) =>
      throw new UnimplementedError();

  Future<Response> put(url, {Map<String, String> headers, body,
      Encoding encoding}) => throw new UnimplementedError();

  Future<Response> patch(url, {Map<String, String> headers, body,
      Encoding encoding}) => throw new UnimplementedError();

  Future<Response> delete(url, {Map<String, String> headers}) =>
      throw new UnimplementedError();

  Future<String> read(url, {Map<String, String> headers}) =>
      throw new UnimplementedError();

  Future<Uint8List> readBytes(url, {Map<String, String> headers}) =>
      throw new UnimplementedError();

  Future<StreamedResponse> send(BaseRequest request) =>
      throw new UnimplementedError();

  void close() => throw new UnimplementedError();

}
