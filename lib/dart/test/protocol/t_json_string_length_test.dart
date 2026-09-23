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

library thrift.test.protocol.t_json_string_length_test;

import 'dart:typed_data';

import 'package:test/test.dart';
import 'package:thrift/thrift.dart';

/// A JSON string or number is delimited rather than length-prefixed, so its
/// size is whatever the peer sends -- there is no declared length to reject up
/// front, and every value, including the method name of a message, is held to
/// the maximum string size as it is read. This binding has no TConfiguration,
/// so the maximum is a knob of its own with the same default as the binary and
/// compact protocols.
///
/// The counting transport is what separates "stopped at the maximum" from
/// "read the whole value and returned it" -- a bare "did it throw?" check
/// cannot tell the two apart, because a short buffer fails the read regardless.
/// The payloads are plain ASCII, so one transport byte is read per accumulated
/// byte and the read stops within a few bytes of the maximum.
void main() {
  Uint8List wire(String s) => Uint8List.fromList(s.codeUnits);

  final Matcher throwsSizeLimit = throwsA(predicate(
      (e) => e is TProtocolError && e.type == TProtocolErrorType.SIZE_LIMIT));

  group('JSON string length', () {
    test('the maximum has a usable default', () {
      expect(TJsonProtocol(_CountingTransport(Uint8List(0))).maxStringSize,
          defaultMaxStringSize);
    });

    test('a string over a lowered maximum is refused near the maximum', () {
      final inner = _CountingTransport(wire('"${'a' * 64}"'));
      final protocol = TJsonProtocol(inner, maxStringSize: 32);

      expect(() => protocol.readString(), throwsSizeLimit);
      expect(inner.bytesRequested, lessThanOrEqualTo(32 + 8));
    });

    test('a string at the maximum still reads', () {
      final inner = _CountingTransport(wire('"${'a' * 32}"'));
      final protocol = TJsonProtocol(inner, maxStringSize: 32);

      expect(protocol.readString(), 'a' * 32);
    });

    test('one byte over the maximum is refused', () {
      final inner = _CountingTransport(wire('"${'a' * 33}"'));
      final protocol = TJsonProtocol(inner, maxStringSize: 32);

      expect(() => protocol.readString(), throwsSizeLimit);
    });

    test('a number over the maximum is refused near the maximum', () {
      final inner = _CountingTransport(wire('${'1' * 64} '));
      final protocol = TJsonProtocol(inner, maxStringSize: 32);

      expect(() => protocol.readI64(), throwsSizeLimit);
      expect(inner.bytesRequested, lessThanOrEqualTo(32 + 8));
    });

    test('a number at the maximum still reads', () {
      // 32 numeric characters, with leading zeros so the value stays small.
      final inner = _CountingTransport(wire('${'0' * 30}42 '));
      final protocol = TJsonProtocol(inner, maxStringSize: 32);

      expect(protocol.readI64(), 42);
    });

    test('a base64 value over the maximum is refused near the maximum', () {
      final inner = _CountingTransport(wire('"${'QUJD' * 16}"'));
      final protocol = TJsonProtocol(inner, maxStringSize: 32);

      expect(() => protocol.readBinary(), throwsSizeLimit);
      expect(inner.bytesRequested, lessThanOrEqualTo(32 + 8));
    });

    test('a base64 value at the maximum still reads', () {
      final inner = _CountingTransport(wire('"${'QUJD' * 8}"'));
      final protocol = TJsonProtocol(inner, maxStringSize: 32);

      expect(protocol.readBinary(), wire('ABC' * 8));
    });

    test('a message name over the maximum is refused near the maximum', () {
      final inner = _CountingTransport(wire('[1,"${'a' * 64}",1,0]'));
      final protocol = TJsonProtocol(inner, maxStringSize: 32);

      expect(() => protocol.readMessageBegin(), throwsSizeLimit);
      // The framing bytes [1," plus the maximum and a byte or two.
      expect(inner.bytesRequested, lessThanOrEqualTo(32 + 12));
    });
  });
}

/// Serves a fixed list of bytes and records how much was asked of it.
class _CountingTransport extends TTransport {
  final Uint8List _data;
  int _pos = 0;
  int bytesRequested = 0;

  _CountingTransport(this._data);

  @override
  bool get isOpen => true;

  @override
  Future open() async {}

  @override
  Future close() async {}

  @override
  int read(Uint8List buffer, int offset, int length) {
    bytesRequested += length;
    final available = _data.length - _pos;
    final give = available < length ? available : length;
    if (give <= 0) return 0;
    buffer.setRange(offset, offset + give, _data, _pos);
    _pos += give;
    return give;
  }

  @override
  void write(Uint8List buffer, int offset, int length) {}

  @override
  Future flush() async {}
}
