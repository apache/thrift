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

library thrift.test.protocol.t_declared_length_test;

import 'dart:typed_data';

import 'package:test/test.dart';
import 'package:thrift/thrift.dart';

/// A frame length and a field length are numbers the peer chose, and
/// `Uint8List(n)` commits all of n before a single one of those bytes has
/// arrived. This binding has no TConfiguration, so both are held to a knob of
/// their own.
///
/// The counting transport is what separates "refused the declared length" from
/// "tried to read it and ran out" -- a bare "did it throw?" check passes either
/// way, because a short buffer fails the read regardless.
void main() {
  Uint8List i32(int value) {
    final out = Uint8List(4);
    out.buffer.asByteData().setInt32(0, value);
    return out;
  }

  Uint8List concat(Uint8List a, Uint8List b) =>
      Uint8List.fromList(<int>[...a, ...b]);

  group('protocol field length', () {
    test('a field declaring 2 GB is refused, without reading it', () {
      final inner = _CountingTransport(i32(0x7FFFFFFF));
      final protocol = TBinaryProtocol(inner);

      expect(
          () => protocol.readBinary(),
          throwsA(predicate((e) =>
              e is TProtocolError && e.type == TProtocolErrorType.SIZE_LIMIT)));
      expect(inner.bytesRequested, 4);
    });

    test('the maximum has a usable default', () {
      expect(TBinaryProtocol(_CountingTransport(Uint8List(0))).maxStringSize,
          defaultMaxStringSize);
    });

    test('a field over a lowered maximum is refused', () {
      final payload = Uint8List(64);
      final inner = _CountingTransport(concat(i32(payload.length), payload));
      final protocol = TBinaryProtocol(inner, maxStringSize: 32);

      expect(
          () => protocol.readBinary(),
          throwsA(predicate((e) =>
              e is TProtocolError && e.type == TProtocolErrorType.SIZE_LIMIT)));
    });

    test('a field within the maximum still reads', () {
      final payload = Uint8List.fromList([1, 2, 3, 4]);
      final inner = _CountingTransport(concat(i32(payload.length), payload));
      final protocol = TBinaryProtocol(inner);

      expect(protocol.readBinary(), payload);
    });
  });

  group('framed transport frame size', () {
    test('the maximum has a usable default', () {
      expect(TFramedTransport(_CountingTransport(Uint8List(0))).maxFrameSize,
          defaultMaxFrameSize);
    });

    test('rejects a maximum that is not usable', () {
      expect(
          () => TFramedTransport(_CountingTransport(Uint8List(0)),
              maxFrameSize: 0),
          throwsA(predicate((e) => e is ArgumentError)));
    });

    test('a frame declaring 4 GB is refused', () async {
      // getUint32, so the top bit is magnitude rather than a sign: the old
      // `size < 0` check was dead code. The frame is read from flush()'s
      // readable-bytes callback, so that is what drives it.
      final header = Uint8List(4)..buffer.asByteData().setUint32(0, 0xFFFFFFFF);
      final inner = _CountingTransport(header);
      final transport = TFramedTransport(inner);

      Object? caught;
      await transport.flush().catchError((e) {
        caught = e;
        return Uint8List(0);
      });

      expect(caught is TTransportError, true);
      // Four bytes of header, and nothing on account of what it declared.
      expect(inner.bytesRequested, 4);
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
