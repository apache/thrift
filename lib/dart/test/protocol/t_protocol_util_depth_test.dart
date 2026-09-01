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

library thrift.test.protocol.t_protocol_util_depth_test;

import 'dart:typed_data';

import 'package:test/test.dart';
import 'package:thrift/thrift.dart';

/// [TProtocolUtil.skip] is the path an unknown field takes, so how deeply it
/// nests is chosen by the peer rather than by the IDL. It threads a limit and
/// decrements it per level, which reads as bounded -- but the limit defaulted
/// to 2^53 - 1, a value no payload can reach, so the only thing that ended the
/// recursion was the VM stack running out.
///
/// Each level costs the peer two bytes.
void main() {
  /// Writes [depth] nested structs, each holding one field of type STRUCT.
  Uint8List nested(int depth) {
    final out = <int>[];
    for (var i = 0; i < depth; i++) {
      out.add(TType.STRUCT);
      out.add(0); // field id, high byte
      out.add(1); // field id, low byte
    }
    // One STOP closes the innermost struct, then one per struct on the way out.
    for (var i = 0; i <= depth; i++) {
      out.add(TType.STOP);
    }
    return Uint8List.fromList(out);
  }

  TProtocol protocolOver(Uint8List bytes) =>
      TBinaryProtocol(_ListTransport(bytes));

  group('TProtocolUtil.skip recursion depth', () {
    setUp(() {
      // maxRecursionLimit is a mutable static, so pin it rather than depend on
      // whatever ambient value another test left behind.
      TProtocolUtil.maxRecursionLimit = TProtocolUtil.defaultRecursionLimit;
    });

    test('has a reachable default limit', () {
      expect(TProtocolUtil.defaultRecursionLimit, 64);
      expect(TProtocolUtil.defaultRecursionLimit,
          TProtocol.defaultRecursionDepth);
    });

    test('refuses nesting past the limit', () {
      final protocol = protocolOver(nested(200));

      expect(() => TProtocolUtil.skip(protocol, TType.STRUCT),
          throwsA(predicate((e) =>
              e is TProtocolError && e.type == TProtocolErrorType.DEPTH_LIMIT)));
    });

    test('refuses deep nesting without exhausting the stack', () {
      // ~300 KB on the wire. Against a limit of 2^53 - 1 this recurses until
      // the VM gives up.
      final protocol = protocolOver(nested(100000));

      expect(() => TProtocolUtil.skip(protocol, TType.STRUCT),
          throwsA(predicate((e) =>
              e is TProtocolError && e.type == TProtocolErrorType.DEPTH_LIMIT)));
    });

    test('still skips nesting within the limit', () {
      final protocol = protocolOver(nested(TProtocol.defaultRecursionDepth - 2));

      expect(() => TProtocolUtil.skip(protocol, TType.STRUCT), returnsNormally);
    });
  });
}

/// Serves a fixed list of bytes. The protocol under test only reads.
class _ListTransport extends TTransport {
  final Uint8List _data;
  int _pos = 0;

  _ListTransport(this._data);

  @override
  bool get isOpen => true;

  @override
  Future open() async {}

  @override
  Future close() async {}

  @override
  int read(Uint8List buffer, int offset, int length) {
    final give = (_data.length - _pos) < length ? (_data.length - _pos) : length;
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
