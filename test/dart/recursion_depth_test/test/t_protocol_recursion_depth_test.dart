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
// KIND, either express or implied.  See the License for the
// specific language governing permissions and limitations
// under the License.

// Exercises the recursion-depth limit through the *generated* struct read/write
// path (TBase.write / TBase.read) -- the real code path that deeply nested
// input exercises -- rather than calling TProtocol.incrementRecursionDepth /
// decrementRecursionDepth in isolation.
//
// The recursive IDL types come from test/Recursive.thrift, generated into the
// `Recursive` package: CoRec <-> CoRec2 form a mutually recursive chain and
// RecTree is a wide tree of nested structs.
//
// Dart hard-codes the limit at TProtocol's _defaultRecursionDepth (64), so the
// boundary is: a chain of 64 structs round-trips, 65 is rejected with
// DEPTH_LIMIT.

library thrift.test.recursion_depth_test;

import 'dart:typed_data' show Uint8List;

import 'package:test/test.dart';
import 'package:thrift/thrift.dart';
import 'package:Recursive/Recursive.dart';

// The hard recursion limit baked into TProtocol (_defaultRecursionDepth).
const int kRecursionLimit = 64;

// Build a CoRec/CoRec2 chain that is exactly [depth] structs deep.
CoRec? makeNestedRecs(int depth) =>
    depth <= 0 ? null : (CoRec()..other = makeNestedCoRec2(depth - 1));

CoRec2? makeNestedCoRec2(int depth) =>
    depth <= 0 ? null : (CoRec2()..other = makeNestedRecs(depth - 1));

// Build a CoError/CoError2 exception chain that is exactly [depth] structs deep.
// Exceptions are read/written through the same generated path as structs, so
// the same bound applies. The chain terminates in a null field, which the
// generated write() skips, so [depth] structs nest exactly [depth] levels.
CoError? makeNestedError(int depth) =>
    depth <= 0 ? null : (CoError()..other = makeNestedError2(depth - 1));

CoError2? makeNestedError2(int depth) =>
    depth <= 0 ? null : (CoError2()..other = makeNestedError(depth - 1));

// Serialize via the generated write() over a fresh protocol of the given kind.
Uint8List writeWith(TBase obj, TProtocolFactory factory) =>
    TSerializer(protocolFactory: factory).write(obj);

// Deserialize via the generated read() over a fresh protocol of the given kind.
void readWith(TBase into, Uint8List bytes, TProtocolFactory factory) =>
    TDeserializer(protocolFactory: factory).read(into, bytes);

// Craft a [depth]-deep nested-struct payload with raw protocol primitives,
// bypassing the recursion counter (which lives in the generated write()). This
// is the only way to obtain an over-limit payload, since a normal write() of
// such a chain would itself be rejected at the limit.
Uint8List craftDeepChain(TProtocolFactory factory, int depth) {
  final transport = TBufferedTransport();
  final protocol = factory.getProtocol(transport);

  void emit(int d) {
    protocol.writeStructBegin(TStruct('CoRec'));
    if (d > 1) {
      protocol.writeFieldBegin(TField('other', TType.STRUCT, 1));
      emit(d - 1);
      protocol.writeFieldEnd();
    }
    protocol.writeFieldStop();
    protocol.writeStructEnd();
  }

  emit(depth);
  return transport.consumeWriteBuffer();
}

// Craft a [depth]-deep nested CoError payload with raw protocol primitives.
// Uses the real recursive field (id 1, type STRUCT) so the reader recurses
// through the guarded generated read(), not skip().
Uint8List craftDeepErrorChain(TProtocolFactory factory, int depth) {
  final transport = TBufferedTransport();
  final protocol = factory.getProtocol(transport);

  void emit(int d) {
    protocol.writeStructBegin(TStruct('CoError'));
    if (d > 1) {
      protocol.writeFieldBegin(TField('other', TType.STRUCT, 1));
      emit(d - 1);
      protocol.writeFieldEnd();
    }
    protocol.writeFieldStop();
    protocol.writeStructEnd();
  }

  emit(depth);
  return transport.consumeWriteBuffer();
}

final Matcher throwsDepthLimit = throwsA(predicate(
    (e) => e is TProtocolError && e.type == TProtocolErrorType.DEPTH_LIMIT));

void main() {
  final factories = <String, TProtocolFactory>{
    'binary': TBinaryProtocolFactory(),
    'compact': TCompactProtocolFactory(),
    'json': TJsonProtocolFactory(),
  };

  factories.forEach((name, factory) {
    group('$name protocol', () {
      // A chain one level below the limit must round-trip cleanly.
      test('round-trips a chain one below the limit', () {
        final data = makeNestedRecs(kRecursionLimit - 1)!;
        final bytes = writeWith(data, factory);
        readWith(CoRec(), bytes, factory);
      });

      // A chain exactly at the limit must still round-trip (off-by-one guard).
      test('round-trips a chain exactly at the limit', () {
        final data = makeNestedRecs(kRecursionLimit)!;
        final bytes = writeWith(data, factory);
        readWith(CoRec(), bytes, factory);
      });

      // Writing a chain one level over the limit must be rejected.
      test('rejects writing a chain above the limit', () {
        final data = makeNestedRecs(kRecursionLimit + 1)!;
        expect(() => writeWith(data, factory), throwsDepthLimit);
      });

      // Reading a too-deep payload must be rejected.
      test('rejects reading a payload above the limit', () {
        final bytes = craftDeepChain(factory, kRecursionLimit + 1);
        expect(() => readWith(CoRec(), bytes, factory), throwsDepthLimit);
      });

      // Decrement regression guard: a wide (shallow) tree whose total number of
      // struct-begins far exceeds the limit must still round-trip. This only
      // holds if decrementRecursionDepth() unwinds each sibling back to depth 1.
      test('round-trips a wide (shallow) structure', () {
        final tree = RecTree()
          ..item = 0
          ..children = <RecTree>[];
        for (var i = 0; i < (kRecursionLimit * 3); i++) {
          tree.children!.add(RecTree()
            ..item = i
            ..children = <RecTree>[]);
        }
        final bytes = writeWith(tree, factory);
        readWith(RecTree(), bytes, factory);
      });

      // A cyclic object graph would recurse forever without the limit; it must
      // instead fail with DEPTH_LIMIT.
      test('rejects a cyclic object graph', () {
        final data = makeNestedRecs(2)!; // CoRec -> CoRec2 -> null
        data.other!.other = data; // close the loop: CoRec2.other -> CoRec
        expect(() => writeWith(data, factory), throwsDepthLimit);
      });

      // The same bound must apply to recursive exceptions.
      test('round-trips an exception exactly at the limit', () {
        final data = makeNestedError(kRecursionLimit)!;
        final bytes = writeWith(data, factory);
        readWith(CoError(), bytes, factory);
      });

      test('rejects writing an exception above the limit', () {
        final data = makeNestedError(kRecursionLimit + 1)!;
        expect(() => writeWith(data, factory), throwsDepthLimit);
      });

      test('rejects reading an exception payload above the limit', () {
        final bytes = craftDeepErrorChain(factory, kRecursionLimit + 1);
        expect(() => readWith(CoError(), bytes, factory), throwsDepthLimit);
      });
    });
  });
}
