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

library thrift.test.transport.t_json_protocol_test;

import 'dart:typed_data' show Uint8List;

import 'package:test/test.dart';
import 'package:thrift/thrift.dart';

void main() {
  final message = new TMessage('my message', TMessageType.ONEWAY, 123);

  TProtocol protocol;

  var sharedTests = () {
    test('Test message', () async {
      protocol.writeMessageEnd();

      await protocol.transport.flush();

      var subject = protocol.readMessageBegin();

      expect(subject.name, message.name);
      expect(subject.type, message.type);
      expect(subject.seqid, message.seqid);
    });

    test('Test struct', () async {
      var input = new TStruct();

      protocol.writeStructBegin(input);
      protocol.writeStructEnd();
      protocol.writeMessageEnd();

      await protocol.transport.flush();

      protocol.readMessageBegin();
      var output = protocol.readStructBegin();

      // name is not serialized, see C# version for reference
      expect(output, isNotNull);
    });

    test('Test field', () async {
      var input = new TField('my field', TType.MAP, 123);

      protocol.writeFieldBegin(input);
      protocol.writeFieldEnd();
      protocol.writeMessageEnd();

      await protocol.transport.flush();

      protocol.readMessageBegin();
      var output = protocol.readFieldBegin();

      // name is not serialized, see C# version for reference
      expect(output.type, input.type);
      expect(output.id, input.id);
    });

    test('Test map', () async {
      var input = new TMap(TType.STRING, TType.STRUCT, 123);

      protocol.writeMapBegin(input);
      protocol.writeMapEnd();
      protocol.writeMessageEnd();

      await protocol.transport.flush();

      protocol.readMessageBegin();
      var output = protocol.readMapBegin();

      expect(output.keyType, input.keyType);
      expect(output.valueType, input.valueType);
      expect(output.length, input.length);
    });

    test('Test list', () async {
      var input = new TList(TType.STRING, 123);

      protocol.writeListBegin(input);
      protocol.writeListEnd();
      protocol.writeMessageEnd();

      await protocol.transport.flush();

      protocol.readMessageBegin();
      var output = protocol.readListBegin();

      expect(output.elementType, input.elementType);
      expect(output.length, input.length);
    });

    test('Test set', () async {
      var input = new TSet(TType.STRING, 123);

      protocol.writeSetBegin(input);
      protocol.writeSetEnd();
      protocol.writeMessageEnd();

      await protocol.transport.flush();

      protocol.readMessageBegin();
      var output = protocol.readListBegin();

      expect(output.elementType, input.elementType);
      expect(output.length, input.length);
    });

    test('Test bool', () async {
      var input = true;

      protocol.writeBool(input);
      protocol.writeMessageEnd();

      await protocol.transport.flush();

      protocol.readMessageBegin();
      var output = protocol.readBool();

      expect(output, input);
    });

    test('Test byte', () async {
      var input = 64;

      protocol.writeByte(input);
      protocol.writeMessageEnd();

      await protocol.transport.flush();

      protocol.readMessageBegin();
      var output = protocol.readByte();

      expect(output, input);
    });

    test('Test I16', () async {
      var input = 32767;

      protocol.writeI16(input);
      protocol.writeMessageEnd();

      await protocol.transport.flush();

      protocol.readMessageBegin();
      var output = protocol.readI16();

      expect(output, input);
    });

    test('Test I32', () async {
      var input = 2147483647;

      protocol.writeI32(input);
      protocol.writeMessageEnd();

      await protocol.transport.flush();

      protocol.readMessageBegin();
      var output = protocol.readI32();

      expect(output, input);
    });

    test('Test I64', () async {
      var input = 9223372036854775807;

      protocol.writeI64(input);
      protocol.writeMessageEnd();

      await protocol.transport.flush();

      protocol.readMessageBegin();
      var output = protocol.readI64();

      expect(output, input);
    });

    test('Test double', () async {
      var input = 3.1415926;

      protocol.writeDouble(input);
      protocol.writeMessageEnd();

      await protocol.transport.flush();

      protocol.readMessageBegin();
      var output = protocol.readDouble();

      expect(output, input);
    });

    test('Test string', () async {
      var input = 'There are only two hard things in computer science: '
          'cache invalidation, naming things, and off-by-one errors.';

      protocol.writeString(input);
      protocol.writeMessageEnd();

      await protocol.transport.flush();

      protocol.readMessageBegin();
      var output = protocol.readString();

      expect(output, input);
    });

    test('Test binary', () async {
      var input = new Uint8List.fromList(new List.filled(100, 123));

      protocol.writeBinary(input);
      protocol.writeMessageEnd();

      await protocol.transport.flush();

      protocol.readMessageBegin();
      var output = protocol.readBinary();

      expect(output.length, input.length);
      expect(output.every((i) => i == 123), isTrue);
    });
  };

  group('JSON', () {
    setUp(() {
      protocol = new TJsonProtocol(new TBufferedTransport());
      protocol.writeMessageBegin(message);
    });

    group('shared tests', sharedTests);
  });

  group('binary', () {
    setUp(() {
      protocol = new TBinaryProtocol(new TBufferedTransport());
      protocol.writeMessageBegin(message);
    });

    group('shared tests', sharedTests);
  });
}
