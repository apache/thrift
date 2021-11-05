/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

library thrift.test.serializer.serializer_test;

import 'package:test/test.dart';
import 'package:thrift/thrift.dart';
import 'serializer_test_data.dart';

void main() {
  var serializer = () {
    TDeserializer deserializer;
    TSerializer serializer;
    TestTObject testTObject;

    setUp(() {
      serializer = TSerializer();
      deserializer = TDeserializer();

      testTObject = TestTObject();
      testTObject.b = true;
      testTObject.s = "TEST";
      testTObject.d = 15.25;
      testTObject.i = 10;

      var testList = List<String>();
      testList.add("TEST 1");
      testList.add("TEST 2");

      testTObject.l = testList;
    });

    assertNewObjectEqualsTObject(TestTObject newObject) {
      expect(newObject.l, equals(testTObject.l));
      expect(newObject.b, equals(testTObject.b));
      expect(newObject.i, equals(testTObject.i));
      expect(newObject.d, equals(testTObject.d));
      expect(newObject.s, equals(testTObject.s));
    }

    runWriteStringTest() {
      var s = serializer.writeString(testTObject);

      var newObject = TestTObject();
      deserializer.readString(newObject, s);

      assertNewObjectEqualsTObject(newObject);
    }

    runWriteTest() {
      var s = serializer.write(testTObject);

      var newObject = TestTObject();
      deserializer.read(newObject, s);

      assertNewObjectEqualsTObject(newObject);
    }

    test('JSON Protocol String', () {
      serializer.protocol = TJsonProtocol(serializer.transport);
      deserializer.protocol = TJsonProtocol(deserializer.transport);

      runWriteStringTest();
    });

    test('JSON Protocol', () {
      serializer.protocol = TJsonProtocol(serializer.transport);
      deserializer.protocol = TJsonProtocol(deserializer.transport);

      runWriteTest();
    });

    test('Binary Protocol String', () {
      serializer.protocol = TBinaryProtocol(serializer.transport);
      deserializer.protocol = TBinaryProtocol(deserializer.transport);

      runWriteStringTest();
    });

    test('Binary Protocol', () {
      serializer.protocol = TBinaryProtocol(serializer.transport);
      deserializer.protocol = TBinaryProtocol(deserializer.transport);

      runWriteTest();
    });

    test('Compact Protocol String', () {
      serializer.protocol = TCompactProtocol(serializer.transport);
      deserializer.protocol = TCompactProtocol(deserializer.transport);

      runWriteStringTest();
    });

    test('Compact Protocol', () {
      serializer.protocol = TCompactProtocol(serializer.transport);
      deserializer.protocol = TCompactProtocol(deserializer.transport);

      runWriteTest();
    });
  };

  group('Serializer', serializer);
}
