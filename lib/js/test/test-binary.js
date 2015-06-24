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
 /* jshint -W100 */
 
/*
 * JavaScript test suite for Thrift BinaryProtocol.
 */

//////////////////////////////////
//BinaryProtocol tests

var transport = new Thrift.Transport("/dummy");

module("BinaryProtocol JavaScript tests");

  test("Boolean", function() {
    var protocol = new Thrift.BinaryProtocol(transport);
    protocol.writeBool( true );
    protocol.writeBool( false );
    equal(protocol.readBool().value, true);
    equal(protocol.readBool().value, false);
  });

  test("I16", function() {
    var value = 54321;
    var protocol = new Thrift.BinaryProtocol(transport);
    protocol.writeI16( value );
    equal(protocol.readI16().value, value);
  });

  test("I32", function() {
    var value = 22832000;
    var protocol = new Thrift.BinaryProtocol(transport);
    protocol.writeI32( value );
    equal(protocol.readI32().value, value);
  });

  test("I64", function() {
    var value = 22832000;
    var protocol = new Thrift.BinaryProtocol(transport);
    protocol.writeI64( value );
    equal(protocol.readI64().value, value);
  });

  test("Double", function() {
    var value = 20.832;
    var protocol = new Thrift.BinaryProtocol(transport);
    protocol.writeDouble( value );
    equal(protocol.readDouble().value, value);
  });

  test("String, different (human) languages", function() {
    var value1 = "zażółcić gęślą jaźń";
    var value2 = "Döner am Nürburgring";
    var value3 = "Да!";
    var protocol = new Thrift.BinaryProtocol(transport);
    protocol.writeString( value1 );
    protocol.writeString( value2 );
    protocol.writeString( value3 );
    equal(protocol.readString().value, value1);
    equal(protocol.readString().value, value2);
    equal(protocol.readString().value, value3);
  });

  test("Struct with skip", function() {
    var structName = "TestStruct";
    var fields = [
      { name: "field1", value: "Döner am Nürburgring", fieldType: Thrift.Type.STRING, fieldId: 0 },
      { name: "field2", value: 765345, fieldType: Thrift.Type.I32, fieldId: 1 },
      { name: "field3", value: false, fieldType: Thrift.Type.BOOL, fieldId: 2 } ];
    
    var protocol = new Thrift.BinaryProtocol(transport);
    protocol.writeStructBegin(structName);
    fields.forEach(function(fieldDef) {
      protocol.writeFieldBegin( fieldDef.name, fieldDef.fieldType, fieldDef.fieldId );
      switch (fieldDef.fieldType) {
        case Thrift.Type.STRING:
          protocol.writeString(fieldDef.value);
          break;
        case Thrift.Type.I32:
          protocol.writeI32(fieldDef.value);
          break;
        case Thrift.Type.BOOL:
          protocol.writeBool(fieldDef.value);
          break;
      }
      protocol.writeFieldEnd();
    });
    protocol.writeFieldStop();
    protocol.writeStructEnd();

    var readStructStart = protocol.readStructBegin();
    equal(readStructStart.fname, '');

    var firstFieldData = protocol.readFieldBegin();
    equal(firstFieldData.fname, '');
    equal(firstFieldData.ftype, fields[0].fieldType);
    equal(firstFieldData.fid, fields[0].fieldId);
    var firstFieldValue = protocol.readString().value;
    equal(firstFieldValue, fields[0].value);
    protocol.readFieldEnd();

    // skipping second field on read:
    var secondFieldData = protocol.readFieldBegin();
    equal(secondFieldData.fname, '');
    equal(secondFieldData.ftype, fields[1].fieldType);
    equal(secondFieldData.fid, fields[1].fieldId);
    var skipped = protocol.skip( secondFieldData.ftype );
    equal(skipped.value, fields[1].value);
    protocol.readFieldEnd();

    var thirdFieldData = protocol.readFieldBegin();
    equal(thirdFieldData.fname, '');
    equal(thirdFieldData.ftype, fields[2].fieldType);
    equal(thirdFieldData.fid, fields[2].fieldId);
    var thirdFieldValue = protocol.readBool().value;
    equal(thirdFieldValue, fields[2].value);
    protocol.readFieldEnd();
  });

  test("List", function() {
    var structName = "TestStruct";
    var items = [1,2,3,4,5];
    var protocol = new Thrift.BinaryProtocol(transport);
    protocol.writeStructBegin(structName);
    protocol.writeFieldBegin("some_shorts", Thrift.Type.LIST, 0);
    protocol.writeListBegin(Thrift.Type.I16, items.length);
    items.forEach(function(i) {
      protocol.writeI16(i);
    });
    protocol.writeListEnd();
    protocol.writeFieldEnd();
    protocol.writeFieldStop();
    protocol.writeStructEnd();

    var readStructStart = protocol.readStructBegin();
    equal(readStructStart.fname, '');

    var firstFieldData = protocol.readFieldBegin();
    equal(firstFieldData.fname, '');
    equal(firstFieldData.ftype, Thrift.Type.LIST);
    equal(firstFieldData.fid, 0);

    var listData = protocol.readListBegin();
    equal(listData.size, items.length);
    equal(listData.etype, Thrift.Type.I16);

    var readItems = [];
    for ( var i=0; i<listData.size; i++ ) {
      readItems.push( protocol.readI16().value );
    }
    deepEqual(items, readItems);

    protocol.readListEnd();
    protocol.readFieldEnd();
  });

  test("Message", function() {
    
    var messageType = Thrift.MessageType.CALL;
    var messageName = "warmUpLap";
    var messageSeqId = 0; // always 0

    var protocol = new Thrift.BinaryProtocol(transport);
    protocol.writeMessageBegin( messageName, messageType, messageSeqId );
    protocol.writeMessageEnd();

    var messageBeginData = protocol.readMessageBegin();

    equal(messageBeginData.fname, messageName);
    equal(messageBeginData.mtype, messageType);
    equal(messageBeginData.rseqid, messageSeqId);
  });


