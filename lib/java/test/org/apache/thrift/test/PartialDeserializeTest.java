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


package org.apache.thrift.test;

import org.apache.thrift.TBase;
import org.apache.thrift.TDeserializer;
import org.apache.thrift.TException;
import org.apache.thrift.TSerializer;
import org.apache.thrift.TFieldIdEnum;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TCompactProtocol;
import org.apache.thrift.protocol.TJSONProtocol;
import org.apache.thrift.protocol.TProtocolFactory;

import thrift.test.Backwards;
import thrift.test.OneOfEach;
import thrift.test.PrimitiveThenStruct;
import thrift.test.StructWithAUnion;
import thrift.test.TestUnion;

public class PartialDeserializeTest {

  private static final TProtocolFactory[] PROTOCOLS = new TProtocolFactory[] {
    new TBinaryProtocol.Factory(), 
    new TCompactProtocol.Factory(), 
    new TJSONProtocol.Factory()
  };

  public static void main(String[] args) throws TException {
    //Root:StructWithAUnion
    //  1:Union
    //    1.3:OneOfEach
    OneOfEach Level3OneOfEach = Fixtures.oneOfEach;
    TestUnion Level2TestUnion = new TestUnion(TestUnion._Fields.STRUCT_FIELD, Level3OneOfEach);
    StructWithAUnion Level1SWU = new StructWithAUnion(Level2TestUnion);

    Backwards bw = new Backwards(2, 1);
    PrimitiveThenStruct pts = new PrimitiveThenStruct(12345, 67890, bw);

    for (TProtocolFactory factory : PROTOCOLS) {
      //Full deserialization test
      testPartialDeserialize(factory, Level1SWU, new StructWithAUnion(), Level1SWU);

      //Level 2 test
      testPartialDeserialize(factory, Level1SWU, new TestUnion(), Level2TestUnion, StructWithAUnion._Fields.TEST_UNION);

      //Level 3 on 3rd field test
      testPartialDeserialize(factory, Level1SWU, new OneOfEach(), Level3OneOfEach, StructWithAUnion._Fields.TEST_UNION, TestUnion._Fields.STRUCT_FIELD);

      //Test early termination when traversed path Field.id exceeds the one being searched for
      testPartialDeserialize(factory, Level1SWU, new OneOfEach(), new OneOfEach(), StructWithAUnion._Fields.TEST_UNION, TestUnion._Fields.I32_FIELD);

      //Test that readStructBegin isn't called on primitive
      testPartialDeserialize(factory, pts, new Backwards(), bw, PrimitiveThenStruct._Fields.BW);
    }
  }

  public static void testPartialDeserialize(TProtocolFactory protocolFactory, TBase input, TBase output, TBase expected, TFieldIdEnum ... fieldIdPath) throws TException {
    byte[] record = new TSerializer(protocolFactory).serialize(input);
    new TDeserializer(protocolFactory).partialDeserialize(output, record, fieldIdPath);
    if(!output.equals(expected))
      throw new RuntimeException("with " + protocolFactory.toString() + ", expected " + expected + " but got " + output);
  }
}

