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

package org.apache.thrift.partial;

import static org.junit.Assert.*;

import org.apache.thrift.TDeserializer;
import org.apache.thrift.TException;
import org.apache.thrift.partial.TestStruct;
import org.apache.thrift.partial.ThriftField;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TCompactProtocol;
import org.junit.Test;

import java.io.IOException;
import java.util.Arrays;
import java.util.Collections;
import java.util.List;

public class PartialThriftComparerTest {

  private ThriftSerDe serde;
  private PartialThriftTestData testData = new PartialThriftTestData();

  public PartialThriftComparerTest() throws TException {
    this.serde = new ThriftSerDe();
  }

  @Test
  public void testCompareSimple() throws TException, IOException {
    TestStruct ts1 = testData.createTestStruct(1, 1);
    assertTrue(ts1.isSetI16Field());
    assertTrue(ts1.isSetI32Field());

    byte[] bytesBinary = serde.serializeBinary(ts1);
    byte[] bytesCompact = serde.serializeCompact(ts1);

    List<String> fieldNames = Arrays.asList("i32Field");
    TDeserializer partialBinaryDeser =
        new TDeserializer(TestStruct.class, fieldNames, new TBinaryProtocol.Factory());
    TDeserializer partialCompactDeser =
        new TDeserializer(TestStruct.class, fieldNames, new TCompactProtocol.Factory());

    ThriftMetadata.ThriftStruct metadata = partialBinaryDeser.getMetadata();
    PartialThriftComparer comparer = new PartialThriftComparer(metadata);

    StringBuilder sb = new StringBuilder();
    TestStruct ts2 = (TestStruct) partialBinaryDeser.partialDeserializeObject(bytesBinary);
    if (!comparer.areEqual(ts1, ts2, sb)) {
      fail(sb.toString());
    }

    ts2 = (TestStruct) partialCompactDeser.partialDeserializeObject(bytesCompact);
    if (!comparer.areEqual(ts1, ts2, sb)) {
      fail(sb.toString());
    }
  }
}
