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

package org.apache.thrift.protocol;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.io.ByteArrayInputStream;
import java.util.Arrays;
import org.apache.thrift.TDeserializer;
import org.apache.thrift.TException;
import org.apache.thrift.TSerializer;
import org.apache.thrift.transport.TIOStreamTransport;
import org.apache.thrift.transport.TMemoryBuffer;
import org.junit.jupiter.api.Test;
import thrift.test.TupleProtocolTestStruct;

public class TestTTupleProtocol extends ProtocolTestBase {

  @Override
  protected boolean canBeUsedNaked() {
    return false;
  }

  @Override
  protected TProtocolFactory getFactory() {
    return new TTupleProtocol.Factory();
  }

  @Test
  public void testBitsetLengthIssue() throws Exception {
    final TupleProtocolTestStruct t1 = new TupleProtocolTestStruct();
    t1.setField1(0);
    t1.setField2(12);
    new TDeserializer(new TTupleProtocol.Factory())
        .deserialize(
            new TupleProtocolTestStruct(),
            new TSerializer(new TTupleProtocol.Factory()).serialize(t1));
  }

  /** A protocol from factory reading a stream that holds only the size a container declares. */
  private static TTupleProtocol containerHeader(TProtocolFactory factory, int size)
      throws TException {
    TMemoryBuffer buffer = new TMemoryBuffer(8);
    new TTupleProtocol(buffer).writeI32(size);
    byte[] header = Arrays.copyOf(buffer.getArray(), buffer.length());
    return (TTupleProtocol)
        factory.getProtocol(new TIOStreamTransport(new ByteArrayInputStream(header)));
  }

  private static void assertRefused(int type, TProtocolFactory factory, int size) {
    TProtocolException list =
        assertThrows(
            TProtocolException.class,
            () -> containerHeader(factory, size).readListBegin(TType.STRUCT));
    assertEquals(type, list.getType());
    TProtocolException set =
        assertThrows(
            TProtocolException.class,
            () -> containerHeader(factory, size).readSetBegin(TType.STRUCT));
    assertEquals(type, set.getType());
    TProtocolException map =
        assertThrows(
            TProtocolException.class,
            () -> containerHeader(factory, size).readMapBegin(TType.STRUCT, TType.STRUCT));
    assertEquals(type, map.getType());
  }

  @Test
  public void testDefaultContainerLengthLimit() throws Exception {
    TProtocolFactory factory = new TTupleProtocol.Factory();
    int limit = (int) TTupleProtocol.DEFAULT_CONTAINER_LENGTH_LIMIT;
    assertEquals(limit, containerHeader(factory, limit).readListBegin(TType.STRUCT).size);
    assertRefused(TProtocolException.SIZE_LIMIT, factory, limit + 1);
    assertRefused(TProtocolException.NEGATIVE_SIZE, factory, -1);
  }

  @Test
  public void testConfiguredContainerLengthLimit() throws Exception {
    assertEquals(
        10, containerHeader(new TTupleProtocol.Factory(10), 10).readListBegin(TType.STRUCT).size);
    assertRefused(TProtocolException.SIZE_LIMIT, new TTupleProtocol.Factory(10), 11);

    int overDefault = (int) TTupleProtocol.DEFAULT_CONTAINER_LENGTH_LIMIT + 1;
    assertEquals(
        overDefault,
        containerHeader(new TTupleProtocol.Factory(-1), overDefault)
            .readListBegin(TType.STRUCT)
            .size);
  }
}
