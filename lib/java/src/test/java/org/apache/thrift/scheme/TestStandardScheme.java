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

package org.apache.thrift.scheme;

import static org.junit.jupiter.api.Assertions.assertEquals;

import org.apache.thrift.Fixtures;
import org.apache.thrift.TBase;
import org.apache.thrift.TDeserializer;
import org.apache.thrift.TException;
import org.apache.thrift.TSerializer;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.transport.TMemoryBuffer;
import org.apache.thrift.transport.TTransport;
import org.apache.thrift.transport.TTransportException;
import org.junit.jupiter.api.Test;
import thrift.test.HolyMoley;
import thrift.test.Nesting;
import thrift.test.OneOfEach;

public class TestStandardScheme {
  TSerializer serializer = new TSerializer();
  TDeserializer deserializer = new TDeserializer();

  public TestStandardScheme() throws TTransportException {}

  /**
   * This tests whether the Standard Scheme properly reads structs serialized using an older version
   * of thrift.
   */
  @Test
  public void testPersistentStructs() throws TException {
    readAndCompare(
        new OneOfEach(), Fixtures.getOneOfEach(), Fixtures.getPersistentBytesOneOfEach());
    readAndCompare(
        new HolyMoley(), Fixtures.getHolyMoley(), Fixtures.getPersistentBytesHolyMoley());
    readAndCompare(new Nesting(), Fixtures.getNesting(), Fixtures.getPersistentBytesNesting());
  }

  private void readAndCompare(TBase struct, TBase fixture, byte[] inputBytes) throws TException {
    TTransport trans = new TMemoryBuffer(0);
    trans.write(inputBytes, 0, inputBytes.length);
    TProtocol iprot = new TBinaryProtocol(trans);
    struct.read(iprot);
    assertEquals(fixture, struct);
  }
}
