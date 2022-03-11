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

package org.apache.thrift.transport;

import org.apache.thrift.TByteArrayOutputStream;
import org.junit.Assert;
import org.junit.Test;

import java.nio.ByteBuffer;
import java.util.Random;

public class TestTMemoryTransport {

  @Test
  public void testReadBatches() throws TTransportException {
    byte[] inputBytes = {0x10, 0x7A, (byte) 0xBF, (byte) 0xFE, 0x53, (byte) 0x82, (byte) 0xFF};
    TMemoryTransport transport = new TMemoryTransport(inputBytes);
    byte[] read = new byte[inputBytes.length];
    int firstBatch = new Random().nextInt(inputBytes.length);
    int secondBatch = inputBytes.length - firstBatch;
    transport.read(read, 0, firstBatch);
    transport.read(read, firstBatch, secondBatch);
    boolean equal = true;
    for (int i = 0; i < inputBytes.length; i++) {
      equal = equal && inputBytes[i] == read[i];
    }
    Assert.assertEquals(ByteBuffer.wrap(inputBytes), ByteBuffer.wrap(read));
  }

  @Test (expected = TTransportException.class)
  public void testReadMoreThanRemaining() throws TTransportException {
    TMemoryTransport transport = new TMemoryTransport(new byte[] {0x00, 0x32});
    byte[] read = new byte[3];
    transport.read(read, 0, 3);
  }

  @Test
  public void testWrite() throws TTransportException {
    TMemoryTransport transport = new TMemoryTransport(new byte[0]);
    byte[] output1 = {0x72, 0x56, 0x29, (byte) 0xAF, (byte) 0x9B};
    transport.write(output1);
    byte[] output2 = {(byte) 0x83, 0x10, 0x00};
    transport.write(output2, 0, 2);
    byte[] expected = {0x72, 0x56, 0x29, (byte) 0xAF, (byte) 0x9B, (byte) 0x83, 0x10};
    TByteArrayOutputStream outputByteArray = transport.getOutput();
    Assert.assertEquals(ByteBuffer.wrap(expected), ByteBuffer.wrap(outputByteArray.get(), 0, outputByteArray.len()));
  }
}
