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

package org.apache.thrift.transport.sasl;

import java.io.IOException;
import java.nio.ByteBuffer;

import org.apache.thrift.EncodingUtils;
import org.apache.thrift.transport.TNonblockingTransport;
import org.junit.Assert;
import org.junit.Test;
import org.mockito.Mockito;
import org.mockito.invocation.InvocationOnMock;
import org.mockito.stubbing.Answer;

import static org.apache.thrift.transport.sasl.DataFrameHeaderReader.PAYLOAD_LENGTH_BYTES;

public class TestDataFrameWriter {

  private static final byte[] BYTES = new byte[]{0x32, 0x2A, (byte) 0xE1, 0x18, (byte) 0x90, 0x75};

  @Test
  public void testProvideEntireByteArrayAsPayload() {
    DataFrameWriter frameWriter = new DataFrameWriter();
    frameWriter.withOnlyPayload(BYTES);
    byte[] expectedBytes = new byte[BYTES.length + PAYLOAD_LENGTH_BYTES];
    EncodingUtils.encodeBigEndian(BYTES.length, expectedBytes);
    System.arraycopy(BYTES, 0, expectedBytes, PAYLOAD_LENGTH_BYTES, BYTES.length);
    Assert.assertEquals(ByteBuffer.wrap(expectedBytes), frameWriter.frameBytes);
  }

  @Test
  public void testProvideByteArrayPortionAsPayload() {
    DataFrameWriter frameWriter = new DataFrameWriter();
    int portionOffset = 2;
    int portionLength = 3;
    frameWriter.withOnlyPayload(BYTES, portionOffset, portionLength);
    byte[] expectedBytes = new byte[portionLength + PAYLOAD_LENGTH_BYTES];
    EncodingUtils.encodeBigEndian(portionLength, expectedBytes);
    System.arraycopy(BYTES, portionOffset, expectedBytes, PAYLOAD_LENGTH_BYTES, portionLength);
    Assert.assertEquals(ByteBuffer.wrap(expectedBytes), frameWriter.frameBytes);
  }

  @Test(expected = IllegalArgumentException.class)
  public void testProvideHeaderAndPayload() {
    DataFrameWriter frameWriter = new DataFrameWriter();
    frameWriter.withHeaderAndPayload(new byte[1], new byte[1]);
  }

  @Test(expected = IllegalStateException.class)
  public void testProvidePayloadToIncompleteFrame() {
    DataFrameWriter frameWriter = new DataFrameWriter();
    frameWriter.withOnlyPayload(BYTES);
    frameWriter.withOnlyPayload(new byte[1]);
  }

  @Test
  public void testWrite() throws Exception {
    DataFrameWriter frameWriter = new DataFrameWriter();
    frameWriter.withOnlyPayload(BYTES);
    // Slow socket which writes one byte per call.
    TNonblockingTransport transport = Mockito.mock(TNonblockingTransport.class);
    SlowWriting slowWriting = new SlowWriting();
    Mockito.when(transport.write(frameWriter.frameBytes)).thenAnswer(slowWriting);
    frameWriter.write(transport);
    while (slowWriting.written < frameWriter.frameBytes.limit()) {
      Assert.assertFalse("Frame writer should not be complete", frameWriter.isComplete());
      frameWriter.write(transport);
    }
    Assert.assertTrue("Frame writer should be complete", frameWriter.isComplete());
  }

  private static class SlowWriting implements Answer<Integer> {
    int written = 0;

    @Override
    public Integer answer(InvocationOnMock invocation) throws Throwable {
      ByteBuffer bytes = (ByteBuffer) invocation.getArguments()[0];
      bytes.get();
      written ++;
      return 1;
    }
  }
}
