package org.apache.thrift.transport;

import junit.framework.TestCase;
import java.nio.charset.StandardCharsets;
import org.apache.thrift.TException;

import java.nio.ByteBuffer;

public class TestTByteBuffer extends TestCase {
  public void testReadWrite() throws Exception {
    final TByteBuffer byteBuffer = new TByteBuffer(ByteBuffer.allocate(16));
    byteBuffer.write("Hello World".getBytes(StandardCharsets.UTF_8));
    assertEquals("Hello World", new String(byteBuffer.flip().toByteArray(), StandardCharsets.UTF_8));
  }

  public void testReuseReadWrite() throws Exception {
    final TByteBuffer byteBuffer = new TByteBuffer(ByteBuffer.allocate(16));
    byteBuffer.write("Hello World".getBytes(StandardCharsets.UTF_8));
    assertEquals("Hello World", new String(byteBuffer.flip().toByteArray(), StandardCharsets.UTF_8));

    byteBuffer.clear();

    byteBuffer.write("Goodbye Horses".getBytes(StandardCharsets.UTF_8));
    assertEquals("Goodbye Horses", new String(byteBuffer.flip().toByteArray(), StandardCharsets.UTF_8));
  }

  public void testOverflow() throws Exception {
    final TByteBuffer byteBuffer = new TByteBuffer(ByteBuffer.allocate(4));
    try {
      byteBuffer.write("Hello World".getBytes(StandardCharsets.UTF_8));
      fail("Expected write operation to fail with TTransportException");
    } catch (TTransportException e) {
      assertEquals("Not enough room in output buffer", e.getMessage());
    }
  }
}
