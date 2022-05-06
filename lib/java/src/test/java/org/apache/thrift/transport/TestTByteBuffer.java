package org.apache.thrift.transport;

import org.junit.jupiter.api.Test;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;

import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

public class TestTByteBuffer  {
  @Test
  public void testReadWrite() throws Exception {
    final TByteBuffer byteBuffer = new TByteBuffer(ByteBuffer.allocate(16));
    byteBuffer.write("Hello World".getBytes(StandardCharsets.UTF_8));
    assertEquals("Hello World", new String(byteBuffer.flip().toByteArray(), StandardCharsets.UTF_8));
  }

  @Test
  public void testReuseReadWrite() throws Exception {
    final TByteBuffer byteBuffer = new TByteBuffer(ByteBuffer.allocate(16));
    byteBuffer.write("Hello World".getBytes(StandardCharsets.UTF_8));
    assertEquals("Hello World", new String(byteBuffer.flip().toByteArray(), StandardCharsets.UTF_8));

    byteBuffer.clear();

    byteBuffer.write("Goodbye Horses".getBytes(StandardCharsets.UTF_8));
    assertEquals("Goodbye Horses", new String(byteBuffer.flip().toByteArray(), StandardCharsets.UTF_8));
  }

  @Test
  public void testOverflow() throws Exception {
    final TByteBuffer byteBuffer = new TByteBuffer(ByteBuffer.allocate(4));
    TTransportException e = assertThrows(TTransportException.class, ()-> byteBuffer.write("Hello World".getBytes(StandardCharsets.UTF_8)));
    assertEquals("Not enough room in output buffer", e.getMessage());
  }
}
