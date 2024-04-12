package org.apache.thrift.transport;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertEquals;
import static org.junit.jupiter.api.Assertions.assertThrows;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import org.apache.thrift.TConfiguration;
import org.junit.jupiter.api.Test;

public class TestTByteBuffer {
  @Test
  public void testReadWrite() throws Exception {
    final TByteBuffer byteBuffer = new TByteBuffer(ByteBuffer.allocate(16));
    byteBuffer.write("Hello World".getBytes(StandardCharsets.UTF_8));
    assertEquals(
        "Hello World", new String(byteBuffer.flip().toByteArray(), StandardCharsets.UTF_8));
  }

  @Test
  public void testReuseReadWrite() throws Exception {
    final TByteBuffer byteBuffer = new TByteBuffer(ByteBuffer.allocate(16));
    byteBuffer.write("Hello World".getBytes(StandardCharsets.UTF_8));
    assertEquals(
        "Hello World", new String(byteBuffer.flip().toByteArray(), StandardCharsets.UTF_8));

    byteBuffer.clear();

    byteBuffer.write("Goodbye Horses".getBytes(StandardCharsets.UTF_8));
    assertEquals(
        "Goodbye Horses", new String(byteBuffer.flip().toByteArray(), StandardCharsets.UTF_8));
  }

  @Test
  public void testOverflow() throws Exception {
    final TByteBuffer byteBuffer = new TByteBuffer(ByteBuffer.allocate(4));
    TTransportException e =
        assertThrows(
            TTransportException.class,
            () -> byteBuffer.write("Hello World".getBytes(StandardCharsets.UTF_8)));
    assertEquals("Not enough room in output buffer", e.getMessage());
  }

  @Test
  public void testSmallTConfiguration() throws Exception {
    // Test that TByteBuffer init fail with small max message size.
    final TConfiguration configSmall =
        new TConfiguration(
            4, TConfiguration.DEFAULT_MAX_FRAME_SIZE, TConfiguration.DEFAULT_RECURSION_DEPTH);
    TTransportException e =
        assertThrows(
            TTransportException.class,
            () -> new TByteBuffer(configSmall, ByteBuffer.allocate(100)));
    assertEquals("MaxMessageSize reached", e.getMessage());
  }

  @Test
  public void testLargeTConfiguration() throws Exception {
    // Test that TByteBuffer init pass with large max message size beyond
    // TConfiguration.DEFAULT_MAX_MESSAGE_SIZE.
    int maxSize = 101 * 1024 * 1024;
    int bufferSize = (100 * 1024 + 512) * 1024;
    final TConfiguration configLarge =
        new TConfiguration(
            maxSize, TConfiguration.DEFAULT_MAX_FRAME_SIZE, TConfiguration.DEFAULT_RECURSION_DEPTH);
    assertDoesNotThrow(() -> new TByteBuffer(configLarge, ByteBuffer.allocate(bufferSize)));
  }
}
