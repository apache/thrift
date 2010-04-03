package org.apache.thrift.transport;

import java.util.Arrays;

import junit.framework.TestCase;

public class TestTMemoryInputTransport extends TestCase {
  public void testFresh() throws Exception {
    byte[] input_buf = new byte[]{1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    TMemoryInputTransport trans = new TMemoryInputTransport(input_buf);
    assertEquals(0, trans.getBufferPosition());
    assertEquals(input_buf, trans.getBuffer());
    assertEquals(10, trans.getBytesRemainingInBuffer());

    byte[] buf1 = new byte[4];
    trans.readAll(buf1, 0, 4);
    assertTrue(Arrays.equals(new byte[]{1, 2, 3, 4}, buf1));
    assertEquals(4, trans.getBufferPosition());
    assertEquals(6, trans.getBytesRemainingInBuffer());

    trans.consumeBuffer(2);

    assertEquals(6, trans.getBufferPosition());
    assertEquals(4, trans.getBytesRemainingInBuffer());

    trans.readAll(buf1, 0, 4);
    assertTrue(Arrays.equals(new byte[]{7, 8, 9, 10}, buf1));
    assertEquals(10, trans.getBufferPosition());
    assertEquals(0, trans.getBytesRemainingInBuffer());
  }

  public void testReused() throws Exception {
    byte[] input_buf = new byte[]{1, 2, 3, 4, 5, 6, 7, 8, 9, 10};
    TMemoryInputTransport trans = new TMemoryInputTransport(input_buf);
    assertEquals(0, trans.getBufferPosition());
    assertEquals(input_buf, trans.getBuffer());
    assertEquals(10, trans.getBytesRemainingInBuffer());

    byte[] new_buf = new byte[]{10, 9, 8};
    trans.reset(new_buf);
    assertEquals(0, trans.getBufferPosition());
    assertEquals(new_buf, trans.getBuffer());
    assertEquals(3, trans.getBytesRemainingInBuffer());
  }
}
