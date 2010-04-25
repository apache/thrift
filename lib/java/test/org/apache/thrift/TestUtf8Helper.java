package org.apache.thrift;

import java.io.UnsupportedEncodingException;
import java.util.Arrays;

import junit.framework.TestCase;

public class TestUtf8Helper extends TestCase {
  private static final String NON_UNICODE_STRING = "here's some text";

  private static final byte[] kUnicodeBytes = {
    (byte)0xd3, (byte)0x80, (byte)0xe2, (byte)0x85, (byte)0xae, (byte)0xce,
    (byte)0x9d, (byte)0x20, (byte)0xd0, (byte)0x9d, (byte)0xce, (byte)0xbf,
    (byte)0xe2, (byte)0x85, (byte)0xbf, (byte)0xd0, (byte)0xbe, (byte)0xc9,
    (byte)0xa1, (byte)0xd0, (byte)0xb3, (byte)0xd0, (byte)0xb0, (byte)0xcf,
    (byte)0x81, (byte)0xe2, (byte)0x84, (byte)0x8e, (byte)0x20, (byte)0xce,
    (byte)0x91, (byte)0x74, (byte)0x74, (byte)0xce, (byte)0xb1, (byte)0xe2,
    (byte)0x85, (byte)0xbd, (byte)0xce, (byte)0xba, (byte)0x83, (byte)0xe2,
    (byte)0x80, (byte)0xbc
  };

  private static final String UNICODE_STRING = "abc\u5639\u563b";
  private static final byte[] UNICODE_STRING_BYTES;

  private static final String UNICODE_STRING_2;
  private static final byte[] UNICODE_STRING_BYTES_2;

  static {
    try {
      UNICODE_STRING_BYTES = UNICODE_STRING.getBytes("UTF-8");
      UNICODE_STRING_2 = new String(kUnicodeBytes, "UTF-8");
      UNICODE_STRING_BYTES_2 = UNICODE_STRING_2.getBytes("UTF-8");
    } catch (UnsupportedEncodingException e) {
      throw new RuntimeException(e);
    }
  }


  public void testEncode() throws Exception {
    byte[] bytes = NON_UNICODE_STRING.getBytes("UTF-8");
    byte[] otherBytes = Utf8Helper.encode(NON_UNICODE_STRING);
    assertTrue(Arrays.equals(bytes, otherBytes));

    otherBytes = Utf8Helper.encode(UNICODE_STRING);
    assertTrue(Arrays.equals(UNICODE_STRING_BYTES, otherBytes));

    otherBytes = Utf8Helper.encode(UNICODE_STRING_2);
    assertTrue(Arrays.equals(UNICODE_STRING_BYTES_2, otherBytes));
  }

  public void testDecode() throws Exception {
    byte[] bytes = NON_UNICODE_STRING.getBytes("UTF-8");
    assertEquals(NON_UNICODE_STRING, Utf8Helper.decode(bytes));

    assertEquals(UNICODE_STRING, Utf8Helper.decode(UNICODE_STRING_BYTES));
    assertEquals(UNICODE_STRING_2, Utf8Helper.decode(UNICODE_STRING_BYTES_2));
  }
}
