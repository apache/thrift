package org.apache.thrift;

public final class Utf8Helper {
  private Utf8Helper() {}

  public static final int getByteLength(final String s) {
    int byteLength = 0;
    int c;
    for (int i = 0; i < s.length(); i++) {
      c = s.charAt(i);
      if (c <= 0x007F) {
        byteLength++;
      } else if (c > 0x07FF) {
        byteLength+=3;
      } else {
        byteLength+=2;
      }
    }
    return byteLength;
  }

  public static byte[] encode(String s) {
    byte[] buf = new byte[getByteLength(s)];
    encode(s, buf, 0);
    return buf;
  }

  public static void encode(String s, byte[] buf, int offset) {
    int nextByte = 0;
    int c;
    for (int i = 0; i < s.length(); i++) {
      c = s.charAt(i);
      if (c <= 0x007F) {
        buf[offset + nextByte] = (byte)c;
        nextByte++;
      } else if (c > 0x07FF) {
        buf[offset + nextByte    ] = (byte)(0xE0 | c >> 12 & 0x0F);
        buf[offset + nextByte + 1] = (byte)(0x80 | c >>  6 & 0x3F);
        buf[offset + nextByte + 2] = (byte)(0x80 | c       & 0x3F);
        nextByte+=3;
      } else {
        buf[offset + nextByte    ] = (byte)(0xC0 | c >> 6 & 0x1F);
        buf[offset + nextByte + 1] = (byte)(0x80 | c      & 0x3F);
        nextByte+=2;
      }
    }
  }

  public static String decode(byte[] buf) {
    return decode(buf, 0, buf.length);
  }

  public static String decode(byte[] buf, int offset, int byteLength) {
    int charCount = 0;
    char[] chars = new char[byteLength];
    int c;
    int byteIndex = offset;
    int charIndex = 0;
    while (byteIndex < offset + byteLength) {
      c = buf[byteIndex++] & 0xFF;
      switch (c >> 4) {
        case 0:
        case 1:
        case 2:
        case 3:
        case 4:
        case 5:
        case 6:
        case 7:
          chars[charIndex++] = (char) c;
          break;
        case 12:
        case 13:
          chars[charIndex++] = (char) ((c & 0x1F) << 6 | (buf[byteIndex++] & 0x3F));
          break;
        case 14:
          chars[charIndex++] = (char) ((c & 0x0F) << 12 | (buf[byteIndex++] & 0x3F) << 6 | (buf[byteIndex++] & 0x3F) << 0);
          break;
      }
      charCount++;
    }
    return new String(chars, 0, charCount);

  }
  
}
