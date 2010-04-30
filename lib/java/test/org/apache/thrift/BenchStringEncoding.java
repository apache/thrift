package org.apache.thrift;

import java.io.UnsupportedEncodingException;

public class BenchStringEncoding {
  private static final String STRING = "a moderately long (but not overly long) string";
  private static final int HOW_MANY = 100000;
  private static final byte[] BYTES;
  static {
    try {
      BYTES = STRING.getBytes("UTF-8");
    } catch (UnsupportedEncodingException e) {
      throw new RuntimeException(e);
    }
  }

  public static void main(String[] args) throws UnsupportedEncodingException {
    for (int trial = 0; trial < 5; trial++) {
      benchGetBytes();
      benchFromBytes();
      benchEncode();
      benchDecode();
    }
  }

  private static void benchDecode() {
    char[] charBuf = new char[256];
    long start = System.currentTimeMillis();
    for (int i = 0; i < HOW_MANY; i++) {
      Utf8Helper.decode(BYTES, 0, BYTES.length, charBuf);
    }
    long end = System.currentTimeMillis();
    System.out.println("decode: decode: " + (end-start) + "ms");
  }

  private static void benchFromBytes() {
    long start = System.currentTimeMillis();
    for (int i = 0; i < HOW_MANY; i++) {
      try {
        new String(BYTES, "UTF-8");
      } catch (UnsupportedEncodingException e) {
        throw new RuntimeException(e);
      }
    }
    long end = System.currentTimeMillis();
    System.out.println("decode: fromBytes: " + (end-start) + "ms");
  }

  private static void benchEncode() {
    long start = System.currentTimeMillis();
    byte[] outbuf = new byte[256];
    for (int i = 0; i < HOW_MANY; i++) {
      Utf8Helper.encode(STRING, outbuf, 0);
    }
    long end = System.currentTimeMillis();
    System.out.println("encode: directEncode: " + (end-start) + "ms");
  }

  private static void benchGetBytes() throws UnsupportedEncodingException {
    long start = System.currentTimeMillis();
    for (int i = 0; i < HOW_MANY; i++) {
      STRING.getBytes("UTF-8");
    }
    long end = System.currentTimeMillis();
    System.out.println("encode: getBytes(UTF-8): " + (end-start) + "ms");
  }
}
