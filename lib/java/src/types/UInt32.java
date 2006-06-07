package com.facebook.thrift.types;

import java.io.ByteArrayOutputStream;

/**
 * Bit-twiddling bullshit because java doesn't have unsigned types.
 * Also, the JVM is big-endian, but Pillar is written on the little
 * endian architecture, and it doesn't translated numbers to network
 * byte order (also big-endian) when it transmits them.
 *
 * So, a UInt32 received by pillar will come over the net in little
 * endian byte order, which means we have to reverse it for Java.
 *
 * @author Mark Slee (mcslee@facebook.com)
 *
 * See: http://darksleep.com/player/JavaAndUnsignedTypes.html
 *
 */
public class UInt32 {
  private long longValue_ = 0;
  private byte[] data_ = new byte[4];
  
  public long get() {
    return longValue_;
  }

  public int toInt() {
    return (int) longValue_;
  }

  public byte[] data() {
    return data_;
  }

  public UInt32() {
    for (int i = 0; i < 4; i++) data_[i] = (byte) 0;
  }

  public UInt32(byte[] buf, int offset) {
    read(buf, offset);
  }

  public UInt32(long value) {
    set(value);
  }

  public UInt32(int value) {
    this((long)value);
  }

  public void set(long value) {
    if (value < 0) {
      throw new RuntimeException("Cannot assign negative value to UInt32.");
    }
    longValue_ = value;
    data_[0] = (byte)((longValue_ & 0xFF000000L) >> 24);
    data_[1] = (byte)((longValue_ & 0x00FF0000L) >> 16);
    data_[2] = (byte)((longValue_ & 0x0000FF00L) >> 8);
    data_[3] = (byte)((longValue_ & 0x000000FFL));
  }

  public void read(byte[] buf, int offset) {
    for (int i = 0; i < 4; i++) {
      data_[i] = buf[offset+i];
    }

    int[] bytes = new int[4];
    bytes[0] = (0x000000FF & ((int)data_[0]));
    bytes[1] = (0x000000FF & ((int)data_[1]));
    bytes[2] = (0x000000FF & ((int)data_[2]));
    bytes[3] = (0x000000FF & ((int)data_[3]));
    longValue_ = ((long) ((bytes[0] << 24) | (bytes[1] << 16) |
                          (bytes[2] << 8) | (bytes[3]))) & 0xFFFFFFFFL;
  }

  public String toString() {
    return String.valueOf(longValue_);
  }

  public int hashCode() {
    return toInt();
  }

  public boolean equals(Object that) {
    return ((that instanceof UInt32) &&
            (this.longValue_ == ((UInt32) that).longValue_));
  }

}
