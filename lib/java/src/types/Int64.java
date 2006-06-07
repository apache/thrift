package com.facebook.thrift.types;

import java.io.ByteArrayOutputStream;

/**
 * 64-bit integers, just a wrapper around a long, and a byte-order
 * reverser.
 *
 * @author Mark Slee (mcslee@facebook.com)
 *
 */
public class Int64 {
  private long longValue_ = 0L;
  private byte[] data_ = new byte[8];

  public long get() {
    return longValue_;
  }

  public byte[] data() {
    return data_;
  }

  public Int64() {
    for (int i = 0; i < 8; i++) data_[i] = (byte) 0;
  }

  public Int64(long value) {
    set(value);
  }

  public Int64(byte[] buf, int offset) {
    read(buf, offset);
  }


  /**
   * Yes, this could be done in a loop, but written out this way makes
   * it easier to see how the bytes are actually generated.
   */
  public void set(long value) {
    longValue_ = value;
    data_[0] = (byte)((longValue_ & 0xFF00000000000000L) >> 56);
    data_[1] = (byte)((longValue_ & 0x00FF000000000000L) >> 48);
    data_[2] = (byte)((longValue_ & 0x0000FF0000000000L) >> 40);
    data_[3] = (byte)((longValue_ & 0x000000FF00000000L) >> 32);
    data_[4] = (byte)((longValue_ & 0x00000000FF000000L) >> 24);
    data_[5] = (byte)((longValue_ & 0x0000000000FF0000L) >> 16);
    data_[6] = (byte)((longValue_ & 0x000000000000FF00L) >> 8);
    data_[7] = (byte)((longValue_ & 0x00000000000000FFL));
  }

  /**
   * Reverse byte order to calculate the value.
   */
  public void read(byte[] buf, int offset) {
    for (int i = 0; i < 8; i++) {
      data_[i] = buf[offset+i];
    }

    long[] bytes = new long[8];
    bytes[0] = (0xFF & ((long)data_[0]));
    bytes[1] = (0xFF & ((long)data_[1]));
    bytes[2] = (0xFF & ((long)data_[2]));
    bytes[3] = (0xFF & ((long)data_[3]));
    bytes[4] = (0xFF & ((long)data_[4]));
    bytes[5] = (0xFF & ((long)data_[5]));
    bytes[6] = (0xFF & ((long)data_[6]));
    bytes[7] = (0xFF & ((long)data_[7]));

    longValue_ = ((long) ((bytes[0] << 56) | (bytes[1] << 48) |
                          (bytes[2] << 40) | (bytes[3] << 32) |
                          (bytes[4] << 24) | (bytes[5] << 16) |
                          (bytes[6] << 8)  | (bytes[7])));
  }

  public String toString() {
    return String.valueOf(longValue_);
  }

  public int hashCode() {
    return (int)longValue_;
  }

  public boolean equals(Object that) {
    return ((that instanceof Int64) &&
            (this.longValue_ == ((Int64) that).longValue_));
  }

}
