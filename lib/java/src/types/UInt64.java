package com.facebook.thrift.types;

import java.io.ByteArrayOutputStream;

/**
 * More bit-twiddling bullshit. Take a look at UInt32 for details about
 * the endian difference between Pillar and the JVM.
 *
 * Since we don't do arithmetic on unsigned longs, we just implement
 * them here as a raw byte array. The only caveat is the constructor
 * that takes an integer value. That value will be big-endian (JVM)
 * but the byte array needs to be little-endian (Pillar)
 *
 * @author Mark Slee (mcslee@facebook.com)
 *
 */
public class UInt64 {
  private byte[] data_ = new byte[8];

  public byte[] data() {
    return data_;
  }

  public UInt64() {
    for (int i = 0; i < 8; i++) {
      data_[i] = (byte) 0;
    }
  }

  public UInt64(long value) {
    set(value);
  }

  public UInt64(byte[] buf, int offset) {
    read(buf, offset);
  }

  public long toLong() {
    long[] bytes = new long[8];
    bytes[0] = (0xFF & ((long)data_[0]));
    bytes[1] = (0xFF & ((long)data_[1]));
    bytes[2] = (0xFF & ((long)data_[2]));
    bytes[3] = (0xFF & ((long)data_[3]));
    bytes[4] = (0xFF & ((long)data_[4]));
    bytes[5] = (0xFF & ((long)data_[5]));
    bytes[6] = (0xFF & ((long)data_[6]));
    bytes[7] = (0xFF & ((long)data_[7]));

    return ((long) ((bytes[0] << 56) | (bytes[1] << 48) |
                    (bytes[2] << 40) | (bytes[3] << 32) |
                    (bytes[4] << 24) | (bytes[5] << 16) |
                    (bytes[6] << 8)  | (bytes[7])));
  }

  /**
   * "HOLD IT! Pay close attention..." -Prodigy - Out of Space
   *
   * This is some wacky business. We want to take the integer value
   * represented JVM style and put it into an 8-byte array that mirrors
   * Intel/Pillar endianness (little). To do this, we graduate the
   * integer to a long and then reverse the byte order, so bytes
   * 5-8 in the JVM long become bytes 3-0 in the Pillar representation.
   *
   * NOTE: value MUST be positive, or else shit gets ill.
   */
  public void set(long longValue) {
    if (longValue < 0) {
      throw new RuntimeException("Cannot make UInt64 from a negative value.");
    }
    data_[0] = (byte)((longValue & 0xFF00000000000000L) >> 56);
    data_[1] = (byte)((longValue & 0x00FF000000000000L) >> 48);
    data_[2] = (byte)((longValue & 0x0000FF0000000000L) >> 40);
    data_[3] = (byte)((longValue & 0x000000FF00000000L) >> 32);
    data_[4] = (byte)((longValue & 0x00000000FF000000L) >> 24);
    data_[5] = (byte)((longValue & 0x0000000000FF0000L) >> 16);
    data_[6] = (byte)((longValue & 0x000000000000FF00L) >> 8);
    data_[7] = (byte)((longValue & 0x00000000000000FFL));
  }

  public void read(byte[] buf, int offset) {
    for (int i = 0; i < 8; i++) {
      data_[i] = buf[offset+i];
    }
  }

  /**
   * Equivalent to << 8, shifting left by a byte.
   */
  public UInt64 lshift() {
    for (int i = 7; i > 0; i--) {
      data_[i] = data_[i-1];
    }
    data_[0] = (byte) 0;
    return this;
  }

  /**
   * Equivalent to |, logical or across all bytes
   */
  public UInt64 lor(UInt64 that) {
    for (int i = 0; i < 8; i++) {
      this.data_[i] = (byte) (this.data_[i] | that.data_[i]);
    }
    return this;
  }

  public int hashCode() {
    return ((0xFF & (int)data_[3]) << 24) |
           ((0xFF & (int)data_[2]) << 16) |
           ((0xFF & (int)data_[1]) << 8) |
           ((0xFF & (int)data_[0]));
  }

  public boolean equals(Object that) {
    if (that instanceof UInt64) {
      for (int i = 0; i < 8; i++) {
        if (this.data_[i] != ((UInt64)that).data_[i]) {
          return false;
        }
      }
      return true;
    }
    return false;
  }

}
