package com.facebook.thrift.types;

import java.io.ByteArrayOutputStream;

/**
 * Wrapper for Pillar TyInt32. We have to flip the byte order in here
 * because Pillar is little endian.
 *
 * @author Mark Slee (mcslee@facebook.com)
 *
 * See: http://darksleep.com/player/JavaAndUnsignedTypes.html
 *
 */
public class Int32 {
  private int intValue_ = 0;
  private byte[] data_ = new byte[4];
  
  public int get() {
    return intValue_;
  }

  public byte[] data() {
    return data_;
  }

  public Int32() {
    for (int i = 0; i < 4; i++) {
      data_[i] = 0;
    }
  }

  public Int32(byte[] buf, int offset) {
    read(buf, offset);
  }

  public Int32(int value) {
    set(value);
  }

  public void set(int value) {
    intValue_ = value;    
    data_[0] = (byte)((intValue_ & 0xFF000000L) >> 24);
    data_[1] = (byte)((intValue_ & 0x00FF0000L) >> 16);
    data_[2] = (byte)((intValue_ & 0x0000FF00L) >> 8);
    data_[3] = (byte)((intValue_ & 0x000000FFL));
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
    intValue_ = ((int) (bytes[0] << 24 | bytes[1] << 16 |
                        bytes[2] << 8 | bytes[3]));
  }

  public String toString() {
    return String.valueOf(intValue_);
  }

  public int hashCode() {
    return intValue_;
  }

  public boolean equals(Object that) {
    return ((that instanceof Int32) &&
            (this.intValue_ == ((Int32) that).intValue_));
  }

}
