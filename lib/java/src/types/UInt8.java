package com.facebook.thrift.types;

import java.io.ByteArrayOutputStream;

/**
 * WILL IT EVER END? Even 'byte' is signed in Java, so we must use
 * a short to represent bytes. 
 *
 * @author Mark Slee (mcslee@facebook.com)
 *
 */
public class UInt8 {
  private short shortValue_;
  private byte[] data_ = new byte[1];
  
  public short get() {
    return shortValue_;
  }

  public byte[] data() {
    return data_;
  }

  public UInt8() {
    data_[0] = (byte)0;
  }

  public UInt8(byte[] buf, int offset) {
    read(buf, offset);
  }

  public UInt8(short value) {
    set(value);
  }

  public void set(short value) {
    if (value < 0) throw new RuntimeException("Cannot apply negative value to UInt8");
    shortValue_ = value;
    data_[0] = (byte)((shortValue_ & 0x00FF));
  }

  public void read(byte[] buf, int offset) {
    data_[0] = buf[offset];
    shortValue_ = (short) (0x00FF & (short)data_[0]);
  }

  public String toString() {
    return String.valueOf(shortValue_);
  }

  public int hashCode() {
    return (int)shortValue_;
  }

  public boolean equals(Object that) {
    return ((that instanceof UInt8) &&
            (this.shortValue_ == ((UInt8) that).shortValue_));
  }

}
