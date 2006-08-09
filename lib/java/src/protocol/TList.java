package com.facebook.thrift.protocol;

/**
 * Helper class that encapsulates list metadata.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public class TList {
  public TList() {}
  
  public TList(byte t, int s) {
    elemType = t;
    size = s;
  }

  public byte elemType = TType.STOP;
  public int  size     = 0;
}
