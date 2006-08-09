package com.facebook.thrift.protocol;

/**
 * Helper class that encapsulates map metadata.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public class TMap {
  public TMap() {}
  
  public TMap(byte k, byte v, int s) {
    keyType = k;
    valueType = v;
    size = s;
  }

  public byte  keyType   = TType.STOP;
  public byte  valueType = TType.STOP;
  public int   size      = 0;
}
