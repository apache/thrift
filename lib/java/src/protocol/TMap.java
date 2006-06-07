package com.facebook.thrift.protocol;

import com.facebook.thrift.types.*;

/**
 * Helper class that encapsulates map metadata.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public class TMap {
  public TMap() {}
  
  public TMap(TType k, TType v, int s) {
    this(k, v, new UInt32(s));
  }

  public TMap(TType k, TType v, UInt32 s) {
    keyType = k;
    valueType = v;
    size = s;
  }

  public TType  keyType = TType.STOP;
  public TType  valueType = TType.STOP;
  public UInt32 size = new UInt32();;
}
