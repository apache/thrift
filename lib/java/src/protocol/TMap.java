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
    this(k, v, new Int32(s));
  }

  public TMap(TType k, TType v, Int32 s) {
    keyType = k;
    valueType = v;
    size = s;
  }

  public TType  keyType = TType.STOP;
  public TType  valueType = TType.STOP;
  public Int32 size = new Int32();;
}
