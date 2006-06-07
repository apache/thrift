package com.facebook.thrift.protocol;

import com.facebook.thrift.types.*;

/**
 * Helper class that encapsulates set metadata.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public class TSet {
  public TSet() {}

  public TSet(TType t, int s) {
    this(t, new UInt32(s));
  }

  public TSet(TType t, UInt32 s) {
    elemType = t;
    size = s;
  }

  public TType  elemType = TType.STOP;
  public UInt32 size = new UInt32();
}
