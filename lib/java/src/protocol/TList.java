package com.facebook.thrift.protocol;

import com.facebook.thrift.types.*;

/**
 * Helper class that encapsulates list metadata.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public class TList {
  public TList() {}
  
  public TList(TType t, int s) {
    this(t, new Int32(s));
  }

  public TList(TType t, Int32 s) {
    elemType = t;
    size = s;
  }

  public TType  elemType = TType.STOP;
  public Int32 size = new Int32();
}
