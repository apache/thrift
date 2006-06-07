package com.facebook.thrift.protocol;

import com.facebook.thrift.types.*;

/**
 * Helper class that encapsulates field metadata.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public class TField {
  public TField() {}

  public TField(String n, TType t, int i) {
    this(n, t, new UInt32(i));
  }

  public TField(String n, TType t, UInt32 i) {
    name = n;
    type = t;
    id = i;
  }

  public String name = "";
  public TType  type = TType.STOP;
  public UInt32 id = new UInt32();
}
