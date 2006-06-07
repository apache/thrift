package com.facebook.thrift.protocol;

import com.facebook.thrift.types.*;

/**
 * Helper class that encapsulates struct metadata.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public class TStruct {
  public TStruct() {}

  public TStruct(String n) {
    name = n;
  }

  public String name = "";
}
