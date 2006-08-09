package com.facebook.thrift.protocol;

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
