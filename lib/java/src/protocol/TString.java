package com.facebook.thrift.protocol;

/**
 * Wrapper around String so that you can pass this object to a function and
 * have it set the internal string value.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public class TString {
  public TString() {}

  public TString(String v) {
    value = v;
  }

  public String value = "";
}
