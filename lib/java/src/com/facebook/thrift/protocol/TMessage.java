// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

package com.facebook.thrift.protocol;

/**
 * Helper class that encapsulates struct metadata.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public class TMessage {
  public TMessage() {}

  public TMessage(String n, byte t, int s) {
    name = n;
    type = t;
    seqid = s;
  }

  public String name = "";
  public byte type;
  public int seqid;
}
