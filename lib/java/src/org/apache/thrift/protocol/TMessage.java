// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

package org.apache.thrift.protocol;

/**
 * Helper class that encapsulates struct metadata.
 *
 */
public final class TMessage {
  public TMessage() {
    this("", TType.STOP, 0);
  }

  public TMessage(String n, byte t, int s) {
    name = n;
    type = t;
    seqid = s;
  }

  public final String name;
  public final byte type;
  public final int seqid;

  public String toString() {
    return "<TMessage name:'" + name + "' type: " + type + " seqid:" + seqid + ">";
  }
  
  public boolean equals(TMessage other) {
    return name.equals(other.name) && type == other.type && seqid == other.seqid;
  }
}
