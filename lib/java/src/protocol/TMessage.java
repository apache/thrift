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
