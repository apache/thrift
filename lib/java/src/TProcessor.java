package com.facebook.thrift;

import com.facebook.thrift.transport.TTransport;

/**
 * A processor is a generic object which operates upon an input stream and
 * writes to some output stream.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public interface TProcessor {
  public boolean process(TTransport in, TTransport out)
    throws TException;
}
