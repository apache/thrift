package com.facebook.thrift.protocol;

import com.facebook.thrift.transport.TTransport;

/**
 * Factory interface for constructing protocol encoder/decoder pair from an
 * input and output transport.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public interface TProtocolFactory {
  public TProtocol[] getIOProtocols(TTransport in, TTransport out);
}
