package com.facebook.thrift.protocol;

import com.facebook.thrift.transport.TTransport;

/**
 * Factory interface for constructing protocol instances.
 *
 * @author Mark Slee <mcslee@facebook.com>
 * @author Aditya Agarwal <aditya@facebook.com>
 */
public interface TProtocolFactory {
  public TProtocol getProtocol(TTransport trans);
}
