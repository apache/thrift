// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

package org.apache.thrift.transport;

/**
 * Server transport. Object which provides client transports.
 *
 */
public abstract class TServerTransport {

  public abstract void listen() throws TTransportException;

  public final TTransport accept() throws TTransportException {
    TTransport transport = acceptImpl();
    if (transport == null) {
      throw new TTransportException("accept() may not return NULL");
    }
    return transport;
  }

  public abstract void close();

  protected abstract TTransport acceptImpl() throws TTransportException;

  /**
   * Optional method implementation. This signals to the server transport
   * that it should break out of any accept() or listen() that it is currently
   * blocked on. This method, if implemented, MUST be thread safe, as it may
   * be called from a different thread context than the other TServerTransport
   * methods.
   */
  public void interrupt() {}

}
