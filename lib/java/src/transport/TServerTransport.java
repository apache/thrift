package com.facebook.thrift.transport;

/**
 * Server transport. Object which provides client transports.
 *
 * @author Mark Slee <mcslee@facebook.com>
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
}
