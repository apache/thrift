package com.facebook.thrift.transport;

/**
 * Factory class used to create wrapped instance of Transports.
 * This is used primarily in servers, which get Transports from
 * a ServerTransport and then may want to mutate them (i.e. create
 * a BufferedTransport from the underlying base transport)
 *
 * @author Mark Slee <mcslee@facebook.com>
 * @author Aditya Agarwal <aditya@facebook.com>
 */
public class TTransportFactory {

  /**
   * Return a wrapped instance of the base Transport.
   *
   * @param in The base transport
   * @returns Wrapped Transport
   */
  public TTransport getTransport(TTransport trans) {
    return trans;
  }

}
