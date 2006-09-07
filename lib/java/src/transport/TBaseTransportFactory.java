package com.facebook.thrift.transport;

/**
 * Base transport factory just returns the arg transport.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public class TBaseTransportFactory implements TTransportFactory {

  /**
   * Returns a list of two transports (input, output) from a simple
   * Transport.
   *
   * @param in The base transport
   * @returns Array of two transports, first for input, second for output
   */
  public TTransport[] getIOTransports(TTransport in) {
    TTransport[] out = new TTransport[2];
    out[0] = out[1] = in;
    return out;
  }

}
