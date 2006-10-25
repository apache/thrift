package com.facebook.thrift.transport;

/**
 * Factory class used to create an input and output transport out of a simple
 * transport. This is used primarily in servers, which get Transports from
 * a ServerTransport and then may want to mutate them.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public class TTransportFactory {

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
