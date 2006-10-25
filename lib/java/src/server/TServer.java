package com.facebook.thrift.server;

import com.facebook.thrift.TProcessor;
import com.facebook.thrift.protocol.TBinaryProtocol;
import com.facebook.thrift.protocol.TProtocolFactory;
import com.facebook.thrift.transport.TServerTransport;
import com.facebook.thrift.transport.TTransportFactory;

/**
 * Generic interface for a Thrift server.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public abstract class TServer {

  /**
   * Core processor
   */
  protected TProcessor processor_;

  /**
   * Server transport
   */
  protected TServerTransport serverTransport_;

  /**
   * Transport Factory
   */
  protected TTransportFactory transportFactory_;

  /**
   * Protocol Factory
   */
  protected TProtocolFactory protocolFactory_;

  /**
   * Default constructors.
   */

  protected TServer(TProcessor processor,
                    TServerTransport serverTransport) {
    this(processor,
         serverTransport,
         new TTransportFactory(),
         new TBinaryProtocol.Factory());
  }

  protected TServer(TProcessor processor,
                    TServerTransport serverTransport,
                    TTransportFactory transportFactory) {
    this(processor,
         serverTransport,
         transportFactory,
         new TBinaryProtocol.Factory());
  }

  protected TServer(TProcessor processor,
                    TServerTransport serverTransport,
                    TTransportFactory transportFactory,
                    TProtocolFactory protocolFactory) {
    processor_ = processor;
    serverTransport_ = serverTransport;
    transportFactory_ = transportFactory;
    protocolFactory_ = protocolFactory;
  }
  
  /**
   * The run method fires up the server and gets things going.
   */
  public abstract void serve();

}
