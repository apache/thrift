package com.facebook.thrift.server;

import com.facebook.thrift.TProcessor;
import com.facebook.thrift.transport.TServerTransport;
import com.facebook.thrift.transport.TTransportFactory;
import com.facebook.thrift.transport.TBaseTransportFactory;

/**
 * Generic interface for a Thrift server.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public abstract class TServer {

  /**
   * The options class should be subclassed by particular servers which have
   * specific options needs, while the general options should live here.
   */
  public static class Options {
    public Options() {}
  }

  /**
   * Core processor
   */
  protected TProcessor processor_;

  /**
   * Server options
   */
  protected Options options_;

  /**
   * Server transport
   */
  protected TServerTransport serverTransport_;

  /**
   * Transport Factory
   */
  protected TTransportFactory transportFactory_;

  /**
   * Default constructors.
   */

  protected TServer(TProcessor processor,
                    TServerTransport serverTransport) {
    this(processor,
         serverTransport,
         new TBaseTransportFactory(),
         new Options());
  }

  protected TServer(TProcessor processor,
                    TServerTransport serverTransport,
                    TTransportFactory transportFactory) {
    this(processor,
         serverTransport,
         transportFactory,
         new Options());
  }


  protected TServer(TProcessor processor,
                    TServerTransport serverTransport,
                    Options options) {
    this(processor,
         serverTransport,
         new TBaseTransportFactory(),
         options);
  }

  protected TServer(TProcessor processor,
                    TServerTransport serverTransport,
                    TTransportFactory transportFactory,
                    Options options) {
    processor_ = processor;
    serverTransport_ = serverTransport;
    transportFactory_ = transportFactory;
    options_ = options;
  }
  
  /**
   * The run method fires up the server and gets things going.
   */
  public abstract void serve();
}
