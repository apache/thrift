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
   * Input Transport Factory
   */
  protected TTransportFactory inputTransportFactory_;

  /**
   * Output Transport Factory
   */
  protected TTransportFactory outputTransportFactory_;

  /**
   * Input Protocol Factory
   */
  protected TProtocolFactory inputProtocolFactory_;

  /**
   * Output Protocol Factory
   */
  protected TProtocolFactory outputProtocolFactory_;

  /**
   * Default constructors.
   */

  protected TServer(TProcessor processor,
                    TServerTransport serverTransport) {
    this(processor,
         serverTransport,         
         new TTransportFactory(),
         new TTransportFactory(),
         new TBinaryProtocol.Factory(),
         new TBinaryProtocol.Factory());
  }

  protected TServer(TProcessor processor,
                    TServerTransport serverTransport,
                    TTransportFactory transportFactory) {
    this(processor,
         serverTransport,
         transportFactory,
         transportFactory,
         new TBinaryProtocol.Factory(),
         new TBinaryProtocol.Factory());
  }

  protected TServer(TProcessor processor,
                    TServerTransport serverTransport,
                    TTransportFactory transportFactory,
                    TProtocolFactory protocolFactory) {
    this(processor,
         serverTransport,
         transportFactory,
         transportFactory,
         protocolFactory,
         protocolFactory);
  }

  protected TServer(TProcessor processor,
                    TServerTransport serverTransport,
                    TTransportFactory inputTransportFactory,
                    TTransportFactory outputTransportFactory,
                    TProtocolFactory inputProtocolFactory,
                    TProtocolFactory outputProtocolFactory) {
    processor_ = processor;
    serverTransport_ = serverTransport;
    inputTransportFactory_ = inputTransportFactory;
    outputTransportFactory_ = outputTransportFactory;
    inputProtocolFactory_ = inputProtocolFactory;
    outputProtocolFactory_ = outputProtocolFactory;
  }
  
  /**
   * The run method fires up the server and gets things going.
   */
  public abstract void serve();

}
