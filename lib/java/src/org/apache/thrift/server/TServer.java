// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

package org.apache.thrift.server;

import org.apache.thrift.TProcessorFactory;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocolFactory;
import org.apache.thrift.transport.TServerTransport;
import org.apache.thrift.transport.TTransportFactory;

/**
 * Generic interface for a Thrift server.
 *
 */
public abstract class TServer {

  /**
   * Core processor
   */
  protected TProcessorFactory processorFactory_;

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

  protected TServer(TProcessorFactory processorFactory,
                    TServerTransport serverTransport) {
    this(processorFactory,
         serverTransport,
         new TTransportFactory(),
         new TTransportFactory(),
         new TBinaryProtocol.Factory(),
         new TBinaryProtocol.Factory());
  }

  protected TServer(TProcessorFactory processorFactory,
                    TServerTransport serverTransport,
                    TTransportFactory transportFactory) {
    this(processorFactory,
         serverTransport,
         transportFactory,
         transportFactory,
         new TBinaryProtocol.Factory(),
         new TBinaryProtocol.Factory());
  }

  protected TServer(TProcessorFactory processorFactory,
                    TServerTransport serverTransport,
                    TTransportFactory transportFactory,
                    TProtocolFactory protocolFactory) {
    this(processorFactory,
         serverTransport,
         transportFactory,
         transportFactory,
         protocolFactory,
         protocolFactory);
  }

  protected TServer(TProcessorFactory processorFactory,
                    TServerTransport serverTransport,
                    TTransportFactory inputTransportFactory,
                    TTransportFactory outputTransportFactory,
                    TProtocolFactory inputProtocolFactory,
                    TProtocolFactory outputProtocolFactory) {
    processorFactory_ = processorFactory;
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

  /**
   * Stop the server. This is optional on a per-implementation basis. Not
   * all servers are required to be cleanly stoppable.
   */
  public void stop() {}

}
