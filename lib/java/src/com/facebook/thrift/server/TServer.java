// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

package com.facebook.thrift.server;

import com.facebook.thrift.TProcessorFactory;
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
