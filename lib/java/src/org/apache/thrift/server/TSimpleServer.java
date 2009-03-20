// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

package org.apache.thrift.server;

import java.util.logging.Level;
import java.util.logging.Logger;

import org.apache.thrift.TException;
import org.apache.thrift.TProcessor;
import org.apache.thrift.TProcessorFactory;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.protocol.TProtocolFactory;
import org.apache.thrift.transport.TServerTransport;
import org.apache.thrift.transport.TTransport;
import org.apache.thrift.transport.TTransportFactory;
import org.apache.thrift.transport.TTransportException;

/**
 * Simple singlethreaded server for testing.
 *
 */
public class TSimpleServer extends TServer {

  private static final Logger LOGGER = Logger.getLogger(TSimpleServer.class.getName());

  private boolean stopped_ = false;

  public TSimpleServer(TProcessor processor,
                       TServerTransport serverTransport) {
    super(new TProcessorFactory(processor), serverTransport);
  }

  public TSimpleServer(TProcessor processor,
                       TServerTransport serverTransport,
                       TTransportFactory transportFactory,
                       TProtocolFactory protocolFactory) {
    super(new TProcessorFactory(processor), serverTransport, transportFactory, protocolFactory);
  }

  public TSimpleServer(TProcessor processor,
                       TServerTransport serverTransport,
                       TTransportFactory inputTransportFactory,
                       TTransportFactory outputTransportFactory,
                       TProtocolFactory inputProtocolFactory,
                       TProtocolFactory outputProtocolFactory) {
    super(new TProcessorFactory(processor), serverTransport,
          inputTransportFactory, outputTransportFactory,
          inputProtocolFactory, outputProtocolFactory);
  }

  public TSimpleServer(TProcessorFactory processorFactory,
          TServerTransport serverTransport) {
    super(processorFactory, serverTransport);
  }

  public TSimpleServer(TProcessorFactory processorFactory,
          TServerTransport serverTransport,
          TTransportFactory transportFactory,
          TProtocolFactory protocolFactory) {
    super(processorFactory, serverTransport, transportFactory, protocolFactory);
  }

  public TSimpleServer(TProcessorFactory processorFactory,
          TServerTransport serverTransport,
          TTransportFactory inputTransportFactory,
          TTransportFactory outputTransportFactory,
          TProtocolFactory inputProtocolFactory,
          TProtocolFactory outputProtocolFactory) {
    super(processorFactory, serverTransport,
          inputTransportFactory, outputTransportFactory,
          inputProtocolFactory, outputProtocolFactory);
  }


  public void serve() {
    stopped_ = false;
    try {
      serverTransport_.listen();
    } catch (TTransportException ttx) {
      LOGGER.log(Level.SEVERE, "Error occurred during listening.", ttx);
      return;
    }

    while (!stopped_) {
      TTransport client = null;
      TProcessor processor = null;
      TTransport inputTransport = null;
      TTransport outputTransport = null;
      TProtocol inputProtocol = null;
      TProtocol outputProtocol = null;
      try {
        client = serverTransport_.accept();
        if (client != null) {
          processor = processorFactory_.getProcessor(client);
          inputTransport = inputTransportFactory_.getTransport(client);
          outputTransport = outputTransportFactory_.getTransport(client);
          inputProtocol = inputProtocolFactory_.getProtocol(inputTransport);
          outputProtocol = outputProtocolFactory_.getProtocol(outputTransport);
          while (processor.process(inputProtocol, outputProtocol)) {}
        }
      } catch (TTransportException ttx) {
        // Client died, just move on
      } catch (TException tx) {
        if (!stopped_) {
          LOGGER.log(Level.SEVERE, "Thrift error occurred during processing of message.", tx);
        }
      } catch (Exception x) {
        if (!stopped_) {
          LOGGER.log(Level.SEVERE, "Error occurred during processing of message.", x);
        }
      }

      if (inputTransport != null) {
        inputTransport.close();
      }

      if (outputTransport != null) {
        outputTransport.close();
      }

    }
  }

  public void stop() {
    stopped_ = true;
    serverTransport_.interrupt();
  }
}
