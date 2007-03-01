// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

package com.facebook.thrift.server;

import com.facebook.thrift.TException;
import com.facebook.thrift.TProcessor;
import com.facebook.thrift.protocol.TProtocol;
import com.facebook.thrift.protocol.TProtocolFactory;
import com.facebook.thrift.transport.TServerTransport;
import com.facebook.thrift.transport.TTransport;
import com.facebook.thrift.transport.TTransportFactory;
import com.facebook.thrift.transport.TTransportException;

/**
 * Simple singlethreaded server for testing.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public class TSimpleServer extends TServer {

  public TSimpleServer(TProcessor processor,
                       TServerTransport serverTransport) {
    super(processor, serverTransport);
  }

  public TSimpleServer(TProcessor processor,
                       TServerTransport serverTransport,
                       TTransportFactory transportFactory,
                       TProtocolFactory protocolFactory) {
    super(processor, serverTransport, transportFactory, protocolFactory);
  }

  public TSimpleServer(TProcessor processor,
                       TServerTransport serverTransport,
                       TTransportFactory inputTransportFactory,
                       TTransportFactory outputTransportFactory,
                       TProtocolFactory inputProtocolFactory,
                       TProtocolFactory outputProtocolFactory) {                       
    super(processor, serverTransport, 
          inputTransportFactory, outputTransportFactory,
          inputProtocolFactory, outputProtocolFactory);
  }

  public void serve() {
    try {
      serverTransport_.listen();
    } catch (TTransportException ttx) {
      ttx.printStackTrace();
      return;
    }

    while (true) {
      TTransport client = null;
      TTransport inputTransport = null;
      TTransport outputTransport = null;
      TProtocol inputProtocol = null;
      TProtocol outputProtocol = null;
      try {
        client = serverTransport_.accept();
        if (client != null) {
          inputTransport = inputTransportFactory_.getTransport(client);
          outputTransport = outputTransportFactory_.getTransport(client);
          inputProtocol = inputProtocolFactory_.getProtocol(inputTransport);
          outputProtocol = outputProtocolFactory_.getProtocol(outputTransport);
          while (processor_.process(inputProtocol, outputProtocol)) {}
        }
      } catch (TTransportException ttx) {
        // Client died, just move on
      } catch (TException tx) {
        tx.printStackTrace();
      } catch (Exception x) {
        x.printStackTrace();
      }

      if (inputTransport != null) {
        inputTransport.close();
      }

      if (outputTransport != null) {
        outputTransport.close();
      }

    }
  }
}
