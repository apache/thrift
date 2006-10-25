package com.facebook.thrift.server;

import com.facebook.thrift.TException;
import com.facebook.thrift.TProcessor;
import com.facebook.thrift.protocol.TProtocol;
import com.facebook.thrift.protocol.TProtocolFactory;
import com.facebook.thrift.transport.TServerTransport;
import com.facebook.thrift.transport.TTransport;
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

  public void serve() {
    try {
      serverTransport_.listen();
    } catch (TTransportException ttx) {
      ttx.printStackTrace();
      return;
    }

    while (true) {
      TTransport client = null;
      TTransport[] iot = null;
      TProtocol[] iop = null;
      try {
        client = serverTransport_.accept();
        if (client != null) {
          iot = transportFactory_.getIOTransports(client);
          iop = protocolFactory_.getIOProtocols(iot[0], iot[1]);
          while (processor_.process(iop[0], iop[1]));
        }
      } catch (TTransportException ttx) {
        // Client died, just move on
      } catch (TException tx) {
        tx.printStackTrace();
      } catch (Exception x) {
        x.printStackTrace();
      }

      if (iot != null) {
        if (iot[0] != null) {
          iot[0].close();
        }
        if (iot[1] != null) {
          iot[1].close();
        }
      }
    }
  }
}
