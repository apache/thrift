package com.facebook.thrift.server;

import com.facebook.thrift.TException;
import com.facebook.thrift.TProcessor;
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
      TTransport[] io = null;
      try {
        client = serverTransport_.accept();
        if (client != null) {
          io = transportFactory_.getIOTransports(client);
          while (processor_.process(io[0], io[1]));
        }
      } catch (TTransportException ttx) {
        // Client died, just move on
      } catch (TException tx) {
        tx.printStackTrace();
      } catch (Exception x) {
        x.printStackTrace();
      }

      if (io != null) {
        if (io[0] != null) {
          io[0].close();
        }
        if (io[1] != null) {
          io[1].close();
        }
      }
    }
  }
}
