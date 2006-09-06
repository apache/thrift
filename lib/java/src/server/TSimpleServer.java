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

  private TServerTransport serverTransport_;

  public TSimpleServer(TProcessor processor,
                       TServerTransport serverTransport) {
    this(processor, new TServer.Options(), serverTransport);
  }


  public TSimpleServer(TProcessor processor,
                       TServer.Options options,
                       TServerTransport serverTransport) {
    super(processor, options);
    serverTransport_ = serverTransport;
  }

  public void run() {
    try {
      serverTransport_.listen();
    } catch (TTransportException ttx) {
      ttx.printStackTrace();
      return;
    }

    while (true) {
      TTransport client = null;
      try {
        client = serverTransport_.accept();
        if (client != null) {
          while (processor_.process(client, client));
        }
      } catch (TException tx) {
        tx.printStackTrace();
      }

      if (client != null) {
        client.close();
        client = null;
      }
    }
  }
}
