package com.facebook.thrift.server;

import com.facebook.thrift.TException;
import com.facebook.thrift.TProcessor;
import com.facebook.thrift.protocol.TProtocol;
import com.facebook.thrift.protocol.TProtocolFactory;
import com.facebook.thrift.transport.TServerTransport;
import com.facebook.thrift.transport.TTransport;
import com.facebook.thrift.transport.TTransportException;
import com.facebook.thrift.transport.TTransportFactory;

import java.util.concurrent.ExecutorService;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;


/**
 * Server which uses Java's built in ThreadPool management to spawn off
 * a worker pool that 
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public class TThreadPoolServer extends TServer {

  // Executor service for handling client connections
  private ExecutorService executorService_;

  // Customizable server options
  public static class Options {
    public int minWorkerThreads = 5;
    public int maxWorkerThreads = Integer.MAX_VALUE;
  }

  public TThreadPoolServer(TProcessor processor,
                           TServerTransport serverTransport) {
    this(processor, serverTransport, new Options());
  }
  
  public TThreadPoolServer(TProcessor processor,
                           TServerTransport serverTransport,
                           Options options) {
    super(processor, serverTransport);

    executorService_ = null;

    LinkedBlockingQueue<Runnable> executorQueue =
      new LinkedBlockingQueue<Runnable>();

    executorService_ = new ThreadPoolExecutor(options.minWorkerThreads,
                                              options.maxWorkerThreads,
                                              60,
                                              TimeUnit.SECONDS,
                                              executorQueue);
  }


  public void serve() {
    try {
      serverTransport_.listen();
    } catch (TTransportException ttx) {
      ttx.printStackTrace();
      return;
    }

    while (true) {
      int failureCount = 0;
      try {
        TTransport client = serverTransport_.accept();
        WorkerProcess wp = new WorkerProcess(client);
        executorService_.execute(wp);
      } catch (TTransportException ttx) {
        ++failureCount;
        ttx.printStackTrace();
      }     
    }
  }

  private class WorkerProcess implements Runnable {

    /**
     * Client that this services.
     */
    private TTransport client_;

    /**
     * Default constructor.
     *
     * @param client Transport to process
     */
    private WorkerProcess(TTransport client) {
      client_ = client;
    }

    /**
     * Loops on processing a client forever
     */
    public void run() {
      TTransport[] iot = null;
      TProtocol[] iop = null;
      try {
        iot = transportFactory_.getIOTransports(client_);
        iop = protocolFactory_.getIOProtocols(iot[0], iot[1]);
        while (processor_.process(iop[0], iop[1])) {}
      } catch (TTransportException ttx) {
        // Assume the client died and continue silently
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
