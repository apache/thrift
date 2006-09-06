package com.facebook.thrift.server;

import com.facebook.thrift.TException;
import com.facebook.thrift.TProcessor;
import com.facebook.thrift.transport.TServerTransport;
import com.facebook.thrift.transport.TTransport;
import com.facebook.thrift.transport.TTransportException;

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

  // Server transport
  private TServerTransport serverTransport_;

  // Executor service for handling client connections
  private ExecutorService executorService_;

  // Customizable server options
  public static class Options extends TServer.Options {
    public int port = 9190;
    public int minWorkerThreads = 5;
    public int maxWorkerThreads = Integer.MAX_VALUE;
  }

  public TThreadPoolServer(TProcessor processor,
                           TServerTransport serverTransport) {
    this(processor, new Options(), serverTransport);
  }

  public TThreadPoolServer(TProcessor processor,
                           Options options,
                           TServerTransport serverTransport) {
    super(processor, options);
    serverTransport_ = serverTransport;
    executorService_ = null;

    LinkedBlockingQueue<Runnable> executorQueue =
      new LinkedBlockingQueue<Runnable>();

    executorService_ = new ThreadPoolExecutor(options.minWorkerThreads,
                                              options.maxWorkerThreads,
                                              60,
                                              TimeUnit.SECONDS,
                                              executorQueue);
  }


  public void run() {
    try {
      serverTransport_.listen();
    } catch (TTransportException ttx) {
      ttx.printStackTrace();
      return;
    }

    while (true) {
      try {
        TTransport client = serverTransport_.accept();
        WorkerProcess wp = new WorkerProcess(client);
        executorService_.execute(wp);
      } catch (TTransportException ttx) {
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
      try {
        while (processor_.process(client_, client_)) {}
      } catch (TException tx) {
        tx.printStackTrace();
      }
      client_.close();
    }
  }
}
