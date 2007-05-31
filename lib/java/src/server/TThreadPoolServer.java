// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

package com.facebook.thrift.server;

import com.facebook.thrift.TException;
import com.facebook.thrift.TProcessor;
import com.facebook.thrift.TProcessorFactory;
import com.facebook.thrift.protocol.TProtocol;
import com.facebook.thrift.protocol.TProtocolFactory;
import com.facebook.thrift.protocol.TBinaryProtocol;
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
    this(processor, serverTransport, 
         new TTransportFactory(), new TTransportFactory(),
         new TBinaryProtocol.Factory(), new TBinaryProtocol.Factory(),
         new Options());
  }

  public TThreadPoolServer(TProcessorFactory processorFactory,
          TServerTransport serverTransport) {
    this(processorFactory, serverTransport, 
         new TTransportFactory(), new TTransportFactory(),
         new TBinaryProtocol.Factory(), new TBinaryProtocol.Factory(),
         new Options());
  }

  public TThreadPoolServer(TProcessor processor,
                           TServerTransport serverTransport,
                           TTransportFactory transportFactory,
                           TProtocolFactory protocolFactory) {
    this(processor, serverTransport, 
         transportFactory, transportFactory,
         protocolFactory, protocolFactory,
         new Options());
  }

  public TThreadPoolServer(TProcessorFactory processorFactory,
          TServerTransport serverTransport,
          TTransportFactory transportFactory,
          TProtocolFactory protocolFactory) {
    this(processorFactory, serverTransport, 
         transportFactory, transportFactory,
         protocolFactory, protocolFactory,
         new Options());
  }

  
  public TThreadPoolServer(TProcessor processor,
          TServerTransport serverTransport,
          TTransportFactory inputTransportFactory,
          TTransportFactory outputTransportFactory,
          TProtocolFactory inputProtocolFactory,
          TProtocolFactory outputProtocolFactory,
          Options options) {
    this(new TProcessorFactory(processor), serverTransport,
         inputTransportFactory, outputTransportFactory,
         inputProtocolFactory, outputProtocolFactory,
         options);
  }
  
  public TThreadPoolServer(TProcessorFactory processorFactory,
                           TServerTransport serverTransport,
                           TTransportFactory inputTransportFactory,
                           TTransportFactory outputTransportFactory,
                           TProtocolFactory inputProtocolFactory,
                           TProtocolFactory outputProtocolFactory,
                           Options options) {
    super(processorFactory, serverTransport, 
          inputTransportFactory, outputTransportFactory,
          inputProtocolFactory, outputProtocolFactory);

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
      TProcessor processor = null;
      TTransport inputTransport = null;
      TTransport outputTransport = null;
      TProtocol inputProtocol = null;
      TProtocol outputProtocol = null;
      try {
        processor = processorFactory_.getProcessor(client_);
        inputTransport = inputTransportFactory_.getTransport(client_);
        outputTransport = outputTransportFactory_.getTransport(client_);
        inputProtocol = inputProtocolFactory_.getProtocol(inputTransport);
        outputProtocol = outputProtocolFactory_.getProtocol(outputTransport);
        while (processor.process(inputProtocol, outputProtocol)) {}
      } catch (TTransportException ttx) {
        // Assume the client died and continue silently
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
