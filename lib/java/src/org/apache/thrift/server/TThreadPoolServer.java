/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.apache.thrift.server;

import java.util.Optional;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.RejectedExecutionException;
import java.util.concurrent.SynchronousQueue;
import java.util.concurrent.ThreadFactory;
import java.util.concurrent.ThreadPoolExecutor;
import java.util.concurrent.TimeUnit;

import org.apache.thrift.TException;
import org.apache.thrift.TProcessor;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.transport.TServerTransport;
import org.apache.thrift.transport.TTransport;
import org.apache.thrift.transport.TTransportException;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Server which uses Java's built in ThreadPool management to spawn off
 * a worker pool that deals with client connections in blocking way.
 */
public class TThreadPoolServer extends TServer {
  private static final Logger LOGGER = LoggerFactory.getLogger(TThreadPoolServer.class);

  public static class Args extends AbstractServerArgs<Args> {
    public int minWorkerThreads = 5;
    public int maxWorkerThreads = Integer.MAX_VALUE;
    public ExecutorService executorService;
    public int stopTimeoutVal = 60;
    public TimeUnit stopTimeoutUnit = TimeUnit.SECONDS;

    public Args(TServerTransport transport) {
      super(transport);
    }

    public Args minWorkerThreads(int n) {
      minWorkerThreads = n;
      return this;
    }

    public Args maxWorkerThreads(int n) {
      maxWorkerThreads = n;
      return this;
    }

    public Args stopTimeoutVal(int n) {
      stopTimeoutVal = n;
      return this;
    }

    public Args stopTimeoutUnit(TimeUnit tu) {
      stopTimeoutUnit = tu;
      return this;
    }

    public Args executorService(ExecutorService executorService) {
      this.executorService = executorService;
      return this;
    }
  }

  // Executor service for handling client connections
  private ExecutorService executorService_;

  private final TimeUnit stopTimeoutUnit;

  private final long stopTimeoutVal;

  public TThreadPoolServer(Args args) {
    super(args);

    stopTimeoutUnit = args.stopTimeoutUnit;
    stopTimeoutVal = args.stopTimeoutVal;

    executorService_ = args.executorService != null ?
        args.executorService : createDefaultExecutorService(args);
  }

  private static ExecutorService createDefaultExecutorService(Args args) {
    return new ThreadPoolExecutor(args.minWorkerThreads, args.maxWorkerThreads, 60L, TimeUnit.SECONDS,
        new SynchronousQueue<>(), new ThreadFactory() {
          @Override
          public Thread newThread(Runnable r) {
            Thread thread = new Thread(r);
            thread.setDaemon(true);
            thread.setName("TThreadPoolServer WorkerProcess-%d");
            return thread;
          }
        });
  }

  protected ExecutorService getExecutorService() {
    return executorService_;
  }

  protected boolean preServe() {
    try {
      serverTransport_.listen();
    } catch (TTransportException ttx) {
      LOGGER.error("Error occurred during listening.", ttx);
      return false;
    }

    // Run the preServe event
    if (eventHandler_ != null) {
      eventHandler_.preServe();
    }
    stopped_ = false;
    setServing(true);
    return true;
  }

  public void serve() {
    if (!preServe()) {
      return;
    }

    execute();

    executorService_.shutdownNow();

    if (!waitForShutdown()) {
      LOGGER.error("Shutdown is not done after " + stopTimeoutVal + stopTimeoutUnit);
    }

    setServing(false);
  }

  protected void execute() {
    while (!stopped_) {
      try {
        TTransport client = serverTransport_.accept();
        try {
          executorService_.execute(new WorkerProcess(client));
        } catch (RejectedExecutionException ree) {
          if (!stopped_) {
            LOGGER.warn("ThreadPool is saturated with incoming requests. Closing latest connection.");
          }
          client.close();
        }
      } catch (TTransportException ttx) {
        if (!stopped_) {
          LOGGER.warn("Transport error occurred during acceptance of message", ttx);
        }
      }
    }
  }

  protected boolean waitForShutdown() {
    // Loop until awaitTermination finally does return without a interrupted
    // exception. If we don't do this, then we'll shut down prematurely. We want
    // to let the executorService clear it's task queue, closing client sockets
    // appropriately.
    long timeoutMS = stopTimeoutUnit.toMillis(stopTimeoutVal);
    long now = System.currentTimeMillis();
    while (timeoutMS >= 0) {
      try {
        return executorService_.awaitTermination(timeoutMS, TimeUnit.MILLISECONDS);
      } catch (InterruptedException ix) {
        long newnow = System.currentTimeMillis();
        timeoutMS -= (newnow - now);
        now = newnow;
      }
    }
    return false;
  }

  public void stop() {
    stopped_ = true;
    serverTransport_.interrupt();
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

      Optional<TServerEventHandler> eventHandler = Optional.empty();
      ServerContext connectionContext = null;

      try {
        processor = processorFactory_.getProcessor(client_);
        inputTransport = inputTransportFactory_.getTransport(client_);
        outputTransport = outputTransportFactory_.getTransport(client_);
        inputProtocol = inputProtocolFactory_.getProtocol(inputTransport);
        outputProtocol = outputProtocolFactory_.getProtocol(outputTransport);

        eventHandler = Optional.ofNullable(getEventHandler());

        if (eventHandler.isPresent()) {
          connectionContext = eventHandler.get().createContext(inputProtocol, outputProtocol);
        }

        while (true) {
          if (Thread.currentThread().isInterrupted()) {
            LOGGER.debug("WorkerProcess requested to shutdown");
            break;
          }
          if (eventHandler.isPresent()) {
            eventHandler.get().processContext(connectionContext, inputTransport, outputTransport);
          }
          // This process cannot be interrupted by Interrupting the Thread. This
          // will return once a message has been processed or the socket timeout
          // has elapsed, at which point it will return and check the interrupt
          // state of the thread.
          processor.process(inputProtocol, outputProtocol);
        }
      } catch (Exception x) {
        LOGGER.debug("Error processing request", x);

        // We'll usually receive RuntimeException types here
        // Need to unwrap to ascertain real causing exception before we choose to ignore
        // Ignore err-logging all transport-level/type exceptions
        if (!isIgnorableException(x)) {
          // Log the exception at error level and continue
          LOGGER.error((x instanceof TException ? "Thrift " : "") + "Error occurred during processing of message.", x);
        }
      } finally {
        if (eventHandler.isPresent()) {
          eventHandler.get().deleteContext(connectionContext, inputProtocol, outputProtocol);
        }
        if (inputTransport != null) {
          inputTransport.close();
        }
        if (outputTransport != null) {
          outputTransport.close();
        }
        if (client_.isOpen()) {
          client_.close();
        }
      }
    }

    private boolean isIgnorableException(Exception x) {
      TTransportException tTransportException = null;

      if (x instanceof TTransportException) {
        tTransportException = (TTransportException) x;
      } else if (x.getCause() instanceof TTransportException) {
        tTransportException = (TTransportException) x.getCause();
      }

      if (tTransportException != null) {
        switch(tTransportException.getType()) {
          case TTransportException.END_OF_FILE:
          case TTransportException.TIMED_OUT:
            return true;
        }
      }
      return false;
    }
  }
}
