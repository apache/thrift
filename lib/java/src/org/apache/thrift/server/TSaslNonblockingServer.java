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

import java.io.IOException;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.BlockingQueue;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.LinkedBlockingQueue;
import java.util.concurrent.TimeUnit;

import javax.security.auth.callback.CallbackHandler;

import org.apache.thrift.TProcessor;
import org.apache.thrift.transport.TNonblockingServerSocket;
import org.apache.thrift.transport.TNonblockingServerTransport;
import org.apache.thrift.transport.TNonblockingTransport;
import org.apache.thrift.transport.TTransportException;
import org.apache.thrift.transport.sasl.NonblockingSaslHandler;
import org.apache.thrift.transport.sasl.NonblockingSaslHandler.Phase;
import org.apache.thrift.transport.sasl.TBaseSaslProcessorFactory;
import org.apache.thrift.transport.sasl.TSaslProcessorFactory;
import org.apache.thrift.transport.sasl.TSaslServerFactory;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * TServer with sasl support, using asynchronous execution and nonblocking io.
 */
public class TSaslNonblockingServer extends TServer {
  private static final Logger LOGGER = LoggerFactory.getLogger(TSaslNonblockingServer.class);

  private static final int DEFAULT_NETWORK_THREADS = 1;
  private static final int DEFAULT_AUTHENTICATION_THREADS = 1;
  private static final int DEFAULT_PROCESSING_THREADS = Runtime.getRuntime().availableProcessors();

  private final AcceptorThread acceptor;
  private final NetworkThreadPool networkThreadPool;
  private final ExecutorService authenticationExecutor;
  private final ExecutorService processingExecutor;
  private final TSaslServerFactory saslServerFactory;
  private final TSaslProcessorFactory saslProcessorFactory;

  public TSaslNonblockingServer(Args args) throws IOException {
    super(args);
    acceptor = new AcceptorThread((TNonblockingServerSocket) serverTransport_);
    networkThreadPool = new NetworkThreadPool(args.networkThreads);
    authenticationExecutor = Executors.newFixedThreadPool(args.saslThreads);
    processingExecutor = Executors.newFixedThreadPool(args.processingThreads);
    saslServerFactory = args.saslServerFactory;
    saslProcessorFactory = args.saslProcessorFactory;
  }

  @Override
  public void serve() {
    if (eventHandler_ != null) {
      eventHandler_.preServe();
    }
    networkThreadPool.start();
    acceptor.start();
    setServing(true);
  }

  /**
   * Trigger a graceful shutdown, but it does not block to wait for the shutdown to finish.
   */
  @Override
  public void stop() {
    if (!stopped_) {
      setServing(false);
      stopped_ = true;
      acceptor.wakeup();
      networkThreadPool.wakeupAll();
      authenticationExecutor.shutdownNow();
      processingExecutor.shutdownNow();
    }
  }

  /**
   * Gracefully shut down the server and block until all threads are stopped.
   *
   * @throws InterruptedException if is interrupted while waiting for shutdown.
   */
  public void shutdown() throws InterruptedException {
    stop();
    acceptor.join();
    for (NetworkThread networkThread : networkThreadPool.networkThreads) {
      networkThread.join();
    }
    while (!authenticationExecutor.isTerminated()) {
      authenticationExecutor.awaitTermination(10, TimeUnit.SECONDS);
    }
    while (!processingExecutor.isTerminated()) {
      processingExecutor.awaitTermination(10, TimeUnit.SECONDS);
    }
  }

  private class AcceptorThread extends Thread {

    private final TNonblockingServerTransport serverTransport;
    private final Selector acceptSelector;

    private AcceptorThread(TNonblockingServerSocket serverTransport) throws IOException {
      super("acceptor-thread");
      this.serverTransport = serverTransport;
      acceptSelector = Selector.open();
      serverTransport.registerSelector(acceptSelector);
    }

    @Override
    public void run() {
      try {
        serverTransport.listen();
        while (!stopped_) {
          select();
          acceptNewConnection();
        }
      } catch (TTransportException e) {
        // Failed to listen.
        LOGGER.error("Failed to listen on server socket, error " + e.getType(), e);
      } catch (Throwable e) {
        // Unexpected errors.
        LOGGER.error("Unexpected error in acceptor thread.", e);
      } finally {
        TSaslNonblockingServer.this.stop();
        close();
      }
    }

    void wakeup() {
      acceptSelector.wakeup();
    }

    private void acceptNewConnection() {
      Iterator<SelectionKey> selectedKeyItr = acceptSelector.selectedKeys().iterator();
      while (!stopped_ && selectedKeyItr.hasNext()) {
        SelectionKey selected = selectedKeyItr.next();
        selectedKeyItr.remove();
        if (selected.isAcceptable()) {
          try {
            while (true) {
              // Accept all available connections from the backlog.
              TNonblockingTransport connection = serverTransport.accept();
              if (connection == null) {
                break;
              }
              if (!networkThreadPool.acceptNewConnection(connection)) {
                LOGGER.error("Network thread does not accept: " + connection);
                connection.close();
              }
            }
          } catch (TTransportException e) {
            LOGGER.warn("Failed to accept incoming connection.", e);
          }
        } else {
          LOGGER.error("Not acceptable selection: " + selected.channel());
        }
      }
    }

    private void select() {
      try {
        acceptSelector.select();
      } catch (IOException e) {
        LOGGER.error("Failed to select on the server socket.", e);
      }
    }

    private void close() {
      LOGGER.info("Closing acceptor thread.");
      serverTransport.close();
      try {
        acceptSelector.close();
      } catch (IOException e) {
        LOGGER.error("Failed to close accept selector.", e);
      }
    }
  }

  private class NetworkThread extends Thread {
    private final BlockingQueue<TNonblockingTransport> incomingConnections = new LinkedBlockingQueue<>();
    private final BlockingQueue<NonblockingSaslHandler> stateTransitions = new LinkedBlockingQueue<>();
    private final Selector ioSelector;

    NetworkThread(String name) throws IOException {
      super(name);
      ioSelector = Selector.open();
    }

    @Override
    public void run() {
      try {
        while (!stopped_) {
          handleIncomingConnections();
          handleStateChanges();
          select();
          handleIO();
        }
      } catch (Throwable e) {
        LOGGER.error("Unreoverable error in " + getName(), e);
      } finally {
        close();
      }
    }

    private void handleStateChanges() {
      while (true) {
        NonblockingSaslHandler statemachine = stateTransitions.poll();
        if (statemachine == null) {
          return;
        }
        tryRunNextPhase(statemachine);
      }
    }

    private void select() {
      try {
        ioSelector.select();
      } catch (IOException e) {
        LOGGER.error("Failed to select in " + getName(), e);
      }
    }

    private void handleIO() {
      Iterator<SelectionKey> selectedKeyItr = ioSelector.selectedKeys().iterator();
      while (!stopped_ && selectedKeyItr.hasNext()) {
        SelectionKey selected = selectedKeyItr.next();
        selectedKeyItr.remove();
        if (!selected.isValid()) {
          closeChannel(selected);
        }
        NonblockingSaslHandler saslHandler = (NonblockingSaslHandler) selected.attachment();
        if (selected.isReadable()) {
          saslHandler.handleRead();
        } else if (selected.isWritable()) {
          saslHandler.handleWrite();
        } else {
          LOGGER.error("Invalid intrest op " + selected.interestOps());
          closeChannel(selected);
          continue;
        }
        if (saslHandler.isCurrentPhaseDone()) {
          tryRunNextPhase(saslHandler);
        }
      }
    }

    // The following methods are modifying the registered channel set on the selector, which itself
    // is not thread safe. Thus we need a lock to protect it from race condition.

    private synchronized void handleIncomingConnections() {
      while (true) {
        TNonblockingTransport connection = incomingConnections.poll();
        if (connection == null) {
          return;
        }
        if (!connection.isOpen()) {
          LOGGER.warn("Incoming connection is already closed");
          continue;
        }
        try {
          SelectionKey selectionKey = connection.registerSelector(ioSelector, SelectionKey.OP_READ);
          if (selectionKey.isValid()) {
            NonblockingSaslHandler saslHandler = new NonblockingSaslHandler(selectionKey, connection,
                saslServerFactory, saslProcessorFactory, inputProtocolFactory_, outputProtocolFactory_,
                eventHandler_);
            selectionKey.attach(saslHandler);
          }
        } catch (IOException e) {
          LOGGER.error("Failed to register connection for the selector, close it.", e);
          connection.close();
        }
      }
    }

    private synchronized void close() {
      LOGGER.warn("Closing " + getName());
      while (true) {
        TNonblockingTransport incomingConnection = incomingConnections.poll();
        if (incomingConnection == null) {
          break;
        }
        incomingConnection.close();
      }
      Set<SelectionKey> registered = ioSelector.keys();
      for (SelectionKey selection : registered) {
        closeChannel(selection);
      }
      try {
        ioSelector.close();
      } catch (IOException e) {
        LOGGER.error("Failed to close io selector " + getName(), e);
      }
    }

    private synchronized void closeChannel(SelectionKey selectionKey) {
      if (selectionKey.attachment() == null) {
        try {
          selectionKey.channel().close();
        } catch (IOException e) {
          LOGGER.error("Failed to close channel.", e);
        } finally {
          selectionKey.cancel();
        }
      } else {
        NonblockingSaslHandler saslHandler = (NonblockingSaslHandler) selectionKey.attachment();
        saslHandler.close();
      }
    }

    private void tryRunNextPhase(NonblockingSaslHandler saslHandler) {
      Phase nextPhase = saslHandler.getNextPhase();
      saslHandler.stepToNextPhase();
      switch (nextPhase) {
        case EVALUATING_SASL_RESPONSE:
          authenticationExecutor.submit(new Computation(saslHandler));
          break;
        case PROCESSING:
          processingExecutor.submit(new Computation(saslHandler));
          break;
        case CLOSING:
          saslHandler.runCurrentPhase();
          break;
        default: // waiting for next io event for the current state machine
      }
    }

    public boolean accept(TNonblockingTransport connection) {
      if (stopped_) {
        return false;
      }
      if (incomingConnections.offer(connection)) {
        wakeup();
        return true;
      }
      return false;
    }

    private void wakeup() {
      ioSelector.wakeup();
    }

    private class Computation implements Runnable {

      private final NonblockingSaslHandler statemachine;

      private Computation(NonblockingSaslHandler statemachine) {
        this.statemachine = statemachine;
      }

      @Override
      public void run() {
        try {
          while (!statemachine.isCurrentPhaseDone()) {
            statemachine.runCurrentPhase();
          }
          stateTransitions.add(statemachine);
          wakeup();
        } catch (Throwable e) {
          LOGGER.error("Damn it!", e);
        }
      }
    }
  }

  private class NetworkThreadPool {
    private final List<NetworkThread> networkThreads;
    private int accepted = 0;

    NetworkThreadPool(int size) throws IOException {
      networkThreads = new ArrayList<>(size);
      int digits = (int) Math.log10(size) + 1;
      String threadNamePattern = "network-thread-%0" + digits + "d";
      for (int i = 0; i < size; i++) {
        networkThreads.add(new NetworkThread(String.format(threadNamePattern, i)));
      }
    }

    /**
     * Round robin new connection among all the network threads.
     *
     * @param connection incoming connection.
     * @return true if the incoming connection is accepted by network thread pool.
     */
    boolean acceptNewConnection(TNonblockingTransport connection) {
      return networkThreads.get((accepted ++) % networkThreads.size()).accept(connection);
    }

    public void start() {
      for (NetworkThread thread : networkThreads) {
        thread.start();
      }
    }

    void wakeupAll() {
      for (NetworkThread networkThread : networkThreads) {
        networkThread.wakeup();
      }
    }
  }

  public static class Args extends AbstractServerArgs<Args> {

    private int networkThreads = DEFAULT_NETWORK_THREADS;
    private int saslThreads = DEFAULT_AUTHENTICATION_THREADS;
    private int processingThreads = DEFAULT_PROCESSING_THREADS;
    private TSaslServerFactory saslServerFactory = new TSaslServerFactory();
    private TSaslProcessorFactory saslProcessorFactory;

    public Args(TNonblockingServerTransport transport) {
      super(transport);
    }

    public Args networkThreads(int networkThreads) {
      this.networkThreads = networkThreads <= 0 ? DEFAULT_NETWORK_THREADS : networkThreads;
      return this;
    }

    public Args saslThreads(int authenticationThreads) {
      this.saslThreads = authenticationThreads <= 0 ? DEFAULT_AUTHENTICATION_THREADS : authenticationThreads;
      return this;
    }

    public Args processingThreads(int processingThreads) {
      this.processingThreads = processingThreads <= 0 ? DEFAULT_PROCESSING_THREADS : processingThreads;
      return this;
    }

    public Args processor(TProcessor processor) {
      saslProcessorFactory = new TBaseSaslProcessorFactory(processor);
      return this;
    }

    public Args saslProcessorFactory(TSaslProcessorFactory saslProcessorFactory) {
      if (saslProcessorFactory == null) {
        throw new NullPointerException("Processor factory cannot be null");
      }
      this.saslProcessorFactory = saslProcessorFactory;
      return this;
    }

    public Args addSaslMechanism(String mechanism, String protocol, String serverName,
                                 Map<String, String> props, CallbackHandler cbh) {
      saslServerFactory.addSaslMechanism(mechanism, protocol, serverName, props, cbh);
      return this;
    }

    public Args saslServerFactory(TSaslServerFactory saslServerFactory) {
      if (saslServerFactory == null) {
        throw new NullPointerException("saslServerFactory cannot be null");
      }
      this.saslServerFactory = saslServerFactory;
      return this;
    }
  }
}
