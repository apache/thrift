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

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.spi.SelectorProvider;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import org.apache.thrift.TByteArrayOutputStream;
import org.apache.thrift.TException;
import org.apache.thrift.TProcessor;
import org.apache.thrift.TProcessorFactory;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.protocol.TProtocolFactory;
import org.apache.thrift.transport.TFramedTransport;
import org.apache.thrift.transport.TIOStreamTransport;
import org.apache.thrift.transport.TNonblockingServerTransport;
import org.apache.thrift.transport.TNonblockingTransport;
import org.apache.thrift.transport.TTransport;
import org.apache.thrift.transport.TTransportException;

/**
 * A nonblocking TServer implementation. This allows for fairness amongst all
 * connected clients in terms of invocations.
 *
 * This server is inherently single-threaded. If you want a limited thread pool
 * coupled with invocation-fairness, see THsHaServer.
 *
 * To use this server, you MUST use a TFramedTransport at the outermost
 * transport, otherwise this server will be unable to determine when a whole
 * method call has been read off the wire. Clients must also use TFramedTransport.
 */
public class TNonblockingServer extends TServer {
  private static final Logger LOGGER =
    LoggerFactory.getLogger(TNonblockingServer.class.getName());

  // Flag for stopping the server
  private volatile boolean stopped_;

  private SelectThread selectThread_;

  /**
   * The maximum amount of memory we will allocate to client IO buffers at a
   * time. Without this limit, the server will gladly allocate client buffers
   * right into an out of memory exception, rather than waiting.
   */
  private final long MAX_READ_BUFFER_BYTES;

  protected final Options options_;

  /**
   * How many bytes are currently allocated to read buffers.
   */
  private long readBufferBytesAllocated = 0;

  /**
   * Create server with given processor and server transport, using
   * TBinaryProtocol for the protocol, TFramedTransport.Factory on both input
   * and output transports. A TProcessorFactory will be created that always
   * returns the specified processor.
   */
  public TNonblockingServer(TProcessor processor,
                           TNonblockingServerTransport serverTransport) {
    this(new TProcessorFactory(processor), serverTransport);
  }

  /**
   * Create server with specified processor factory and server transport.
   * TBinaryProtocol is assumed. TFramedTransport.Factory is used on both input
   * and output transports.
   */
  public TNonblockingServer(TProcessorFactory processorFactory,
                            TNonblockingServerTransport serverTransport) {
    this(processorFactory, serverTransport,
         new TFramedTransport.Factory(), new TFramedTransport.Factory(),
         new TBinaryProtocol.Factory(), new TBinaryProtocol.Factory());
  }

  public TNonblockingServer(TProcessor processor,
                            TNonblockingServerTransport serverTransport,
                            TProtocolFactory protocolFactory) {
    this(processor, serverTransport,
         new TFramedTransport.Factory(), new TFramedTransport.Factory(),
         protocolFactory, protocolFactory);
  }

  public TNonblockingServer(TProcessor processor,
                            TNonblockingServerTransport serverTransport,
                            TFramedTransport.Factory transportFactory,
                            TProtocolFactory protocolFactory) {
    this(processor, serverTransport,
         transportFactory, transportFactory,
         protocolFactory, protocolFactory);
  }

  public TNonblockingServer(TProcessorFactory processorFactory,
                            TNonblockingServerTransport serverTransport,
                            TFramedTransport.Factory transportFactory,
                            TProtocolFactory protocolFactory) {
    this(processorFactory, serverTransport,
         transportFactory, transportFactory,
         protocolFactory, protocolFactory);
  }

  public TNonblockingServer(TProcessor processor,
                            TNonblockingServerTransport serverTransport,
                            TFramedTransport.Factory inputTransportFactory,
                            TFramedTransport.Factory outputTransportFactory,
                            TProtocolFactory inputProtocolFactory,
                            TProtocolFactory outputProtocolFactory) {
    this(new TProcessorFactory(processor), serverTransport,
         inputTransportFactory, outputTransportFactory,
         inputProtocolFactory, outputProtocolFactory);
  }

  public TNonblockingServer(TProcessorFactory processorFactory,
                            TNonblockingServerTransport serverTransport,
                            TFramedTransport.Factory inputTransportFactory,
                            TFramedTransport.Factory outputTransportFactory,
                            TProtocolFactory inputProtocolFactory,
                            TProtocolFactory outputProtocolFactory) {
    this(processorFactory, serverTransport,
          inputTransportFactory, outputTransportFactory,
          inputProtocolFactory, outputProtocolFactory,
          new Options());
  }

  public TNonblockingServer(TProcessorFactory processorFactory,
                            TNonblockingServerTransport serverTransport,
                            TFramedTransport.Factory inputTransportFactory,
                            TFramedTransport.Factory outputTransportFactory,
                            TProtocolFactory inputProtocolFactory,
                            TProtocolFactory outputProtocolFactory,
                            Options options) {
    super(processorFactory, serverTransport,
          inputTransportFactory, outputTransportFactory,
          inputProtocolFactory, outputProtocolFactory);
    options_ = options;
    options_.validate();
    MAX_READ_BUFFER_BYTES = options.maxReadBufferBytes;
  }

  /**
   * Begin accepting connections and processing invocations.
   */
  public void serve() {
    // start listening, or exit
    if (!startListening()) {
      return;
    }

    // start the selector, or exit
    if (!startSelectorThread()) {
      return;
    }

    // this will block while we serve
    joinSelector();

    // do a little cleanup
    stopListening();
  }

  /**
   * Have the server transport start accepting connections.
   *
   * @return true if we started listening successfully, false if something went
   * wrong.
   */
  protected boolean startListening() {
    try {
      serverTransport_.listen();
      return true;
    } catch (TTransportException ttx) {
      LOGGER.error("Failed to start listening on server socket!", ttx);
      return false;
    }
  }

  /**
   * Stop listening for conections.
   */
  protected void stopListening() {
    serverTransport_.close();
  }

  /**
   * Start the selector thread running to deal with clients.
   *
   * @return true if everything went ok, false if we couldn't start for some
   * reason.
   */
  protected boolean startSelectorThread() {
    // start the selector
    try {
      selectThread_ = new SelectThread((TNonblockingServerTransport)serverTransport_);
      selectThread_.start();
      return true;
    } catch (IOException e) {
      LOGGER.error("Failed to start selector thread!", e);
      return false;
    }
  }

  /**
   * Block until the selector exits.
   */
  protected void joinSelector() {
    // wait until the selector thread exits
    try {
      selectThread_.join();
    } catch (InterruptedException e) {
      // for now, just silently ignore. technically this means we'll have less of
      // a graceful shutdown as a result.
    }
  }

  /**
   * Stop serving and shut everything down.
   */
  public void stop() {
    stopped_ = true;
    if (selectThread_ != null) {
      selectThread_.wakeupSelector();
    }
  }

  /**
   * Perform an invocation. This method could behave several different ways
   * - invoke immediately inline, queue for separate execution, etc.
   */
  protected void requestInvoke(FrameBuffer frameBuffer) {
    frameBuffer.invoke();
  }

  /**
   * A FrameBuffer wants to change its selection preferences, but might not be
   * in the select thread.
   */
  protected void requestSelectInterestChange(FrameBuffer frameBuffer) {
    selectThread_.requestSelectInterestChange(frameBuffer);
  }

  /**
   * The thread that will be doing all the selecting, managing new connections
   * and those that still need to be read.
   */
  protected class SelectThread extends Thread {

    private final TNonblockingServerTransport serverTransport;
    private final Selector selector;

    // List of FrameBuffers that want to change their selection interests.
    private final Set<FrameBuffer> selectInterestChanges =
      new HashSet<FrameBuffer>();

    /**
     * Set up the SelectorThread.
     */
    public SelectThread(final TNonblockingServerTransport serverTransport)
    throws IOException {
      this.serverTransport = serverTransport;
      this.selector = SelectorProvider.provider().openSelector();
      serverTransport.registerSelector(selector);
    }

    /**
     * The work loop. Handles both selecting (all IO operations) and managing
     * the selection preferences of all existing connections.
     */
    public void run() {
      while (!stopped_) {
        select();
        processInterestChanges();
      }
    }

    /**
     * If the selector is blocked, wake it up.
     */
    public void wakeupSelector() {
      selector.wakeup();
    }

    /**
     * Add FrameBuffer to the list of select interest changes and wake up the
     * selector if it's blocked. When the select() call exits, it'll give the
     * FrameBuffer a chance to change its interests.
     */
    public void requestSelectInterestChange(FrameBuffer frameBuffer) {
      synchronized (selectInterestChanges) {
        selectInterestChanges.add(frameBuffer);
      }
      // wakeup the selector, if it's currently blocked.
      selector.wakeup();
    }

    /**
     * Select and process IO events appropriately:
     * If there are connections to be accepted, accept them.
     * If there are existing connections with data waiting to be read, read it,
     * bufferring until a whole frame has been read.
     * If there are any pending responses, buffer them until their target client
     * is available, and then send the data.
     */
    private void select() {
      try {
        // wait for io events.
        selector.select();

        // process the io events we received
        Iterator<SelectionKey> selectedKeys = selector.selectedKeys().iterator();
        while (!stopped_ && selectedKeys.hasNext()) {
          SelectionKey key = selectedKeys.next();
          selectedKeys.remove();

          // skip if not valid
          if (!key.isValid()) {
            cleanupSelectionkey(key);
            continue;
          }

          // if the key is marked Accept, then it has to be the server
          // transport.
          if (key.isAcceptable()) {
            handleAccept();
          } else if (key.isReadable()) {
            // deal with reads
            handleRead(key);
          } else if (key.isWritable()) {
            // deal with writes
            handleWrite(key);
          } else {
            LOGGER.warn("Unexpected state in select! " + key.interestOps());
          }
        }
      } catch (IOException e) {
        LOGGER.warn("Got an IOException while selecting!", e);
      }
    }

    /**
     * Check to see if there are any FrameBuffers that have switched their
     * interest type from read to write or vice versa.
     */
    private void processInterestChanges() {
      synchronized (selectInterestChanges) {
        for (FrameBuffer fb : selectInterestChanges) {
          fb.changeSelectInterests();
        }
        selectInterestChanges.clear();
      }
    }

    /**
     * Accept a new connection.
     */
    private void handleAccept() throws IOException {
      SelectionKey clientKey = null;
      TNonblockingTransport client = null;
      try {
        // accept the connection
        client = (TNonblockingTransport)serverTransport.accept();
        clientKey = client.registerSelector(selector, SelectionKey.OP_READ);

        // add this key to the map
        FrameBuffer frameBuffer = new FrameBuffer(client, clientKey);
        clientKey.attach(frameBuffer);
      } catch (TTransportException tte) {
        // something went wrong accepting.
        LOGGER.warn("Exception trying to accept!", tte);
        tte.printStackTrace();
        if (clientKey != null) cleanupSelectionkey(clientKey);
        if (client != null) client.close();
      }
    }

    /**
     * Do the work required to read from a readable client. If the frame is
     * fully read, then invoke the method call.
     */
    private void handleRead(SelectionKey key) {
      FrameBuffer buffer = (FrameBuffer)key.attachment();
      if (buffer.read()) {
        // if the buffer's frame read is complete, invoke the method.
        if (buffer.isFrameFullyRead()) {
          requestInvoke(buffer);
        }
      } else {
        cleanupSelectionkey(key);
      }
    }

    /**
     * Let a writable client get written, if there's data to be written.
     */
    private void handleWrite(SelectionKey key) {
      FrameBuffer buffer = (FrameBuffer)key.attachment();
      if (!buffer.write()) {
        cleanupSelectionkey(key);
      }
    }

    /**
     * Do connection-close cleanup on a given SelectionKey.
     */
    private void cleanupSelectionkey(SelectionKey key) {
      // remove the records from the two maps
      FrameBuffer buffer = (FrameBuffer)key.attachment();
      if (buffer != null) {
        // close the buffer
        buffer.close();
      }
      // cancel the selection key
      key.cancel();
    }
  } // SelectorThread

  /**
   * Class that implements a sort of state machine around the interaction with
   * a client and an invoker. It manages reading the frame size and frame data,
   * getting it handed off as wrapped transports, and then the writing of
   * reponse data back to the client. In the process it manages flipping the
   * read and write bits on the selection key for its client.
   */
  protected class FrameBuffer {
    //
    // Possible states for the FrameBuffer state machine.
    //
    // in the midst of reading the frame size off the wire
    private static final int READING_FRAME_SIZE = 1;
    // reading the actual frame data now, but not all the way done yet
    private static final int READING_FRAME = 2;
    // completely read the frame, so an invocation can now happen
    private static final int READ_FRAME_COMPLETE = 3;
    // waiting to get switched to listening for write events
    private static final int AWAITING_REGISTER_WRITE = 4;
    // started writing response data, not fully complete yet
    private static final int WRITING = 6;
    // another thread wants this framebuffer to go back to reading
    private static final int AWAITING_REGISTER_READ = 7;
    // we want our transport and selection key invalidated in the selector thread
    private static final int AWAITING_CLOSE = 8;

    //
    // Instance variables
    //

    // the actual transport hooked up to the client.
    private final TNonblockingTransport trans_;

    // the SelectionKey that corresponds to our transport
    private final SelectionKey selectionKey_;

    // where in the process of reading/writing are we?
    private int state_ = READING_FRAME_SIZE;

    // the ByteBuffer we'll be using to write and read, depending on the state
    private ByteBuffer buffer_;

    private TByteArrayOutputStream response_;

    public FrameBuffer( final TNonblockingTransport trans,
                        final SelectionKey selectionKey) {
      trans_ = trans;
      selectionKey_ = selectionKey;
      buffer_ = ByteBuffer.allocate(4);
    }

    /**
     * Give this FrameBuffer a chance to read. The selector loop should have
     * received a read event for this FrameBuffer.
     *
     * @return true if the connection should live on, false if it should be
     * closed
     */
    public boolean read() {
      if (state_ == READING_FRAME_SIZE) {
        // try to read the frame size completely
        if (!internalRead()) {
          return false;
        }

        // if the frame size has been read completely, then prepare to read the
        // actual frame.
        if (buffer_.remaining() == 0) {
          // pull out the frame size as an integer.
          int frameSize = buffer_.getInt(0);
          if (frameSize <= 0) {
            LOGGER.error("Read an invalid frame size of " + frameSize
              + ". Are you using TFramedTransport on the client side?");
            return false;
          }

          // if this frame will always be too large for this server, log the
          // error and close the connection.
          if (frameSize + 4 > MAX_READ_BUFFER_BYTES) {
            LOGGER.error("Read a frame size of " + frameSize
              + ", which is bigger than the maximum allowable buffer size for ALL connections.");
            return false;
          }

          // if this frame will push us over the memory limit, then return.
          // with luck, more memory will free up the next time around.
          if (readBufferBytesAllocated + frameSize + 4 > MAX_READ_BUFFER_BYTES) {
            return true;
          }

          // incremement the amount of memory allocated to read buffers
          readBufferBytesAllocated += frameSize + 4;

          // reallocate the readbuffer as a frame-sized buffer
          buffer_ = ByteBuffer.allocate(frameSize + 4);
          // put the frame size at the head of the buffer
          buffer_.putInt(frameSize);

          state_ = READING_FRAME;
        } else {
          // this skips the check of READING_FRAME state below, since we can't
          // possibly go on to that state if there's data left to be read at
          // this one.
          return true;
        }
      }

      // it is possible to fall through from the READING_FRAME_SIZE section
      // to READING_FRAME if there's already some frame data available once
      // READING_FRAME_SIZE is complete.

      if (state_ == READING_FRAME) {
        if (!internalRead()) {
          return false;
        }

        // since we're already in the select loop here for sure, we can just
        // modify our selection key directly.
        if (buffer_.remaining() == 0) {
          // get rid of the read select interests
          selectionKey_.interestOps(0);
          state_ = READ_FRAME_COMPLETE;
        }

        return true;
      }

      // if we fall through to this point, then the state must be invalid.
      LOGGER.error("Read was called but state is invalid (" + state_ + ")");
      return false;
    }

    /**
     * Give this FrameBuffer a chance to write its output to the final client.
     */
    public boolean write() {
      if (state_ == WRITING) {
        try {
          if (trans_.write(buffer_) < 0) {
            return false;
          }
        } catch (IOException e) {
          LOGGER.warn("Got an IOException during write!", e);
          return false;
        }

        // we're done writing. now we need to switch back to reading.
        if (buffer_.remaining() == 0) {
          prepareRead();
        }
        return true;
      }

      LOGGER.error("Write was called, but state is invalid (" + state_ + ")");
      return false;
    }

    /**
     * Give this FrameBuffer a chance to set its interest to write, once data
     * has come in.
     */
    public void changeSelectInterests() {
      if (state_ == AWAITING_REGISTER_WRITE) {
        // set the OP_WRITE interest
        selectionKey_.interestOps(SelectionKey.OP_WRITE);
        state_ = WRITING;
      } else if (state_ == AWAITING_REGISTER_READ) {
        prepareRead();
      } else if (state_ == AWAITING_CLOSE){
        close();
        selectionKey_.cancel();
      } else {
        LOGGER.error(
          "changeSelectInterest was called, but state is invalid ("
          + state_ + ")");
      }
    }

    /**
     * Shut the connection down.
     */
    public void close() {
      // if we're being closed due to an error, we might have allocated a
      // buffer that we need to subtract for our memory accounting.
      if (state_ == READING_FRAME || state_ == READ_FRAME_COMPLETE) {
        readBufferBytesAllocated -= buffer_.array().length;
      }
      trans_.close();
    }

    /**
     * Check if this FrameBuffer has a full frame read.
     */
    public boolean isFrameFullyRead() {
      return state_ == READ_FRAME_COMPLETE;
    }

    /**
     * After the processor has processed the invocation, whatever thread is
     * managing invocations should call this method on this FrameBuffer so we
     * know it's time to start trying to write again. Also, if it turns out
     * that there actually isn't any data in the response buffer, we'll skip
     * trying to write and instead go back to reading.
     */
    public void responseReady() {
      // the read buffer is definitely no longer in use, so we will decrement
      // our read buffer count. we do this here as well as in close because
      // we'd like to free this read memory up as quickly as possible for other
      // clients.
      readBufferBytesAllocated -= buffer_.array().length;

      if (response_.len() == 0) {
        // go straight to reading again. this was probably an oneway method
        state_ = AWAITING_REGISTER_READ;
        buffer_ = null;
      } else {
        buffer_ = ByteBuffer.wrap(response_.get(), 0, response_.len());

        // set state that we're waiting to be switched to write. we do this
        // asynchronously through requestSelectInterestChange() because there is a
        // possibility that we're not in the main thread, and thus currently
        // blocked in select(). (this functionality is in place for the sake of
        // the HsHa server.)
        state_ = AWAITING_REGISTER_WRITE;
      }
      requestSelectInterestChange();
    }

    /**
     * Actually invoke the method signified by this FrameBuffer.
     */
    public void invoke() {
      TTransport inTrans = getInputTransport();
      TProtocol inProt = inputProtocolFactory_.getProtocol(inTrans);
      TProtocol outProt = outputProtocolFactory_.getProtocol(getOutputTransport());

      try {
        processorFactory_.getProcessor(inTrans).process(inProt, outProt);
        responseReady();
        return;
      } catch (TException te) {
        LOGGER.warn("Exception while invoking!", te);
      } catch (Exception e) {
        LOGGER.error("Unexpected exception while invoking!", e);
      }
      // This will only be reached when there is an exception.
      state_ = AWAITING_CLOSE;
      requestSelectInterestChange();
    }

    /**
     * Wrap the read buffer in a memory-based transport so a processor can read
     * the data it needs to handle an invocation.
     */
    private TTransport getInputTransport() {
      return inputTransportFactory_.getTransport(new TIOStreamTransport(
        new ByteArrayInputStream(buffer_.array())));
    }

    /**
     * Get the transport that should be used by the invoker for responding.
     */
    private TTransport getOutputTransport() {
      response_ = new TByteArrayOutputStream();
      return outputTransportFactory_.getTransport(new TIOStreamTransport(response_));
    }

    /**
     * Perform a read into buffer.
     *
     * @return true if the read succeeded, false if there was an error or the
     * connection closed.
     */
    private boolean internalRead() {
      try {
        if (trans_.read(buffer_) < 0) {
          return false;
        }
        return true;
      } catch (IOException e) {
        LOGGER.warn("Got an IOException in internalRead!", e);
        return false;
      }
    }

    /**
     * We're done writing, so reset our interest ops and change state accordingly.
     */
    private void prepareRead() {
      // we can set our interest directly without using the queue because
      // we're in the select thread.
      selectionKey_.interestOps(SelectionKey.OP_READ);
      // get ready for another go-around
      buffer_ = ByteBuffer.allocate(4);
      state_ = READING_FRAME_SIZE;
    }

    /**
     * When this FrameBuffer needs to change it's select interests and execution
     * might not be in the select thread, then this method will make sure the
     * interest change gets done when the select thread wakes back up. When the
     * current thread is the select thread, then it just does the interest change
     * immediately.
     */
    private void requestSelectInterestChange() {
      if (Thread.currentThread() == selectThread_) {
        changeSelectInterests();
      } else {
        TNonblockingServer.this.requestSelectInterestChange(this);
      }
    }
  } // FrameBuffer


  public static class Options {
    public long maxReadBufferBytes = Long.MAX_VALUE;

    public Options() {}

    public void validate() {
      if (maxReadBufferBytes <= 1024) {
        throw new IllegalArgumentException("You must allocate at least 1KB to the read buffer.");
      }
    }
  }
}
