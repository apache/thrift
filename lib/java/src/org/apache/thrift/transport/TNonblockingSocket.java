
package org.apache.thrift.transport;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.SocketException;

import java.nio.ByteBuffer;
import java.nio.channels.SocketChannel;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;

/**
 * Socket implementation of the TTransport interface. To be commented soon!
 */
public class TNonblockingSocket extends TNonblockingTransport {

  private SocketChannel socketChannel = null;

  /**
   * Wrapped Socket object
   */
  private Socket socket_ = null;

  /**
   * Remote host
   */
  private String host_  = null;

  /**
   * Remote port
   */
  private int port_ = 0;

  /**
   * Socket timeout
   */
  private int timeout_ = 0;

  /**
   * Constructor that takes an already created socket.
   *
   * @param socketChannel Already created SocketChannel object
   * @throws TTransportException if there is an error setting up the streams
   */
  public TNonblockingSocket(SocketChannel socketChannel) throws TTransportException {
    try {
      // make it a nonblocking channel
      socketChannel.configureBlocking(false);
    } catch (IOException e) {
      throw new TTransportException(e);
    }

    this.socketChannel = socketChannel;
    this.socket_ = socketChannel.socket();
    try {
      socket_.setSoLinger(false, 0);
      socket_.setTcpNoDelay(true);
    } catch (SocketException sx) {
      sx.printStackTrace();
    }

    // if (isOpen()) {
    //   try {
    //     // inputStream_ = new BufferedInputStream(socket_.getInputStream(), 1024);
    //     // outputStream_ = new BufferedOutputStream(socket_.getOutputStream(), 1024);
    //   } catch (IOException iox) {
    //     close();
    //     throw new TTransportException(TTransportException.NOT_OPEN, iox);
    //   }
    // }
  }

  // This is all for the clientside stuff. Not sure that we'll actually be supporting that yet.
  // /**
  //  * Creates a new unconnected socket that will connect to the given host
  //  * on the given port.
  //  *
  //  * @param host Remote host
  //  * @param port Remote port
  //  */
  // public TNonblockingSocket(String host, int port) {
  //   this(host, port, 0);
  // }
  //
  // /**
  //  * Creates a new unconnected socket that will connect to the given host
  //  * on the given port.
  //  *
  //  * @param host    Remote host
  //  * @param port    Remote port
  //  * @param timeout Socket timeout
  //  */
  // public TSocket(String host, int port, int timeout) {
  //   host_ = host;
  //   port_ = port;
  //   timeout_ = timeout;
  //   initSocket();
  // }


  /**
   * Register this socket with the specified selector for both read and write
   * operations.
   *
   * @param selector
   * @return the selection key for this socket.
   */
  public SelectionKey registerSelector(Selector selector, int interests) throws IOException {
    // Register the new SocketChannel with our Selector, indicating
    // we'd like to be notified when there's data waiting to be read
    return socketChannel.register(selector, interests);
  }

  /**
   * Initializes the socket object
   */
  private void initSocket() {
    socket_ = new Socket();
    try {
      socket_.setSoLinger(false, 0);
      socket_.setTcpNoDelay(true);
      socket_.setSoTimeout(timeout_);
    } catch (SocketException sx) {
      sx.printStackTrace();
    }
  }

  /**
   * Sets the socket timeout
   *
   * @param timeout Milliseconds timeout
   */
  public void setTimeout(int timeout) {
    timeout_ = timeout;
    try {
      socket_.setSoTimeout(timeout);
    } catch (SocketException sx) {
      sx.printStackTrace();
    }
  }

  /**
   * Returns a reference to the underlying socket.
   */
  public Socket getSocket() {
    if (socket_ == null) {
      initSocket();
    }
    return socket_;
  }

  /**
   * Checks whether the socket is connected.
   */
  public boolean isOpen() {
    if (socket_ == null) {
      return false;
    }
    return socket_.isConnected();
  }

  /**
   * Connects the socket, creating a new socket object if necessary.
   */
  public void open() throws TTransportException {
    throw new RuntimeException("Not implemented yet");
    // if (isOpen()) {
    //   throw new TTransportException(TTransportException.ALREADY_OPEN, "Socket already connected.");
    // }
    //
    // if (host_.length() == 0) {
    //   throw new TTransportException(TTransportException.NOT_OPEN, "Cannot open null host.");
    // }
    // if (port_ <= 0) {
    //   throw new TTransportException(TTransportException.NOT_OPEN, "Cannot open without port.");
    // }
    //
    // if (socket_ == null) {
    //   initSocket();
    // }
    //
    // try {
    //   socket_.connect(new InetSocketAddress(host_, port_));
    //   inputStream_ = new BufferedInputStream(socket_.getInputStream(), 1024);
    //   outputStream_ = new BufferedOutputStream(socket_.getOutputStream(), 1024);
    // } catch (IOException iox) {
    //   close();
    //   throw new TTransportException(TTransportException.NOT_OPEN, iox);
    // }
  }

  /**
   * Perform a nonblocking read into buffer.
   */
  public int read(ByteBuffer buffer) throws IOException {
    return socketChannel.read(buffer);
  }


  /**
   * Reads from the underlying input stream if not null.
   */
  public int read(byte[] buf, int off, int len) throws TTransportException {
    if ((socketChannel.validOps() & SelectionKey.OP_READ) != SelectionKey.OP_READ) {
      throw new TTransportException(TTransportException.NOT_OPEN,
        "Cannot read from write-only socket channel");
    }
    try {
      return socketChannel.read(ByteBuffer.wrap(buf, off, len));
    } catch (IOException iox) {
      throw new TTransportException(TTransportException.UNKNOWN, iox);
    }
  }

  /**
   * Perform a nonblocking write of the data in buffer;
   */
  public int write(ByteBuffer buffer) throws IOException {
    return socketChannel.write(buffer);
  }

  /**
   * Writes to the underlying output stream if not null.
   */
  public void write(byte[] buf, int off, int len) throws TTransportException {
    if ((socketChannel.validOps() & SelectionKey.OP_WRITE) != SelectionKey.OP_WRITE) {
      throw new TTransportException(TTransportException.NOT_OPEN,
        "Cannot write to write-only socket channel");
    }
    try {
      socketChannel.write(ByteBuffer.wrap(buf, off, len));
    } catch (IOException iox) {
      throw new TTransportException(TTransportException.UNKNOWN, iox);
    }
  }

  /**
   * Flushes the underlying output stream if not null.
   */
  public void flush() throws TTransportException {
    // Not supported by SocketChannel.
  }

  /**
   * Closes the socket.
   */
  public void close() {
    try {
      socketChannel.close();
    } catch (IOException e) {
      // silently ignore.
    }
  }

}
