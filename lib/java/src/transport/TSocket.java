package com.facebook.thrift.transport;

import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.Socket;
import java.net.SocketException;

/**
 * Socket implementation of the TTransport interface. To be commented soon!
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public class TSocket extends TIOStreamTransport {

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
   * @param socket Already created socket object
   * @throws TTransportException if there is an error setting up the streams
   */
  public TSocket(Socket socket) throws TTransportException {
    socket_ = socket;
   
    if (isOpen()) {
      try {
        inputStream_ = new BufferedInputStream(socket_.getInputStream(), 1024);
        outputStream_ = new BufferedOutputStream(socket_.getOutputStream(), 1024);
      } catch (IOException iox) {
        close();
        throw new TTransportException(iox);
      }
    }
  }

  /**
   * Creates a new unconnected socket that will connect to the given host
   * on the given port.
   *
   * @param host Remote host
   * @param port Remote port
   */
  public TSocket(String host, int port) {
    this(host, port, 500);
  }

  /**
   * Creates a new unconnected socket that will connect to the given host
   * on the given port.
   *
   * @param host    Remote host
   * @param port    Remote port
   * @param timeout Socket timeout
   */
  public TSocket(String host, int port, int timeout) {
    socket_ = new Socket();
    host_ = host;
    port_ = port;
    timeout_ = timeout;
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
   * Returns a reference to the underlying socket. Can be used to set
   * socket options, etc. If an underlying socket does not exist yet, this
   * will create one.
   */
  public Socket getSocket() {
    if (socket_ == null) {
      socket_ = new Socket();
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
    if (socket_ == null) {
      if (host_.length() == 0) {
        throw new TTransportException("Cannot open null host.");
      }
      if (port_ <= 0) {
        throw new TTransportException("Cannot open without port.");
      }
      socket_ = new Socket();
    }

    if (isOpen()) {
      throw new TTransportException("Socket already connected.");
    }

    try {
      socket_.connect(new InetSocketAddress(host_, port_));
      inputStream_ = new BufferedInputStream(socket_.getInputStream(), 1024);
      outputStream_ = new BufferedOutputStream(socket_.getOutputStream(), 1024);
    } catch (IOException iox) {
      close();
      throw new TTransportException(iox);
    }
  }

  /**
   * Closes the socket.
   */
  public void close() {
    // Close the underlying streams
    super.close();

    // Close the socket
    if (socket_ != null) {
      try {
        socket_.close();
      } catch (IOException iox) {
        System.err.println("WARNING: exception closing socket: " +
                           iox.getMessage());
      }
      socket_ = null;
    }
  }

}
