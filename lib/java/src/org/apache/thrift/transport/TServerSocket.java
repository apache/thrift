// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

package org.apache.thrift.transport;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 * Wrapper around ServerSocket for Thrift.
 *
 */
public class TServerSocket extends TServerTransport {

  private static final Logger LOGGER = Logger.getLogger(TServerSocket.class.getName());

  /**
   * Underlying serversocket object
   */
  private ServerSocket serverSocket_ = null;

  /**
   * Port to listen on
   */
  private int port_ = 0;

  /**
   * Timeout for client sockets from accept
   */
  private int clientTimeout_ = 0;

  /**
   * Creates a server socket from underlying socket object
   */
  public TServerSocket(ServerSocket serverSocket) {
    this(serverSocket, 0);
  }

  /**
   * Creates a server socket from underlying socket object
   */
  public TServerSocket(ServerSocket serverSocket, int clientTimeout) {
    serverSocket_ = serverSocket;
    clientTimeout_ = clientTimeout;
  }

  /**
   * Creates just a port listening server socket
   */
  public TServerSocket(int port) throws TTransportException {
    this(port, 0);
  }

  /**
   * Creates just a port listening server socket
   */
  public TServerSocket(int port, int clientTimeout) throws TTransportException {
    this(new InetSocketAddress(port), clientTimeout);
    port_ = port;
  }

  public TServerSocket(InetSocketAddress bindAddr) throws TTransportException {
    this(bindAddr, 0);
  }

  public TServerSocket(InetSocketAddress bindAddr, int clientTimeout) throws TTransportException {
    clientTimeout_ = clientTimeout;
    try {
      // Make server socket
      serverSocket_ = new ServerSocket();
      // Prevent 2MSL delay problem on server restarts
      serverSocket_.setReuseAddress(true);
      // Bind to listening port
      serverSocket_.bind(bindAddr);
    } catch (IOException ioe) {
      serverSocket_ = null;
      throw new TTransportException("Could not create ServerSocket on address " + bindAddr.toString() + ".");
    }
  }

  public void listen() throws TTransportException {
    // Make sure not to block on accept
    if (serverSocket_ != null) {
      try {
        serverSocket_.setSoTimeout(0);
      } catch (SocketException sx) {
        LOGGER.log(Level.WARNING, "Could not set socket timeout.", sx);
      }
    }
  }

  protected TSocket acceptImpl() throws TTransportException {
    if (serverSocket_ == null) {
      throw new TTransportException(TTransportException.NOT_OPEN, "No underlying server socket.");
    }
    try {
      Socket result = serverSocket_.accept();
      TSocket result2 = new TSocket(result);
      result2.setTimeout(clientTimeout_);
      return result2;
    } catch (IOException iox) {
      throw new TTransportException(iox);
    }
  }

  public void close() {
    if (serverSocket_ != null) {
      try {
        serverSocket_.close();
      } catch (IOException iox) {
        LOGGER.log(Level.WARNING, "Could not close server socket.", iox);
      }
      serverSocket_ = null;
    }
  }

  public void interrupt() {
    // The thread-safeness of this is dubious, but Java documentation suggests
    // that it is safe to do this from a different thread context
    close();
  }

}
