package com.facebook.thrift.transport;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.SocketException;

/**
 * Wrapper around ServerSocket for Thrift.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public class TServerSocket extends TServerTransport {
  
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
    port_ = port;
    clientTimeout_ = clientTimeout;
    try {
      // Make server socket
      serverSocket_ = new ServerSocket();
      // Prevent 2MSL delay problem on server restarts
      serverSocket_.setReuseAddress(true);
      // Bind to listening port
      serverSocket_.bind(new InetSocketAddress(port_));
    } catch (IOException ioe) {
      serverSocket_ = null;
      throw new TTransportException("Could not create ServerSocket on port " + port + ".");
    }
  }

  public void listen() throws TTransportException {
    // Make sure not to block on accept
    if (serverSocket_ != null) {
      try {
        serverSocket_.setSoTimeout(0);
      } catch (SocketException sx) {
        sx.printStackTrace();
      }
    }
  }
  
  protected TSocket acceptImpl() throws TTransportException {
    if (serverSocket_ == null) {
      throw new TTransportException("No underlying server socket.");
    }
    try {
      // Accept socket and tune TCP params
      Socket result = serverSocket_.accept();
      client.setSoLinger(false, 0);
      client.setTcpNoDelay(true);
      // Wrap in TSocket and set timeout
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
        System.err.println("WARNING: Could not close server socket: " +
                           iox.getMessage());
      }
      serverSocket_ = null;
    }
  }

}
