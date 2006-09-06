package com.facebook.thrift.transport;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.net.ServerSocket;
import java.net.Socket;

/**
 * Wrapper around ServerSocket for Thrift.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public class TServerSocket extends TServerTransport {
  
  private ServerSocket serverSocket_ = null;
  private int port_ = 0;

  public TServerSocket(ServerSocket serverSocket) {
    serverSocket_ = serverSocket;
  }

  public TServerSocket(int port) throws TTransportException {
    port_ = port;
    try {
      serverSocket_ = new ServerSocket();
      serverSocket_.setReuseAddress(true);
      serverSocket_.setSoTimeout(0);
      serverSocket_.bind(new InetSocketAddress(port_));
    } catch (IOException ioe) {
      serverSocket_ = null;
      throw new TTransportException("Could not create ServerSocket on port " + port + ".");
    }
  }

  public void listen() throws TTransportException {}
  
  protected TSocket acceptImpl() throws TTransportException {
    if (serverSocket_ == null) {
      throw new TTransportException("No underlying server socket.");
    }
    try {
      Socket result = serverSocket_.accept();
      return new TSocket(result);
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
