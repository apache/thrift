package com.facebook.thrift.transport;

import java.io.IOException;
import java.net.ServerSocket;
import java.net.Socket;

/**
 * Wrapper around ServerSocket for Thrift.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public class TServerSocket extends TServerTransport {
  
  private ServerSocket serverSocket_;
  
  public TServerSocket(ServerSocket serverSocket) {
    serverSocket_ = serverSocket;
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
