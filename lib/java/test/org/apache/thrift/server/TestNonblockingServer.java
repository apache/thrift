package org.apache.thrift.server;


import org.apache.thrift.TProcessor;
import org.apache.thrift.protocol.TProtocolFactory;
import org.apache.thrift.transport.TFramedTransport;
import org.apache.thrift.transport.TNonblockingServerSocket;
import org.apache.thrift.transport.TSocket;
import org.apache.thrift.transport.TTransport;

public class TestNonblockingServer extends ServerTestBase {

  private Thread serverThread;
  private TServer server;

  protected TServer getServer(TProcessor processor, TNonblockingServerSocket socket, TProtocolFactory protoFactory) {
    return new TNonblockingServer(processor, socket, protoFactory);
  }

  @Override
  public void startServer(final TProcessor processor, final TProtocolFactory protoFactory) throws Exception {
    serverThread = new Thread() {
      public void run() {
        try {
          // Transport
          TNonblockingServerSocket tServerSocket =
            new TNonblockingServerSocket(PORT);

          server = getServer(processor, tServerSocket, protoFactory);

          // Run it
          System.out.println("Starting the server on port " + PORT + "...");
          server.serve();
        } catch (Exception e) {
          e.printStackTrace();
          fail();
        }
      }
    };
    serverThread.start();
    Thread.sleep(1000);
  }

  @Override
  public void stopServer() throws Exception {
    server.stop();
    try {
      serverThread.join();
    } catch (InterruptedException e) {}
  }

  @Override
  public TTransport getTransport() throws Exception {
    TSocket socket = new TSocket(HOST, PORT);
    socket.setTimeout(SOCKET_TIMEOUT);
    return new TFramedTransport(socket);
  }
}
