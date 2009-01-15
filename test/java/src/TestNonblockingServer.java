package org.apache.thrift.test;

import org.apache.thrift.TException;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.protocol.TProtocolFactory;
import org.apache.thrift.server.TServer;
import org.apache.thrift.server.TSimpleServer;
import org.apache.thrift.server.TNonblockingServer;
import org.apache.thrift.server.THsHaServer;
import org.apache.thrift.transport.TNonblockingServerSocket;
import org.apache.thrift.transport.TNonblockingServerTransport;
import org.apache.thrift.transport.TFramedTransport;

// Generated code
import thrift.test.*;

import java.net.ServerSocket;
import java.util.List;
import java.util.ArrayList;
import java.util.Map;
import java.util.HashMap;
import java.util.Set;
import java.util.HashSet;


public class TestNonblockingServer extends TestServer {
  public static void main(String [] args) {
    try {
      int port = 9090;
      boolean hsha = false;

      for (int i = 0; i < args.length; i++) {
        if (args[i].equals("-p")) {
          port = Integer.valueOf(args[i++]);
        } else if (args[i].equals("-hsha")) {
          hsha = true;
        }
      }

      // Processor
      TestHandler testHandler =
        new TestHandler();
      ThriftTest.Processor testProcessor =
        new ThriftTest.Processor(testHandler);

      // Transport
      TNonblockingServerSocket tServerSocket =
        new TNonblockingServerSocket(port);

      TServer serverEngine;

      if (hsha) {
        // HsHa Server
        serverEngine = new THsHaServer(testProcessor, tServerSocket);
      } else {
        // Nonblocking Server
        serverEngine = new TNonblockingServer(testProcessor, tServerSocket);
      }

      // Run it
      System.out.println("Starting the server on port " + port + "...");
      serverEngine.serve();

    } catch (Exception x) {
      x.printStackTrace();
    }
    System.out.println("done.");
  }
}
