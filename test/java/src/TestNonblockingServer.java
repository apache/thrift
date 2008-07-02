package com.facebook.thrift.test;

import com.facebook.thrift.TException;
import com.facebook.thrift.protocol.TBinaryProtocol;
import com.facebook.thrift.protocol.TProtocol;
import com.facebook.thrift.protocol.TProtocolFactory;
import com.facebook.thrift.server.TServer;
import com.facebook.thrift.server.TSimpleServer;
import com.facebook.thrift.server.TNonblockingServer;
import com.facebook.thrift.server.THsHaServer;
import com.facebook.thrift.transport.TNonblockingServerSocket;
import com.facebook.thrift.transport.TNonblockingServerTransport;
import com.facebook.thrift.transport.TFramedTransport;

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
