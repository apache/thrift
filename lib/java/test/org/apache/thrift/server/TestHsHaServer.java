package org.apache.thrift.server;

import org.apache.thrift.TProcessor;
import org.apache.thrift.protocol.TProtocolFactory;
import org.apache.thrift.transport.TNonblockingServerSocket;

public class TestHsHaServer extends TestNonblockingServer {
  protected TServer getServer(TProcessor processor, TNonblockingServerSocket socket, TProtocolFactory protoFactory) {
    return new THsHaServer(processor, socket, protoFactory);
  }
}
