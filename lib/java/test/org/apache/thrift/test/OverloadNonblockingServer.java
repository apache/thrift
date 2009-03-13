
package org.apache.thrift.test;

import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.transport.TSocket;


public class OverloadNonblockingServer {

  public static void main(String[] args) throws Exception {
    int msg_size_mb = Integer.parseInt(args[0]);
    int msg_size = msg_size_mb * 1024 * 1024;

    TSocket socket = new TSocket("localhost", 9090);
    TBinaryProtocol binprot = new TBinaryProtocol(socket);
    socket.open();
    binprot.writeI32(msg_size);
    binprot.writeI32(1);
    socket.flush();

    System.in.read();
    // Thread.sleep(30000);
    for (int i = 0; i < msg_size_mb; i++) {
      binprot.writeBinary(new byte[1024 * 1024]);
    }

    socket.close();
  }
}
