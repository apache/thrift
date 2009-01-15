package org.apache.thrift.transport;

import java.io.IOException;
import java.nio.channels.Selector;
import java.nio.channels.SelectionKey;
import java.nio.ByteBuffer;

public abstract class TNonblockingTransport extends TTransport {
  public abstract SelectionKey registerSelector(Selector selector, int interests) throws IOException;
  public abstract int read(ByteBuffer buffer) throws IOException;
  public abstract int write(ByteBuffer buffer) throws IOException;
}
