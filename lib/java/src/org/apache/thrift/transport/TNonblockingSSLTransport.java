package org.apache.thrift.transport;

import java.io.IOException;
import java.net.SocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;

public abstract class TNonblockingSSLTransport extends TTransport{
	  /**
	   * Non-blocking connection initialization.
	   * @see java.nio.channels.SocketChannel#connect(SocketAddress remote)
	   */
	  public abstract boolean startConnect() throws IOException;

	  /**
	   * Non-blocking connection completion.
	   * @see java.nio.channels.SocketChannel#finishConnect()
	   */
	  public abstract boolean finishConnect() throws IOException;

	  public abstract SelectionKey registerSelector(Selector selector, int interests) throws IOException;

	  public abstract ByteBuffer read(int numBytes) throws IOException;

	  public abstract int write(ByteBuffer buffer) throws IOException;

	  public abstract void beginSSLHandshake() throws IOException;

	  public abstract boolean doHandShake() throws IOException;

	  public abstract boolean startConnection() throws IOException;
}
