
package org.apache.thrift.transport;

import java.nio.channels.Selector;

/**
 * Server transport that can be operated in a nonblocking fashion.
 */
public abstract class TNonblockingServerTransport extends TServerTransport {

  public abstract void registerSelector(Selector selector);
}
