package com.facebook.thrift.server;

import com.facebook.thrift.TProcessor;

/**
 * Generic interface for a Thrift server.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public abstract class TServer {

  /**
   * The options class should be subclassed by particular servers which have
   * specific options needs, while the general options should live here.
   */
  public static class Options {
    public Options() {}
  }

  /** Core processor */
  protected TProcessor processor_;

  /** Server options */
  protected Options options_;

  /**
   * Default options constructor
   */
  protected TServer(TProcessor processor) {
    this(processor, new Options());
  }

  /**
   * Default constructor, all servers take a processor and some options.
   */
  protected TServer(TProcessor processor, Options options) {
    processor_ = processor;
    options_ = options;
  }
  
  /**
   * The run method fires up the server and gets things going.
   */
  public abstract void run();
}
