// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

package org.apache.thrift;

import org.apache.thrift.transport.TTransport;

/**
 * The default processor factory just returns a singleton
 * instance.
 */
public class TProcessorFactory {

  private final TProcessor processor_;

  public TProcessorFactory(TProcessor processor) {
    processor_ = processor;
  }

  public TProcessor getProcessor(TTransport trans) {
    return processor_;
  }
}
