// Copyright (c) 2006- Facebook
// Distributed under the Thrift Software License
//
// See accompanying file LICENSE or visit the Thrift site at:
// http://developers.facebook.com/thrift/

package com.facebook.thrift;

/**
 * Generic exception class for Thrift.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public class TException extends Exception {
  public TException() {
    super();
  }

  public TException(String message) {
    super(message);
  }
 
  public TException(Throwable cause) {
    super(cause);
  }

  public TException(String message, Throwable cause) {
    super(message, cause);
  }
}
