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
