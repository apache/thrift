package com.facebook.thrift.transport;

import com.facebook.thrift.TException;

/**
 * Transport exceptions.
 *
 * @author Mark Slee <mcslee@facebook.com>
 */
public class TTransportException extends TException {
  public TTransportException() {
    super();
  }

  public TTransportException(String message) {
    super(message);
  }

  public TTransportException(Throwable cause) {
    super(cause);
  }

  public TTransportException(String message, Throwable cause) {
    super(message, cause);
  }
}
