<?php

/**
 * Copyright (c) 2006- Facebook
 * Distributed under the Thrift Software License
 *
 * See accompanying file LICENSE or visit the Thrift site at:
 * http://developers.facebook.com/thrift/
 *
 * @package thrift.transport
 * @author Mark Slee <mcslee@facebook.com>
 */

/**
 * Transport that only accepts writes and ignores them.
 * This is useful for measuring the serialized size of structures.
 *
 * @package thrift.transport
 * @author David Reiss <dreiss@facebook.com>
 */
class TNullTransport extends TTransport {

  public function isOpen() {
    return true;
  }

  public function open() {}

  public function close() {}

  public function read($len) {
    throw new TTransportException("Can't read from TNullTransport.");
  }

  public function write($buf) {}

}

?>
