<?php

/**
 * Copyright (c) 2006- Facebook
 * Distributed under the Thrift Software License
 *
 * See accompanying file LICENSE or visit the Thrift site at:
 * http://developers.facebook.com/thrift/
 *
 * @package thrift.transport
 */

/**
 * A memory buffer is a tranpsort that simply reads from and writes to an
 * in-memory string buffer. Anytime you call write on it, the data is simply
 * placed into a buffer, and anytime you call read, data is read from that
 * buffer.
 *
 * @package thrift.transport
 */
class TMemoryBuffer extends TTransport {

  /**
   * Constructor. Optionally pass an initial value
   * for the buffer.
   */
  public function __construct($buf = '') {
    $this->buf_ = $buf;
  }

  protected $buf_ = '';

  public function isOpen() {
    return true;
  }

  public function open() {}

  public function close() {}

  public function write($buf) {
    $this->buf_ .= $buf;
  }

  public function read($len) {
    if (strlen($this->buf_) === 0) {
      throw new TTransportException('TMemoryBuffer: Could not read ' .
                                    $len . ' bytes from buffer.',
                                    TTransportException::UNKNOWN);
    }

    if (strlen($this->buf_) <= $len) {
      $ret = $this->buf_;
      $this->buf_ = '';
      return $ret;
    }

    $ret = substr($this->buf_, 0, $len);
    $this->buf_ = substr($this->buf_, $len);

    return $ret;
  }

  function getBuffer() {
    return $this->buf_;
  }

  public function available() {
    return strlen($this->buf_);
  }
}

?>
