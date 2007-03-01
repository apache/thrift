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
 * Buffered transport. Stores data to an internal buffer that it doesn't
 * actually write out until flush is called. For reading, we do a greedy
 * read and then serve data out of the internal buffer.
 *
 * @package thrift.transport
 * @author Mark Slee <mcslee@facebook.com>
 */
class TBufferedTransport extends TTransport {

  /**
   * Constructor. Creates a buffered transport around an underlying transport
   */
  public function __construct($transport=null, $rBufSize=512, $wBufSize=512) {
    $this->transport_ = $transport;
    $this->rBufSize_ = $rBufSize;
    $this->wBufSize_ = $wBufSize;
  }

  /**
   * The underlying transport
   *
   * @var TTransport
   */
  protected $transport_ = null;

  /**
   * The receive buffer size
   *
   * @var int
   */
  protected $rBufSize_ = 512;

  /**
   * The write buffer size
   *
   * @var int
   */
  protected $wBufSize_ = 512;

  /**
   * The write buffer.
   *
   * @var string
   */
  protected $wBuf_ = '';

  /**
   * The read buffer.
   *
   * @var string
   */
  protected $rBuf_ = '';

  public function isOpen() {
    return $this->transport_->isOpen();
  }

  public function open() {
    $this->transport_->open();
  }

  public function close() {
    $this->transport_->close();
  }

  public function readAll($len) {
    return $this->transport_->readAll($len);
  }
  
  public function read($len) {
    // Methinks PHP is already buffering these for us
    return $this->transport_->read($len);

    if (strlen($this->rBuf_) >= $len) {
      $ret = substr($this->rBuf_, 0, $len);
      $this->rBuf_ = substr($this->rBuf_, $len);
      return $ret;
    }

    $this->rBuf_ .= $this->transport_->read($this->rBufSize_);
    $give = min(strlen($this->rBuf_), $len);
    $ret = substr($this->rBuf_, 0, $give);
    $this->rBuf_ = substr($this->rBuf_, $give);
    return $ret;
  }

  public function write($buf) {
    $this->wBuf_ .= $buf;
    if (strlen($this->wBuf_) >= $this->wBufSize_) {
      $this->transport_->write($this->wBuf_);
      $this->wBuf_ = '';
    }
  }

  public function flush() {
    if (!empty($this->wBuf_)) {
      $this->transport_->write($this->wBuf_);
      $this->wBuf_ = '';
    }
  }

}

?>
