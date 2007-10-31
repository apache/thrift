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
 * Framed transport. Writes and reads data in chunks that are stamped with
 * their length.
 *
 * @package thrift.transport
 * @author Mark Slee <mcslee@facebook.com>
 */
class TFramedTransport extends TTransport {

  /**
   * Underlying transport object.
   *
   * @var TTransport
   */
  private $transport_;

  /**
   * Buffer for read data.
   *
   * @var string
   */
  private $rBuf_;

  /**
   * Buffer for queued output data
   *
   * @var string
   */
  private $wBuf_;

  /**
   * Whether to frame reads
   *
   * @var bool
   */
  private $read_;

  /**
   * Whether to frame writes
   *
   * @var bool
   */
  private $write_;

  /**
   * Constructor.
   *
   * @param TTransport $transport Underlying transport
   */
  public function __construct($transport=null, $read=true, $write=true) {
    $this->transport_ = $transport;
    $this->read_ = $read;
    $this->write_ = $write;
  }

  public function isOpen() {
    return $this->transport_->isOpen();
  }

  public function open() {
    $this->transport_->open();
  }

  public function close() {
    $this->transport_->close();
  }

  /**
   * Reads from the buffer. When more data is required reads another entire
   * chunk and serves future reads out of that.
   *
   * @param int $len How much data
   */
  public function read($len) {
    if (!$this->read_) {
      return $this->transport_->read($len);
    }

    if (empty($this->rBuf_)) {
      $this->readFrame();
    }

    // Just return full buff
    if ($len >= strlen($this->rBuf_)) {
      $out = $this->rBuf_;
      $this->rBuf_ = null;
      return $out;
    }
    
    // Return substr
    $out = substr($this->rBuf_, 0, $len);
    $this->rBuf_ = substr($this->rBuf_, $len);
    return $out;
  }

  /**
   * Put previously read data back into the buffer
   *
   * @param string $data data to return
   */
  public function putBack($data) {
    if (empty($this->rBuf_)) {
      $this->rBuf_ = $data;
    }
    else {
      $this->rBuf_ = ($data . $this->rBuf_);
    }
  }

  /**
   * Reads a chunk of data into the internal read buffer.
   */
  private function readFrame() {
    $buf = $this->transport_->readAll(4);
    $val = unpack('N', $buf);
    $sz = $val[1];

    $this->rBuf_ = $this->transport_->readAll($sz);
  }

  /**
   * Writes some data to the pending output buffer.
   *
   * @param string $buf The data
   * @param int    $len Limit of bytes to write
   */
  public function write($buf, $len=null) {
    if (!$this->write_) {
      return $this->transport_->write($buf, $len);
    }

    if ($len !== null && $len < strlen($buf)) {
      $buf = substr($buf, 0, $len);
    }
    $this->wBuf_ .= $buf;
  }

  /**
   * Writes the output buffer to the stream in the format of a 4-byte length
   * followed by the actual data.
   */
  public function flush() {
    if (!$this->write_) {
      return $this->transport_->flush();
    }

    $out = pack('N', strlen($this->wBuf_));
    $out .= $this->wBuf_;
    $this->transport_->write($out);
    $this->transport_->flush();
    $this->wBuf_ = '';
  }

}
