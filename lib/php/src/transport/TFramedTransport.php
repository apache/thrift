<?php

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
   * Constructor.
   *
   * @param TTransport $transport Underlying transport
   */
  public __construct($transport=null) {
    $this->transport_ = $transport;
  }

  /**
   * Reads from the buffer. When more data is required reads another entire
   * chunk and serves future reads out of that.
   *
   * @param int $len How much data
   */
  public function read($len) {
    $out = '';
    $need = $len;
    $have = strlen($this->rBuf_);
    if ($need > $have) {
      $out = $this->rBuf_;
      $need -= $have;
      $this->readFrame();
    }

    $give = $need;
    if (strlen($this->rBuf_) < $give) {
      $out .= $this->rBuf_;
      $this->rBuf_ = '';
    } else {
      $out .= substr($this->rBuf_, 0, $give);
      $this->rBuf_ = substr($this->rBuf_, $give);
    }

    return $out;
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
    $out = pack('N', strlen($this->wBuf_));
    $out .= $this->wBuf_;
    $this->transport_->write($out);
    $this->transport_->flush();
    $this->wBuf_ = '';
  }

}