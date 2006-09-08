<?php

/**
 * Sockets implementation of the TTransport interface.
 *
 * @package thrift.transport
 * @author Mark Slee <mcslee@facebook.com>
 */
class TSocket extends TTransport {

  /**
   * Handle to PHP socket
   *
   * @var resource
   */
  private $handle_ = null;

  /**
   * Remote hostname
   * 
   * @var string
   */
  protected $host_ = 'localhost';

  /**
   * Remote port
   *
   * @var int
   */
  protected $port_ = '9090';

  /**
   * Send timeout in milliseconds
   *
   * @var int
   */
  private $sendTimeout_ = 100;

  /**
   * Recv timeout in milliseconds
   *
   * @var int
   */
  private $recvTimeout_ = 750;

  /**
   * Is send timeout set?
   *
   * @var bool
   */
  private $sendTimeoutSet_ = FALSE;

  /**
   * Persistent socket or plain?
   *
   * @var bool
   */
  private $persist_ = FALSE;

  /**
   * Debugging on?
   *
   * @var bool
   */
  private $debug_ = FALSE;

  /**
   * Socket constructor
   *
   * @param string $host    Remote hostname
   * @param int    $port    Remote port
   * @param bool   $persist Whether to use a persistent socket
   */
  public function __construct($host='localhost', $port=9090, $persist=FALSE) {
    $this->host_ = $host;
    $this->port_ = $port;
    $this->persist_ = $persist;
  }

  /**
   * Sets the send timeout.
   *
   * @param int $timeout
   */
  public function setSendTimeout($timeout) {
    $this->sendTimeout_ = $timeout;
  }

  /**
   * Sets the receive timeout.
   *
   * @param int $timeout
   */
  public function setRecvTimeout($timeout) {
    $this->recvTimeout_ = $timeout;
  }

  /**
   * Sets debugging output on or off
   *
   * @param bool $debug
   */
  public function setDebug($debug) {
    $this->debug_ = $debug;
  }

  /**
   * Tests whether this is open
   *
   * @return bool true if the socket is open
   */
  public function isOpen() {
    return is_resource($this->handle_);
  }

  /**
   * Connects the socket.
   */
  public function open() {
    if ($this->persist_) {
      $this->handle_ = pfsockopen($this->host_,
                                  $this->port_,
                                  $errno,
                                  $errstr,
                                  $this->sendTimeout_/1000.0);
    } else {
      $this->handle_ = fsockopen($this->host_,
                                 $this->port_,
                                 $errno,
                                 $errstr,
                                 $this->sendTimeout_/1000.0);
    }

    // Connect failed?
    if ($this->handle_ === FALSE) {
      $error = 'TSocket: Could not connect to '.$this->host_.':'.$this->port_;
      if ($this->debug_) {
        error_log($error);
      }
      throw new Exception($error);
    }
    
    stream_set_timeout($this->handle_, 0, $this->sendTimeout_*1000);
    $this->sendTimeoutSet_ = TRUE;
  }

  /**
   * Closes the socket.
   */
  public function close() {
    if (!$this->persist_) {
      @fclose($this->handle_);
      $this->handle_ = null;
    }
  }
  
  /**
   * Uses stream get contents to do the reading
   *
   * @param int $len How many bytes
   * @return string Binary data
   */
  public function readAll($len) {
    if ($this->sendTimeoutSet_) {
      stream_set_timeout($this->handle_, 0, $this->recvTimeout_*1000);
      $this->sendTimeoutSet_ = FALSE;
    }
    $buf = @stream_get_contents($this->handle_, $len);
    if ($buf === FALSE || strlen($buf) !== $len) {
      throw new Exception('TSocket: Could not read '.$len.' bytes from '.
                          $this->host_.':'.$this->port_);
    }
    return $buf;
  }

  /**
   * Read from the socket
   *
   * @param int $len How many bytes
   * @return string Binary data
   */
  public function read($len) {
    if ($this->sendTimeoutSet_) {
      stream_set_timeout($this->handle_, 0, $this->recvTimeout_*1000);
      $this->sendTimeoutSet_ = FALSE;
    }
    $data = @fread($this->handle_, 1);
    if ($data === FALSE) {
      throw new Exception('TSocket: Could not read '.$len.' bytes from '.
                          $this->host_.':'.$this->port_);
    }
    return $data;
  }

  /**
   * Write to the socket.
   *
   * @param string $buf The data to write
   */
  public function write($buf) {
    if (!$this->sendTimeoutSet_) {
      stream_set_timeout($this->handle_, 0, $this->sendTimeout_*1000);
      $this->sendTimeoutSet_ = TRUE;
    }
    while (!empty($buf)) {
      $got = @fwrite($this->handle_, $buf);
      if ($got === 0 || $got === FALSE) {
        throw new Exception('TSocket: Could not write '.strlen($buf).' bytes '.
                            $this->host_.':'.$this->port_);
      }
      $buf = substr($buf, $got);
    }
  }

  /**
   * Flush output to the socket.
   */
  public function flush() {
    $ret = fflush($this->handle_);
    if ($ret === FALSE) {
      throw new Exception('TSocket: Could not flush: '.
                          $this->host_.':'.$this->port_);
    }
  }
}

?>
