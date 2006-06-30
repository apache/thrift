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
  private $host_ = 'localhost';

  /**
   * Remote port
   *
   * @var int
   */
  private $port_ = '9090';

  /**
   * Persistent socket or plain?
   *
   * @var bool
   */
  private $persist_ = false;

  /**
   * Socket constructor
   *
   * @param string $host    Remote hostname
   * @param int    $port    Remote port
   * @param bool   $persist Whether to use a persistent socket
   */
  public function __construct($host='localhost', $port=9090, $persist=false) {
    $this->host_ = $host;
    $this->port_ = $port;
    $this->persist_ = $persist;
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
      $this->handle_ = pfsockopen($this->host_, $this->port_);
    } else {
      $this->handle_ = fsockopen($this->host_, $this->port_);
    }
    if ($this->handle_ === FALSE) {      
      throw new Exception('TSocket: Could not connect to '.
                          $this->host_.':'.$this->port_);
    }
  }

  /**
   * Closes the socket
   */
  public function close() {
    if (!$this->persist_) {
      fclose($this->handle_);
    }
  }
  
  /**
   * Uses stream get contents to do the reading
   */
  public function readAll($len) {
    return stream_get_contents($this->handle_, $len);
  }

  /**
   * Read from the socket
   */
  public function read($len) {
    $data = fread($this->handle_, 1);
    if ($data === FALSE) {
      throw new Exception('TSocket: Could not read '.$len.' bytes from '.
                          $this->host_.':'.$this->port_);
    }
    return $data;
  }

  /**
   * Write to the socket.
   */
  public function write($buf) {
    while (!empty($buf)) {
      $got = fwrite($this->handle_, $buf);
      if ($got == false) {
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
    fflush($this->handle_);
  }
}

?>
