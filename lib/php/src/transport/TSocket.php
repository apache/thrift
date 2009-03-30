<?php
/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements. See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership. The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License. You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied. See the License for the
 * specific language governing permissions and limitations
 * under the License.
 *
 * @package thrift.transport
 */


/**
 * Sockets implementation of the TTransport interface.
 *
 * @package thrift.transport
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
  protected $debug_ = FALSE;

  /**
   * Debug handler
   *
   * @var mixed
   */
  protected $debugHandler_ = null;

  /**
   * Socket constructor
   *
   * @param string $host         Remote hostname
   * @param int    $port         Remote port
   * @param bool   $persist      Whether to use a persistent socket
   * @param string $debugHandler Function to call for error logging
   */
  public function __construct($host='localhost',
                              $port=9090,
                              $persist=FALSE,
                              $debugHandler=null) {
    $this->host_ = $host;
    $this->port_ = $port;
    $this->persist_ = $persist;
    $this->debugHandler_ = $debugHandler ? $debugHandler : 'error_log';
  }

  /**
   * Sets the send timeout.
   *
   * @param int $timeout  Timeout in milliseconds.
   */
  public function setSendTimeout($timeout) {
    $this->sendTimeout_ = $timeout;
  }

  /**
   * Sets the receive timeout.
   *
   * @param int $timeout  Timeout in milliseconds.
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
   * Get the host that this socket is connected to
   *
   * @return string host
   */
  public function getHost() {
    return $this->host_;
  }

  /**
   * Get the remote port that this socket is connected to
   *
   * @return int port
   */
  public function getPort() {
    return $this->port_;
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
      $this->handle_ = @pfsockopen($this->host_,
                                   $this->port_,
                                   $errno,
                                   $errstr,
                                   $this->sendTimeout_/1000.0);
    } else {
      $this->handle_ = @fsockopen($this->host_,
                                  $this->port_,
                                  $errno,
                                  $errstr,
                                  $this->sendTimeout_/1000.0);
    }

    // Connect failed?
    if ($this->handle_ === FALSE) {
      $error = 'TSocket: Could not connect to '.$this->host_.':'.$this->port_.' ('.$errstr.' ['.$errno.'])';
      if ($this->debug_) {
        call_user_func($this->debugHandler_, $error);
      }
      throw new TException($error);
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
    // This call does not obey stream_set_timeout values!
    // $buf = @stream_get_contents($this->handle_, $len);

    $pre = null;
    while (TRUE) {
      $buf = @fread($this->handle_, $len);
      if ($buf === FALSE || $buf === '') {
        $md = stream_get_meta_data($this->handle_);
        if ($md['timed_out']) {
          throw new TException('TSocket: timed out reading '.$len.' bytes from '.
                               $this->host_.':'.$this->port_);
        } else {
          throw new TException('TSocket: Could not read '.$len.' bytes from '.
                               $this->host_.':'.$this->port_);
        }
      } else if (($sz = strlen($buf)) < $len) {
        $md = stream_get_meta_data($this->handle_);
        if ($md['timed_out']) {
          throw new TException('TSocket: timed out reading '.$len.' bytes from '.
                               $this->host_.':'.$this->port_);
        } else {
          $pre .= $buf;
          $len -= $sz;
        }
      } else {
        return $pre.$buf;
      }
    }
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
    $data = @fread($this->handle_, $len);
    if ($data === FALSE || $data === '') {
      $md = stream_get_meta_data($this->handle_);
      if ($md['timed_out']) {
        throw new TException('TSocket: timed out reading '.$len.' bytes from '.
                             $this->host_.':'.$this->port_);
      } else {
        throw new TException('TSocket: Could not read '.$len.' bytes from '.
                             $this->host_.':'.$this->port_);
      }
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
    while (strlen($buf) > 0) {
      $got = @fwrite($this->handle_, $buf);
      if ($got === 0 || $got === FALSE) {
        $md = stream_get_meta_data($this->handle_);
        if ($md['timed_out']) {
          throw new TException('TSocket: timed out writing '.strlen($buf).' bytes from '.
                               $this->host_.':'.$this->port_);
        } else {
            throw new TException('TSocket: Could not write '.strlen($buf).' bytes '.
                                 $this->host_.':'.$this->port_);
        }
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
      throw new TException('TSocket: Could not flush: '.
                           $this->host_.':'.$this->port_);
    }
  }
}

?>
