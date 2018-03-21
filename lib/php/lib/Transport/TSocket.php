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

namespace Thrift\Transport;

use Thrift\Exception\TException;
use Thrift\Exception\TTransportException;
use Thrift\Factory\TStringFuncFactory;

/**
 * Sockets implementation of the TTransport interface.
 *
 * @package thrift.transport
 */
class TSocket extends TTransport
{
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
   * Connect timeout in seconds.
   *
   * Combined with connTimeoutUsec this is used for connect timeouts.
   *
   * @var int
   */
  private $connTimeoutSec_ = 0;

  /**
   * Connect timeout in microseconds.
   *
   * Combined with connTimeoutUsec this is used for connect timeouts.
   *
   * @var int
   */
  private $connTimeoutUsec_ = 100000;

  /**
   * Send timeout in seconds.
   *
   * Combined with sendTimeoutUsec this is used for send timeouts.
   *
   * @var int
   */
  private $sendTimeoutSec_ = 0;

  /**
   * Send timeout in microseconds.
   *
   * Combined with sendTimeoutSec this is used for send timeouts.
   *
   * @var int
   */
  private $sendTimeoutUsec_ = 100000;

  /**
   * Recv timeout in seconds
   *
   * Combined with recvTimeoutUsec this is used for recv timeouts.
   *
   * @var int
   */
  private $recvTimeoutSec_ = 0;

  /**
   * Recv timeout in microseconds
   *
   * Combined with recvTimeoutSec this is used for recv timeouts.
   *
   * @var int
   */
  private $recvTimeoutUsec_ = 750000;

  /**
   * Persistent socket or plain?
   *
   * @var bool
   */
  protected $persist_ = false;

  /**
   * Debugging on?
   *
   * @var bool
   */
  protected $debug_ = false;

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
    public function __construct(
        $host='localhost',
        $port=9090,
        $persist=false,
        $debugHandler=null
    ) {
        $this->host_ = $host;
        $this->port_ = $port;
        $this->persist_ = $persist;
        $this->debugHandler_ = $debugHandler ? $debugHandler : 'error_log';
    }

    /**
     * @param resource $handle
     * @return void
    */
    public function setHandle($handle)
    {
        $this->handle_ = $handle;
    }

    /**
     * Sets the conn timeout.
     *
     * @param int $timeout  Timeout in milliseconds.
     */
    public function setConnTimeout($timeout)
    {
        $this->connTimeoutSec_ = floor($timeout / 1000);
        $this->connTimeoutUsec_ =
        ($timeout - ($this->connTimeoutSec_ * 1000)) * 1000;
    }

    /**
     * Sets the send timeout.
     *
     * @param int $timeout  Timeout in milliseconds.
     */
    public function setSendTimeout($timeout)
    {
            $this->sendTimeoutSec_ = floor($timeout / 1000);
            $this->sendTimeoutUsec_ =
                    ($timeout - ($this->sendTimeoutSec_ * 1000)) * 1000;
    }

    /**
     * Sets the receive timeout.
     *
     * @param int $timeout  Timeout in milliseconds.
     */
    public function setRecvTimeout($timeout) {
            $this->recvTimeoutSec_ = floor($timeout / 1000);
            $this->recvTimeoutUsec_ =
                    ($timeout - ($this->recvTimeoutSec_ * 1000)) * 1000;
    }

    /**
     * Sets debugging output on or off
     *
     * @param bool $debug
     */
    public function setDebug($debug) 
    {
        $this->debug_ = $debug;
    }

    /**
     * Get the host that this socket is connected to
     *
     * @return string host
     */
    public function getHost()
    {
        return $this->host_;
    }

    /**
     * Get the remote port that this socket is connected to
     *
     * @return int port
     */
    public function getPort()
    {
        return $this->port_;
    }

    /**
     * Tests whether this is open
     *
     * @return bool true if the socket is open
     */
    public function isOpen()
    {
            return is_resource($this->handle_);
    }

    /**
     * Connects the socket.
     */
    public function open() 
    {
        if ($this->isOpen()) {
            throw new TTransportException('Socket already connected', 
                TTransportException::ALREADY_OPEN);
        }

        if (empty($this->host_)) {
            throw new TTransportException('Cannot open null host', 
                TTransportException::NOT_OPEN);
        }

        if ($this->port_ <= 0) {
            throw new TTransportException('Cannot open without port', 
                TTransportException::NOT_OPEN);
        }

        if ($this->persist_) {
            $this->handle_ = pfsockopen($this->host_,
                $this->port_,
                $errno,
                $errstr,
                $this->connTimeoutSec_ + ($this->connTimeoutUsec_ / 1000000));
        } else {
            $this->handle_ = fsockopen($this->host_,
                $this->port_,
                $errno,
                $errstr,
                $this->connTimeoutSec_ + ($this->connTimeoutUsec_ / 1000000));
        }

        // Connect failed?
        if ($this->handle_ === FALSE) {
            $error = 'TSocket: Could not connect to '.$this->host_.':'.$this->port_.' ('.$errstr.' ['.$errno.'])';
            if ($this->debug_) {
                call_user_func($this->debugHandler_, $error);
            }
            throw new TException($error);
        }
    }

    /**
     * Closes the socket.
     */
    public function close()
    {
        if (!$this->persist_) {
            fclose($this->handle_);
            $this->handle_ = null;
        }
    }

    /**
     * Read from the socket at most $len bytes.
     *
     * This method will not wait for all the requested data, it will return as
     * soon as any data is received.
     *
     * @param int $len Maximum number of bytes to read.
     * @return string Binary data
     */
    public function read($len)
    {
        if($len <= 0) {
            throw new TTransportException("TSocket: read len $len invalid"); 
        }
        $null = null;
        $read = array($this->handle_);

        $recvTimeoutSec_ = $this->recvTimeoutSec_;
        $recvTimeoutUsec_ = $this->recvTimeoutUsec_;
        $dataTotal = "";
        do {
            $readable = self::stream_select_ex($read, $null, $null, $recvTimeoutSec_, $recvTimeoutUsec_);

            if ($readable > 0) {
                $data = stream_socket_recvfrom($this->handle_, $len);
                if ($data === false) {
                    throw new TTransportException('TSocket: Could not read '.$len.' bytes from '.
                        $this->host_.':'.$this->port_);
                } elseif ($data == '' && feof($this->handle_)) {
                    throw new TTransportException("TSocket read 0 bytes,want $len");
                }
                $dataTotal .= $data;
                $len -= strlen($data);
            } elseif ($readable === 0) {
                throw new TTransportException('TSocket: timed out reading '.$len.' bytes from '.
                    $this->host_.':'.$this->port_);
            } else {
                throw new TTransportException('TSocket: Could not read '.$len.' bytes from '.
                    $this->host_.':'.$this->port_." ".self::sockErr_());
            }
        } while( $len > 0  && ($recvTimeoutSec_ != 0 || $recvTimeoutUsec_ !=0) );

        if($len > 0) {
            throw new TTransportException('TSocket: timed out reading '.$len.' bytes from '.
               $this->host_.':'.$this->port_.' '.self::sockErr());
        }
        return $dataTotal;
    }

    /**
     * Write to the socket.
     *
     * @param string $buf The data to write
     */
    public function write($buf) 
    {
        $null = null;
        $write = array($this->handle_);

        $sendTimeoutSec = $this->sendTimeoutSec_;
        $sendTimeoutUsec = $this->sendTimeoutUsec_;
        // keep writing until all the data has been written
        while (TStringFuncFactory::create()->strlen($buf) > 0) {
            // wait for stream to become available for writing
            $writable = self::stream_select_ex($null,$write,$null,$sendTimeoutSec,$sendTimeoutUsec);
            if ($writable > 0) {
                // write buffer to stream
                $written = stream_socket_sendto($this->handle_, $buf);
                if ($written === -1 || $written === false) {
                    throw new TTransportException('TSocket: Could not write '.TStringFuncFactory::create()->strlen($buf).
                    ' bytes '.$this->host_.':'.$this->port_." ".self::sockErr_());
                }
                // determine how much of the buffer is left to write
                $buf = TStringFuncFactory::create()->substr($buf, $written);
            } elseif ($writable === 0) {
                throw new TTransportException('TSocket: timed out writing '.TStringFuncFactory::create()->strlen($buf).' bytes from '.$this->host_.':'.$this->port_);
            } else {
                throw new TTransportException('TSocket: Could not write '.TStringFuncFactory::create()->strlen($buf).' bytes '.$this->host_.':'.$this->port_);
            }
        }
    }

    /**
     * Flush output to the socket.
     *
     * Since read(), readAll() and write() operate on the sockets directly,
     * this is a no-op
     *
     * If you wish to have flushable buffering behaviour, wrap this TSocket
     * in a TBufferedTransport.
     */
    public function flush() 
    {
        // no-op
    }

    /**
     * stream_select_ex
     *
     * Improvement of stream_select. The tv_sec and tv_usec will be updated after the function
     *
     */
    public static function stream_select_ex(&$read,&$write,&$except,&$tv_sec,&$tv_usec) 
    {
        $timeout = $tv_sec * 1000000 + $tv_usec;
        $begin = gettimeofday();
        $ret = stream_select($read,$write,$except,$tv_sec,$tv_usec);  
        $end = gettimeofday();

        $dt = $end['usec'] + $end['sec'] * 1000000 - $begin['usec'] - $begin['sec'] * 1000000;
        $rest_time = max($timeout - $dt,0);
        $tv_sec = floor($rest_time / 1000000);
        $tv_usec = $rest_time % 1000000;

        return $ret;
    }

    private static function sockErr_() 
    {
        $errno = socket_last_error(); 
        return "errno:".$errno." msg:".socket_strerror($errno)." ";
    }
}
