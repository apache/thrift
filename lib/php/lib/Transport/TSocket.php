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
    protected $handle = null;

    /**
     * Remote hostname
     *
     * @var string
     */
    protected $host = 'localhost';

    /**
     * Remote port
     *
     * @var int
     */
    protected $port = '9090';

    /**
     * Send timeout in seconds.
     *
     * Combined with sendTimeoutUsec this is used for send timeouts.
     *
     * @var int
     */
    protected $sendTimeoutSec = 0;

    /**
     * Send timeout in microseconds.
     *
     * Combined with sendTimeoutSec this is used for send timeouts.
     *
     * @var int
     */
    protected $sendTimeoutUsec = 100000;

    /**
     * Recv timeout in seconds
     *
     * Combined with recvTimeoutUsec this is used for recv timeouts.
     *
     * @var int
     */
    protected $recvTimeoutSec = 0;

    /**
     * Recv timeout in microseconds
     *
     * Combined with recvTimeoutSec this is used for recv timeouts.
     *
     * @var int
     */
    protected $recvTimeoutUsec = 750000;

    /**
     * Persistent socket or plain?
     *
     * @var bool
     */
    protected $persist = false;

    /**
     * Debugging on?
     *
     * @var bool
     */
    protected $debug = false;

    /**
     * Debug handler
     *
     * @var mixed
     */
    protected $debugHandler = null;

    /**
     * Socket constructor
     *
     * @param string $host Remote hostname
     * @param int $port Remote port
     * @param bool $persist Whether to use a persistent socket
     * @param string $debugHandler Function to call for error logging
     */
    public function __construct(
        $host = 'localhost',
        $port = 9090,
        $persist = false,
        $debugHandler = null
    ) {
        $this->host = $host;
        $this->port = $port;
        $this->persist = $persist;
        $this->debugHandler = $debugHandler ? $debugHandler : 'error_log';
    }

    /**
     * @param resource $handle
     * @return void
     */
    public function setHandle($handle)
    {
        $this->handle = $handle;
        stream_set_blocking($this->handle, false);
    }

    /**
     * Sets the send timeout.
     *
     * @param int $timeout Timeout in milliseconds.
     */
    public function setSendTimeout($timeout)
    {
        $this->sendTimeoutSec = floor($timeout / 1000);
        $this->sendTimeoutUsec =
            ($timeout - ($this->sendTimeoutSec * 1000)) * 1000;
    }

    /**
     * Sets the receive timeout.
     *
     * @param int $timeout Timeout in milliseconds.
     */
    public function setRecvTimeout($timeout)
    {
        $this->recvTimeoutSec = floor($timeout / 1000);
        $this->recvTimeoutUsec =
            ($timeout - ($this->recvTimeoutSec * 1000)) * 1000;
    }

    /**
     * Sets debugging output on or off
     *
     * @param bool $debug
     */
    public function setDebug($debug)
    {
        $this->debug = $debug;
    }

    /**
     * Get the host that this socket is connected to
     *
     * @return string host
     */
    public function getHost()
    {
        return $this->host;
    }

    /**
     * Get the remote port that this socket is connected to
     *
     * @return int port
     */
    public function getPort()
    {
        return $this->port;
    }

    /**
     * Tests whether this is open
     *
     * @return bool true if the socket is open
     */
    public function isOpen()
    {
        return is_resource($this->handle);
    }

    /**
     * Connects the socket.
     */
    public function open()
    {
        if ($this->isOpen()) {
            throw new TTransportException('Socket already connected', TTransportException::ALREADY_OPEN);
        }

        if (empty($this->host)) {
            throw new TTransportException('Cannot open null host', TTransportException::NOT_OPEN);
        }

        if ($this->port <= 0 && strpos($this->host, 'unix://') !== 0) {
            throw new TTransportException('Cannot open without port', TTransportException::NOT_OPEN);
        }

        if ($this->persist) {
            $this->handle = @pfsockopen(
                $this->host,
                $this->port,
                $errno,
                $errstr,
                $this->sendTimeoutSec + ($this->sendTimeoutUsec / 1000000)
            );
        } else {
            $this->handle = @fsockopen(
                $this->host,
                $this->port,
                $errno,
                $errstr,
                $this->sendTimeoutSec + ($this->sendTimeoutUsec / 1000000)
            );
        }

        // Connect failed?
        if ($this->handle === false) {
            $error = 'TSocket: Could not connect to ' .
                $this->host . ':' . $this->port . ' (' . $errstr . ' [' . $errno . '])';
            if ($this->debug) {
                call_user_func($this->debugHandler, $error);
            }
            throw new TException($error);
        }

        if (function_exists('socket_import_stream') && function_exists('socket_set_option')) {
            // warnings silenced due to bug https://bugs.php.net/bug.php?id=70939
            $socket = socket_import_stream($this->handle);
            if ($socket !== false) {
                @socket_set_option($socket, SOL_TCP, TCP_NODELAY, 1);
            }
        }
    }

    /**
     * Closes the socket.
     */
    public function close()
    {
        @fclose($this->handle);
        $this->handle = null;
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
        $null = null;
        $read = array($this->handle);
        $readable = @stream_select(
            $read,
            $null,
            $null,
            $this->recvTimeoutSec,
            $this->recvTimeoutUsec
        );

        if ($readable > 0) {
            $data = fread($this->handle, $len);
            if ($data === false) {
                throw new TTransportException('TSocket: Could not read ' . $len . ' bytes from ' .
                    $this->host . ':' . $this->port);
            } elseif ($data == '' && feof($this->handle)) {
                throw new TTransportException('TSocket read 0 bytes');
            }

            return $data;
        } elseif ($readable === 0) {
            throw new TTransportException('TSocket: timed out reading ' . $len . ' bytes from ' .
                $this->host . ':' . $this->port);
        } else {
            throw new TTransportException('TSocket: Could not read ' . $len . ' bytes from ' .
                $this->host . ':' . $this->port);
        }
    }

    /**
     * Write to the socket.
     *
     * @param string $buf The data to write
     */
    public function write($buf)
    {
        $null = null;
        $write = array($this->handle);

        // keep writing until all the data has been written
        while (strlen($buf) > 0) {
            // wait for stream to become available for writing
            $writable = @stream_select(
                $null,
                $write,
                $null,
                $this->sendTimeoutSec,
                $this->sendTimeoutUsec
            );
            if ($writable > 0) {
                // write buffer to stream
                $written = fwrite($this->handle, $buf);
                $closed_socket = $written === 0 && feof($this->handle);
                if ($written === -1 || $written === false || $closed_socket) {
                    throw new TTransportException(
                        'TSocket: Could not write ' . strlen($buf) . ' bytes ' .
                        $this->host . ':' . $this->port
                    );
                }
                // determine how much of the buffer is left to write
                $buf = substr($buf, $written);
            } elseif ($writable === 0) {
                throw new TTransportException(
                    'TSocket: timed out writing ' . strlen($buf) . ' bytes from ' .
                    $this->host . ':' . $this->port
                );
            } else {
                throw new TTransportException(
                    'TSocket: Could not write ' . strlen($buf) . ' bytes ' .
                    $this->host . ':' . $this->port
                );
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
}
