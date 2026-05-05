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
class TSSLSocket extends TSocket
{
    /**
     * Remote port
     *
     * @var null|resource
     */
    protected $context = null;

    /**
     * Socket constructor
     *
     * @param string $host Remote hostname
     * @param int $port Remote port
     * @param resource $context Stream context
     * @param bool $persist Whether to use a persistent socket
     * @param string $debugHandler Function to call for error logging
     */
    public function __construct(
        $host = 'localhost',
        $port = 9090,
        $context = null,
        $debugHandler = null
    ) {
        $this->host = $this->getSSLHost($host);
        $this->port = $port;
        // Initialize a stream context if not provided
        if ($context === null) {
            $context = stream_context_create();
        }
        $this->context = $context;
        $this->debugHandler = $debugHandler ? $debugHandler : 'error_log';
    }

    /**
     * Creates a host name with SSL transport protocol
     * if no transport protocol already specified in
     * the host name.
     *
     * @param string $host Host to listen on
     * @return string $host   Host name with transport protocol
     */
    private function getSSLHost($host)
    {
        $transport_protocol_loc = strpos($host, "://");
        if ($transport_protocol_loc === false) {
            $host = 'ssl://' . $host;
        }
        return $host;
    }

    /**
     * Connects the socket.
     */
    public function open()
    {
        if ($this->isOpen()) {
            throw new TTransportException('Socket already connected', TTransportException::ALREADY_OPEN);
        }

        $host = parse_url($this->host, PHP_URL_HOST);
        if (empty($host)) {
            throw new TTransportException('Cannot open null host', TTransportException::NOT_OPEN);
        }

        if ($this->port <= 0) {
            throw new TTransportException('Cannot open without port', TTransportException::NOT_OPEN);
        }

        $this->handle = @stream_socket_client(
            $this->host . ':' . $this->port,
            $errno,
            $errstr,
            $this->sendTimeoutSec + ($this->sendTimeoutUsec / 1000000),
            STREAM_CLIENT_CONNECT,
            $this->context
        );

        // Connect failed?
        if ($this->handle === false) {
            $error = 'TSocket: Could not connect to ' .
                $this->host . ':' . $this->port . ' (' . $errstr . ' [' . $errno . '])';
            if ($this->debug) {
                call_user_func($this->debugHandler, $error);
            }
            throw new TException($error);
        }
    }
}
