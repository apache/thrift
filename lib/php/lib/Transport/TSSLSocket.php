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

declare(strict_types=1);

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
     * Stream context
     *
     * @var resource|null
     */
    protected $context;

    /**
     * Socket constructor
     *
     * @param resource|null $context Stream context
     */
    public function __construct(
        string $host = 'localhost',
        int $port = 9090,
        $context = null,
        $debugHandler = null,
    ) {
        parent::__construct($this->ensureSslHostPrefix($host), $port, false, $debugHandler);
        $this->context = $context ?? stream_context_create();
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

    /**
     * Returns the host with an `ssl://` prefix when no transport-protocol
     * prefix is already present.
     */
    private function ensureSslHostPrefix(string $host): string
    {
        return str_contains($host, '://') ? $host : 'ssl://' . $host;
    }
}
