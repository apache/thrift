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
 */

declare(strict_types=1);

namespace Thrift\Server;

use Thrift\Transport\TSSLSocket;

/**
 * Socket implementation of a server agent.
 *
 * @package thrift.transport
 */
class TSSLServerSocket extends TServerSocket
{
    /**
     * Stream context
     *
     * @var resource|null
     */
    protected $context;

    /**
     * ServerSocket constructor
     *
     * @param resource|null $context Stream context
     */
    public function __construct(
        string $host = 'localhost',
        int $port = 9090,
        $context = null,
    ) {
        parent::__construct($this->ensureSslHostPrefix($host), $port);
        $this->context = $context ?? stream_context_create();
    }

    /**
     * Opens a new socket server handle
     *
     * @return void
     */
    public function listen()
    {
        $this->listener = @stream_socket_server(
            $this->host . ':' . $this->port,
            $errno,
            $errstr,
            STREAM_SERVER_BIND | STREAM_SERVER_LISTEN,
            $this->context
        );
    }

    /**
     * Implementation of accept. If not client is accepted in the given time
     *
     * @return TSocket
     */
    protected function acceptImpl()
    {
        $handle = @stream_socket_accept($this->listener, $this->acceptTimeout / 1000.0);
        if (!$handle) {
            return null;
        }

        $socket = new TSSLSocket();
        $socket->setHandle($handle);

        return $socket;
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
