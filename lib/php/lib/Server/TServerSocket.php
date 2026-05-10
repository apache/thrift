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

namespace Thrift\Server;

use Thrift\Transport\TSocket;

/**
 * Socket implementation of a server agent.
 *
 * @package thrift.transport
 */
class TServerSocket extends TServerTransport
{
    /**
     * Handle for the listener socket
     *
     * @var resource|null
     */
    protected $listener;

    /**
     * Timeout when listening for a new client
     */
    protected int $acceptTimeout = 30000;

    /**
     * ServerSocket constructor
     */
    public function __construct(
        protected string $host = 'localhost',
        protected int $port = 9090,
    ) {
    }

    /**
     * Sets the accept timeout
     *
     * @param int $acceptTimeout
     * @return void
     */
    public function setAcceptTimeout($acceptTimeout)
    {
        $this->acceptTimeout = $acceptTimeout;
    }

    /**
     * Opens a new socket server handle
     *
     * @return void
     */
    public function listen()
    {
        $this->listener = stream_socket_server('tcp://' . $this->host . ':' . $this->port);
    }

    /**
     * Closes the socket server handle
     *
     * @return void
     */
    public function close()
    {
        @fclose($this->listener);
        $this->listener = null;
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

        $socket = new TSocket();
        $socket->setHandle($handle);

        return $socket;
    }
}
