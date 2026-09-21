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
 */

declare(strict_types=1);

namespace Test\Thrift\Unit\Lib\Server\Fixture;

use Thrift\Server\TServer;
use Thrift\Server\TServerTransport;
use Thrift\Transport\TTransport;

/**
 * Hands out the given connections one per accept(). Once they are all taken,
 * it stops the server and answers the next accept() with nothing, the way a
 * listening socket does when its accept timeout expires.
 */
class QueuedServerTransport extends TServerTransport
{
    public int $accepted = 0;

    public ?TServer $server = null;

    /**
     * @param TTransport[] $connections
     */
    public function __construct(private array $connections)
    {
    }

    public function listen(): void
    {
    }

    public function close(): void
    {
    }

    protected function acceptImpl(): ?TTransport
    {
        if ($this->connections === []) {
            $this->server?->stop();

            return null;
        }
        ++$this->accepted;

        return array_shift($this->connections);
    }
}
