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

use Thrift\Transport\TMemoryBuffer;
use Thrift\Transport\TTransport;

/**
 * An accepted client connection: reads come from the request the client sent,
 * writes are collected apart from it, and close() is counted.
 */
class ConnectionStub extends TTransport
{
    public int $closed = 0;

    public string $written = '';

    private TMemoryBuffer $request;

    public function __construct(string $request)
    {
        $this->request = new TMemoryBuffer($request);
    }

    public function isOpen(): bool
    {
        return true;
    }

    public function open(): void
    {
    }

    public function close(): void
    {
        ++$this->closed;
    }

    public function read(int $len): string
    {
        return $this->request->read($len);
    }

    public function write(string $buf): void
    {
        $this->written .= $buf;
    }
}
