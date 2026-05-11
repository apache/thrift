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

use Thrift\Exception\TTransportException;

/**
 * Base interface for a transport agent.
 *
 * @package thrift.transport
 */
abstract class TTransport
{
    abstract public function isOpen(): bool;

    /**
     * @throws TTransportException if cannot open
     */
    abstract public function open(): void;

    abstract public function close(): void;

    /**
     * @throws TTransportException if cannot read any more data
     */
    abstract public function read(int $len): string;

    /**
     * Guarantees that the full amount of data is read.
     *
     * @throws TTransportException if cannot read data
     */
    public function readAll(int $len): string
    {
        $data = '';
        while (($got = strlen($data)) < $len) {
            $data .= $this->read($len - $got);
        }

        return $data;
    }

    /**
     * @throws TTransportException if writing fails
     */
    abstract public function write(string $buf): void;

    /**
     * @throws TTransportException if a writing error occurs
     */
    public function flush(): void
    {
    }
}
