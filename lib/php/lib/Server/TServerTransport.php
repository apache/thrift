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

namespace Thrift\Server;

use Thrift\Exception\TTransportException;
use Thrift\Transport\TTransport;

/**
 * Generic class for Server agent.
 *
 * @package thrift.transport
 */
abstract class TServerTransport
{
    abstract public function listen(): void;

    abstract public function close(): void;

    /**
     * Subclasses implement accept here.
     */
    abstract protected function acceptImpl(): ?TTransport;

    /**
     * @throws TTransportException when no transport is available
     */
    public function accept(): TTransport
    {
        $transport = $this->acceptImpl();

        if ($transport === null) {
            throw new TTransportException("accept() may not return NULL");
        }

        return $transport;
    }
}
