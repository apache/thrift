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
 * @package thrift.protocol
 */

declare(strict_types=1);

namespace Thrift\Factory;

use Thrift\Protocol\TBinaryProtocolAccelerated;
use Thrift\Transport\TTransport;

/**
 * Accelerated Binary Protocol Factory.
 *
 * Produces TBinaryProtocolAccelerated, which dispatches read/write through
 * the thrift_protocol C extension when available and falls back to
 * TBinaryProtocol otherwise.
 */
class TBinaryProtocolAcceleratedFactory implements TProtocolFactory
{
    public function __construct(
        private bool $strictRead = false,
        private bool $strictWrite = true,
    ) {
    }

    public function getProtocol(TTransport $trans): TBinaryProtocolAccelerated
    {
        return new TBinaryProtocolAccelerated($trans, $this->strictRead, $this->strictWrite);
    }
}
