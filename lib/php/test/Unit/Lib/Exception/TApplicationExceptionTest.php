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

namespace Test\Thrift\Unit\Lib\Exception;

use PHPUnit\Framework\TestCase;
use Thrift\Exception\TApplicationException;
use Thrift\Protocol\TBinaryProtocol;
use Thrift\Transport\TMemoryBuffer;
use Thrift\Type\TType;

class TApplicationExceptionTest extends TestCase
{
    public function testWriteAndReadRoundTripMessageAndCode(): void
    {
        $transport = new TMemoryBuffer();
        $writer = new TBinaryProtocol($transport);

        $written = new TApplicationException(
            'boom',
            TApplicationException::INTERNAL_ERROR
        );
        $written->write($writer);

        $read = new TApplicationException();
        $read->read(new TBinaryProtocol($transport));

        $this->assertSame('boom', $read->getMessage());
        $this->assertSame(TApplicationException::INTERNAL_ERROR, $read->getCode());
    }

    public function testWriteOmitsFalsyMessageAndCodeFields(): void
    {
        $transport = new TMemoryBuffer();
        $protocol = new TBinaryProtocol($transport);

        (new TApplicationException())->write($protocol);

        $this->assertSame(chr(TType::STOP), $transport->getBuffer());
    }
}
