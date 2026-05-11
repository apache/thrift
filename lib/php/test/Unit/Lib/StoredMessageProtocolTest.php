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

namespace Test\Thrift\Unit\Lib;

use PHPUnit\Framework\TestCase;
use Thrift\Protocol\TProtocol;
use Thrift\StoredMessageProtocol;
use Thrift\Transport\TTransport;
use Thrift\Type\TMessageType;

class StoredMessageProtocolTest extends TestCase
{
    public function testReadMessageBeginReturnsStoredValues(): void
    {
        $transport = $this->createStub(TTransport::class);
        $protocol = $this->createMock(TProtocol::class);
        $protocol->expects($this->once())
            ->method('getTransport')
            ->willReturn($transport);

        $stored = new StoredMessageProtocol($protocol, 'ping', TMessageType::ONEWAY, 99);

        $name = '';
        $type = 0;
        $seqid = 0;
        $stored->readMessageBegin($name, $type, $seqid);

        $this->assertSame('ping', $name);
        $this->assertSame(TMessageType::ONEWAY, $type);
        $this->assertSame(99, $seqid);
        $this->assertSame($transport, $stored->getTransport());
    }
}
