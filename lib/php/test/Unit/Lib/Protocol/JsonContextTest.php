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

namespace Test\Thrift\Unit\Lib\Protocol;

use PHPUnit\Framework\TestCase;
use Test\Thrift\Unit\Lib\Protocol\Fixture\JsonProtocolStub;
use Thrift\Protocol\JSON\BaseContext;
use Thrift\Protocol\JSON\ListContext;
use Thrift\Protocol\JSON\LookaheadReader;
use Thrift\Protocol\JSON\PairContext;
use Thrift\Transport\TMemoryBuffer;
use Thrift\Transport\TTransport;

class JsonContextTest extends TestCase
{
    public function testBaseContextDefaults(): void
    {
        $context = new BaseContext();

        $this->assertFalse($context->escapeNum());
        $this->assertNull($context->write());
        $this->assertNull($context->read());
    }

    public function testListContextWritesCommasAfterFirstElement(): void
    {
        $transport = new TMemoryBuffer();
        $context = new ListContext(new JsonProtocolStub($transport));

        $context->write();
        $context->write();
        $context->write();

        $this->assertSame(',,', $transport->getBuffer());
    }

    public function testListContextReadsCommasAfterFirstElement(): void
    {
        $protocol = new JsonProtocolStub(new TMemoryBuffer());
        $context = new ListContext($protocol);

        $context->read();
        $context->read();
        $context->read();

        $this->assertCount(2, $protocol->readChars);
        $this->assertSame([',', ','], $protocol->readChars);
    }

    public function testPairContextWritesColonsAndCommas(): void
    {
        $transport = new TMemoryBuffer();
        $context = new PairContext(new JsonProtocolStub($transport));

        $this->assertTrue($context->escapeNum());

        $context->write();
        $this->assertTrue($context->escapeNum());

        $context->write();
        $this->assertFalse($context->escapeNum());

        $context->write();
        $this->assertTrue($context->escapeNum());

        $this->assertSame(':,', $transport->getBuffer());
    }

    public function testPairContextReadsColonsAndCommas(): void
    {
        $protocol = new JsonProtocolStub(new TMemoryBuffer());
        $context = new PairContext($protocol);

        $context->read();
        $context->read();
        $context->read();

        $this->assertCount(2, $protocol->readChars);
        $this->assertSame([':', ','], $protocol->readChars);
    }

    public function testLookaheadReaderCachesPeekedValue(): void
    {
        $transport = $this->createMock(TTransport::class);
        $transport->expects($this->exactly(2))
            ->method('readAll')
            ->with(1)
            ->willReturnOnConsecutiveCalls('a', 'b');

        $reader = new LookaheadReader(new JsonProtocolStub($transport));

        $this->assertSame('a', $reader->peek());
        $this->assertSame('a', $reader->peek());
        $this->assertSame('a', $reader->read());
        $this->assertSame('b', $reader->read());
    }
}
