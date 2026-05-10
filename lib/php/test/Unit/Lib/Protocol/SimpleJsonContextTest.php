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
use Thrift\Exception\TException;
use Thrift\Protocol\SimpleJSON\CollectionMapKeyException;
use Thrift\Protocol\SimpleJSON\Context;
use Thrift\Protocol\SimpleJSON\ListContext;
use Thrift\Protocol\SimpleJSON\MapContext;
use Thrift\Protocol\SimpleJSON\StructContext;
use Thrift\Protocol\TSimpleJSONProtocol;
use Thrift\Transport\TMemoryBuffer;

class SimpleJsonContextTest extends TestCase
{
    public function testContextDefaults(): void
    {
        $context = new Context();

        $this->assertNull($context->write());
        $this->assertFalse($context->isMapKey());
    }

    public function testListContextWritesCommasAfterFirstElement(): void
    {
        $transport = new TMemoryBuffer();
        $context = new ListContext(new TSimpleJSONProtocol($transport));

        $context->write();
        $context->write();
        $context->write();

        $this->assertSame(',,', $transport->getBuffer());
    }

    public function testStructContextWritesColonsAndCommas(): void
    {
        $transport = new TMemoryBuffer();
        $context = new StructContext(new TSimpleJSONProtocol($transport));

        $context->write();
        $context->write();
        $context->write();

        $this->assertSame(':,', $transport->getBuffer());
    }

    public function testMapContextTogglesMapKeyState(): void
    {
        $transport = new TMemoryBuffer();
        $context = new MapContext(new TSimpleJSONProtocol($transport));

        $this->assertTrue($context->isMapKey());

        $context->write();
        $this->assertFalse($context->isMapKey());

        $context->write();
        $this->assertTrue($context->isMapKey());

        $context->write();
        $this->assertFalse($context->isMapKey());

        $this->assertSame(':,', $transport->getBuffer());
    }

    public function testCollectionMapKeyExceptionPreservesMessage(): void
    {
        $exception = new CollectionMapKeyException('bad key');

        $this->assertInstanceOf(TException::class, $exception);
        $this->assertSame('bad key', $exception->getMessage());
    }
}
