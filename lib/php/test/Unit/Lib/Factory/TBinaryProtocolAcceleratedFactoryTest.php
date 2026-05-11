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

namespace Test\Thrift\Unit\Lib\Factory;

use PHPUnit\Framework\TestCase;
use PHPUnit\Framework\Attributes\DataProvider;
use Test\Thrift\Unit\Lib\ReflectionHelper;
use Thrift\Factory\TBinaryProtocolAcceleratedFactory;
use Thrift\Protocol\TBinaryProtocolAccelerated;
use Thrift\Transport\TBufferedTransport;
use Thrift\Transport\TTransport;

class TBinaryProtocolAcceleratedFactoryTest extends TestCase
{
    use ReflectionHelper;

    #[DataProvider('getProtocolDataProvider')]
    public function testGetProtocol(bool $strictRead, bool $strictWrite): void
    {
        $transport = $this->createStub(TBufferedTransport::class);
        $factory = new TBinaryProtocolAcceleratedFactory($strictRead, $strictWrite);
        $protocol = $factory->getProtocol($transport);

        $this->assertInstanceOf(TBinaryProtocolAccelerated::class, $protocol);
        $this->assertEquals($strictRead, $this->getPropertyValue($protocol, 'strictRead'));
        $this->assertEquals($strictWrite, $this->getPropertyValue($protocol, 'strictWrite'));
    }

    public static function getProtocolDataProvider(): \Generator
    {
        yield 'defaults' => ['strictRead' => false, 'strictWrite' => true];
        yield 'allTrue' => ['strictRead' => true, 'strictWrite' => true];
        yield 'allFalse' => ['strictRead' => false, 'strictWrite' => false];
        yield 'strictReadOnly' => ['strictRead' => true, 'strictWrite' => false];
    }

    public function testGetProtocolDefaultsMatchAcceleratedConstructor(): void
    {
        $transport = $this->createStub(TBufferedTransport::class);
        $protocol = (new TBinaryProtocolAcceleratedFactory())->getProtocol($transport);

        // TBinaryProtocolAccelerated defaults: strictRead=false, strictWrite=true.
        $this->assertFalse($this->getPropertyValue($protocol, 'strictRead'));
        $this->assertTrue($this->getPropertyValue($protocol, 'strictWrite'));
    }

    public function testNonBufferedTransportIsWrapped(): void
    {
        // TBinaryProtocolAccelerated wraps any transport without putBack() in TBufferedTransport.
        $rawTransport = $this->createStub(TTransport::class);
        $protocol = (new TBinaryProtocolAcceleratedFactory())->getProtocol($rawTransport);

        $this->assertInstanceOf(TBufferedTransport::class, $protocol->getTransport());
    }

    public function testGetProtocolCreatesNewInstancePerCall(): void
    {
        $transport = $this->createStub(TBufferedTransport::class);
        $factory = new TBinaryProtocolAcceleratedFactory();

        $this->assertNotSame($factory->getProtocol($transport), $factory->getProtocol($transport));
    }
}
