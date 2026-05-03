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

namespace Test\Thrift\Unit\Lib\Factory;

use PHPUnit\Framework\TestCase;
use PHPUnit\Framework\Attributes\DataProvider;
use Test\Thrift\Unit\Lib\ReflectionHelper;
use Thrift\Factory\TBinaryProtocolFactory;
use Thrift\Protocol\TBinaryProtocol;
use Thrift\Transport\TTransport;

class TBinaryProtocolFactoryTest extends TestCase
{
    use ReflectionHelper;

    /**
     * @param bool $strictRead
     * @param bool $strictWrite
     * @return void
     */
    #[DataProvider('getProtocolDataProvider')]
    public function testGetProtocol(
        $strictRead,
        $strictWrite
    ) {
        $transport = $this->createStub(TTransport::class);
        $factory = new TBinaryProtocolFactory($strictRead, $strictWrite);
        $protocol = $factory->getProtocol($transport);

        $this->assertInstanceOf(TBinaryProtocol::class, $protocol);

        $this->assertEquals($strictRead, $this->getPropertyValue($protocol, 'strictRead_'));
        $this->assertEquals($strictWrite, $this->getPropertyValue($protocol, 'strictWrite_'));
        $this->assertSame($transport, $this->getPropertyValue($protocol, 'trans_'));
    }

    public static function getProtocolDataProvider()
    {
        yield 'allTrue' => [
            'strictRead' => true,
            'strictWrite' => true,
        ];
        yield 'allFalse' => [
            'strictRead' => false,
            'strictWrite' => false,
        ];
        yield 'strictReadTrue' => [
            'strictRead' => true,
            'strictWrite' => false,
        ];
        yield 'strictWriteTrue' => [
            'strictRead' => false,
            'strictWrite' => true,
        ];
    }

    /**
     * @return void
     */
    public function testGetTransport()
    {
        $transport = $this->createStub(TTransport::class);
        $factory = new TBinaryProtocolFactory();
        $protocol = $factory->getProtocol($transport);

        $this->assertSame($transport, $protocol->getTransport());
    }

    /**
     * @return void
     */
    public function testGetProtocolCreatesNewInstancePerCall()
    {
        $transport = $this->createStub(TTransport::class);
        $factory = new TBinaryProtocolFactory();

        $protocol1 = $factory->getProtocol($transport);
        $protocol2 = $factory->getProtocol($transport);

        $this->assertNotSame($protocol1, $protocol2);
    }
}
