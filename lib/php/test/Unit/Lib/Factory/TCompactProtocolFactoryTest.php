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
use Test\Thrift\Unit\Lib\ReflectionHelper;
use Thrift\Factory\TCompactProtocolFactory;
use Thrift\Protocol\TCompactProtocol;
use Thrift\Transport\TTransport;

class TCompactProtocolFactoryTest extends TestCase
{
    use ReflectionHelper;

    /**
     * @return void
     */
    public function testGetProtocol()
    {
        $transport = $this->createStub(TTransport::class);
        $factory = new TCompactProtocolFactory();
        $protocol = $factory->getProtocol($transport);

        $this->assertInstanceOf(TCompactProtocol::class, $protocol);

        $this->assertSame($transport, $this->getPropertyValue($protocol, 'trans'));
    }

    /**
     * @return void
     */
    public function testGetTransport()
    {
        $transport = $this->createStub(TTransport::class);
        $factory = new TCompactProtocolFactory();
        $protocol = $factory->getProtocol($transport);

        $this->assertSame($transport, $protocol->getTransport());
    }

    /**
     * @return void
     */
    public function testGetProtocolCreatesNewInstancePerCall()
    {
        $transport = $this->createStub(TTransport::class);
        $factory = new TCompactProtocolFactory();

        $protocol1 = $factory->getProtocol($transport);
        $protocol2 = $factory->getProtocol($transport);

        $this->assertNotSame($protocol1, $protocol2);
    }
}
