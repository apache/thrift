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

namespace Test\Thrift\Unit\Lib\Server;

use PHPUnit\Framework\TestCase;
use ReflectionProperty;
use Test\Thrift\Unit\Lib\Server\Fixture\ServerStub;
use Thrift\Factory\TProtocolFactory;
use Thrift\Factory\TTransportFactoryInterface;
use Thrift\Server\TServerTransport;

class TServerTest extends TestCase
{
    public function testConstructorStoresCollaborators(): void
    {
        $processor = new \stdClass();
        $transport = $this->createStub(TServerTransport::class);
        $inputTransportFactory = $this->createStub(TTransportFactoryInterface::class);
        $outputTransportFactory = $this->createStub(TTransportFactoryInterface::class);
        $inputProtocolFactory = $this->createStub(TProtocolFactory::class);
        $outputProtocolFactory = $this->createStub(TProtocolFactory::class);

        $server = new ServerStub(
            $processor,
            $transport,
            $inputTransportFactory,
            $outputTransportFactory,
            $inputProtocolFactory,
            $outputProtocolFactory
        );

        $this->assertSame($processor, (new ReflectionProperty($server, 'processor'))->getValue($server));
        $this->assertSame($transport, (new ReflectionProperty($server, 'transport'))->getValue($server));
        $this->assertSame(
            $inputTransportFactory,
            (new ReflectionProperty($server, 'inputTransportFactory'))->getValue($server)
        );
        $this->assertSame(
            $outputTransportFactory,
            (new ReflectionProperty($server, 'outputTransportFactory'))->getValue($server)
        );
        $this->assertSame(
            $inputProtocolFactory,
            (new ReflectionProperty($server, 'inputProtocolFactory'))->getValue($server)
        );
        $this->assertSame(
            $outputProtocolFactory,
            (new ReflectionProperty($server, 'outputProtocolFactory'))->getValue($server)
        );
    }
}
