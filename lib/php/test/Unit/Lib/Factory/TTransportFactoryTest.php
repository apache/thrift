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
use Thrift\Factory\TTransportFactory;
use Thrift\Transport\TTransport;

class TTransportFactoryTest extends TestCase
{
    /**
     * @return void
     */
    public function testGetTransport()
    {
        $transport = $this->createStub(TTransport::class);
        $factory = new TTransportFactory();
        $result = $factory->getTransport($transport);

        $this->assertSame($transport, $result);
    }

    /**
     * @return void
     */
    public function testGetTransportCreatesNewInstancePerCall()
    {
        $factory = new TTransportFactory();

        $transport1 = $this->createStub(TTransport::class);
        $transport2 = $this->createStub(TTransport::class);

        $this->assertSame($transport1, $factory->getTransport($transport1));
        $this->assertSame($transport2, $factory->getTransport($transport2));
        $this->assertNotSame(
            $factory->getTransport($transport1),
            $factory->getTransport($transport2)
        );
    }
}
