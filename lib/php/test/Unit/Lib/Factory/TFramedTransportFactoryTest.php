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
use Thrift\Factory\TFramedTransportFactory;
use Thrift\Transport\TFramedTransport;
use Thrift\Transport\TTransport;

class TFramedTransportFactoryTest extends TestCase
{
    /**
     * @return void
     */
    public function testGetTransport()
    {
        $transport = $this->createMock(TTransport::class);
        $factory = new TFramedTransportFactory();
        $framedTransport = $factory->getTransport($transport);

        $this->assertInstanceOf(TFramedTransport::class, $framedTransport);

        $ref = new \ReflectionClass($framedTransport);
        $refRead = $ref->getProperty('read_');
        $refRead->setAccessible(true);
        $refWrite = $ref->getProperty('write_');
        $refWrite->setAccessible(true);
        $refTrans = $ref->getProperty('transport_');
        $refTrans->setAccessible(true);

        $this->assertTrue($refRead->getValue($framedTransport));
        $this->assertTrue($refWrite->getValue($framedTransport));
        $this->assertSame($transport, $refTrans->getValue($framedTransport));
    }
}
