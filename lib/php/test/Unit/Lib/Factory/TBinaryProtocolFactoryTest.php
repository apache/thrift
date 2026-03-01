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
use Thrift\Factory\TBinaryProtocolFactory;
use Thrift\Protocol\TBinaryProtocol;
use Thrift\Transport\TTransport;

class TBinaryProtocolFactoryTest extends TestCase
{
    /**
     * @dataProvider getProtocolDataProvider
     * @param bool $strictRead
     * @param bool $strictWrite
     * @return void
     */
    public function testGetProtocol(
        $strictRead,
        $strictWrite
    ) {
        $transport = $this->createMock(TTransport::class);
        $factory = new TBinaryProtocolFactory($strictRead, $strictWrite);
        $protocol = $factory->getProtocol($transport);

        $this->assertInstanceOf(TBinaryProtocol::class, $protocol);

        $ref = new \ReflectionClass($protocol);
        $refStrictRead = $ref->getProperty('strictRead_');
        $refStrictRead->setAccessible(true);
        $refStrictWrite = $ref->getProperty('strictWrite_');
        $refStrictWrite->setAccessible(true);
        $refTrans = $ref->getProperty('trans_');
        $refTrans->setAccessible(true);

        $this->assertEquals($strictRead, $refStrictRead->getValue($protocol));
        $this->assertEquals($strictWrite, $refStrictWrite->getValue($protocol));
        $this->assertSame($transport, $refTrans->getValue($protocol));
    }

    public function getProtocolDataProvider()
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
}
