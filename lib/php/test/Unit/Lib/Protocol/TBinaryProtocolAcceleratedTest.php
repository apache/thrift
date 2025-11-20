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
 *
 */

namespace Test\Thrift\Unit\Lib\Protocol;

use PHPUnit\Framework\TestCase;
use Thrift\Protocol\TBinaryProtocolAccelerated;
use Thrift\Transport\TBufferedTransport;
use Thrift\Transport\TMemoryBuffer;
use Thrift\Transport\TSocket;

class TBinaryProtocolAcceleratedTest extends TestCase
{
    /**
     * @dataProvider constructDataProvider
     */
    public function testConstruct(
        $transport,
        $expectedTransport
    ) {
        $protocol = new TBinaryProtocolAccelerated($transport);
        $this->assertInstanceOf($expectedTransport, $protocol->getTransport());
    }

    public function constructDataProvider()
    {
        yield 'not buffered transport' => [
            'transport' => new TMemoryBuffer(),
            'expectedTransport' => TMemoryBuffer::class,
        ];
        yield 'buffered transport' => [
            'transport' => new TSocket(),
            'expectedTransport' => TBufferedTransport::class,
        ];
    }

    /**
     * @dataProvider strictParamsDataProvider
     */
    public function testStrictParams($strictRead, $strictWrite)
    {
        $protocol = new TBinaryProtocolAccelerated(new TMemoryBuffer(), $strictRead, $strictWrite);
        $this->assertEquals($strictRead, $protocol->isStrictRead());
        $this->assertEquals($strictWrite, $protocol->isStrictWrite());
    }

    public function strictParamsDataProvider()
    {
        yield 'strict read and write' => [true, true];
        yield 'not strict read and write' => [false, false];
        yield 'strict read and not strict write' => [true, false];
        yield 'not strict read and strict write' => [false, true];
    }
}
