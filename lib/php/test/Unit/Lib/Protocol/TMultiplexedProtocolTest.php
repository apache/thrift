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
use Thrift\Protocol\TMultiplexedProtocol;
use Thrift\Protocol\TProtocol;
use Thrift\Type\TMessageType;

class TMultiplexedProtocolTest extends TestCase
{

    /**
     * @dataProvider writeMessageBeginDataProvider
     */
    public function testWriteMessageBegin(
        $serviceName,
        $name,
        $type,
        $seqid,
        $expectedName
    ) {
        $protocol = $this->createMock(TProtocol::class);
        $multiplexedProtocol = new TMultiplexedProtocol($protocol, $serviceName);

        $protocol->expects($this->once())
            ->method('writeMessageBegin')
            ->with($expectedName, $type, $seqid);

        $multiplexedProtocol->writeMessageBegin($name, $type, $seqid);
    }

    public function writeMessageBeginDataProvider()
    {
        yield 'messageTypeCall' => [
            'serviceName' => 'serviceName',
            'name' => 'testName',
            'type' => TMessageType::CALL,
            'seqid' => 1,
            'expectedName' => 'serviceName:testName'
        ];
        yield 'messageTypeOneWay' => [
            'serviceName' => 'serviceName',
            'name' => 'testName',
            'type' => TMessageType::ONEWAY,
            'seqid' => 1,
            'expectedName' => 'serviceName:testName'
        ];
        yield 'messageTypeReply' => [
            'serviceName' => 'serviceName',
            'name' => 'testName',
            'type' => TMessageType::REPLY,
            'seqid' => 1,
            'expectedName' => 'testName'
        ];
        yield 'messageTypeException' => [
            'serviceName' => 'serviceName',
            'name' => 'testName',
            'type' => TMessageType::EXCEPTION,
            'seqid' => 1,
            'expectedName' => 'testName'
        ];

    }
}
