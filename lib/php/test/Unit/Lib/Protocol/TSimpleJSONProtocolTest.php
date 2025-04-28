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
use Thrift\Exception\TException;
use Thrift\Protocol\TSimpleJSONProtocol;
use Thrift\Transport\TTransport;
use Thrift\Type\TType;

class TSimpleJSONProtocolTest extends TestCase
{
    /**
     * Reading methods.
     *
     * simplejson is not meant to be read back into thrift
     * - see http://wiki.apache.org/thrift/ThriftUsageJava
     * - use JSON instead
     *
     * @dataProvider readDataProvider
     */
    public function testRead(
        $methodName,
        $methodArguments
    ) {
        $this->expectException(TException::class);
        $this->expectExceptionMessage("Not implemented");

        $transport = $this->createMock(TTransport::class);
        $protocol = new TSimpleJSONProtocol($transport);
        $protocol->$methodName(...$methodArguments);
    }

    public function readDataProvider()
    {
        yield 'readMessageBegin' => [
            'methodName' => 'readMessageBegin',
            'methodArguments' => ['name', 'type', 'seqId'],
        ];
        yield 'readMessageEnd' => [
            'methodName' => 'readMessageEnd',
            'methodArguments' => [],
        ];
        yield 'readStructBegin' => [
            'methodName' => 'readStructBegin',
            'methodArguments' => ['name'],
        ];
        yield 'readStructEnd' => [
            'methodName' => 'readStructEnd',
            'methodArguments' => [],
        ];
        yield 'readFieldBegin' => [
            'methodName' => 'readFieldBegin',
            'methodArguments' => ['name', TType::STRING, 1],
        ];
        yield 'readFieldEnd' => [
            'methodName' => 'readFieldEnd',
            'methodArguments' => [],
        ];
        yield 'readMapBegin' => [
            'methodName' => 'readMapBegin',
            'methodArguments' => [TType::STRING, TType::STRING, 1],
        ];
        yield 'readMapEnd' => [
            'methodName' => 'readMapEnd',
            'methodArguments' => [],
        ];
        yield 'readListBegin' => [
            'methodName' => 'readListBegin',
            'methodArguments' => [TType::STRING, 1],
        ];
        yield 'readListEnd' => [
            'methodName' => 'readListEnd',
            'methodArguments' => [],
        ];
        yield 'readSetBegin' => [
            'methodName' => 'readSetBegin',
            'methodArguments' => [TType::STRING, 1],
        ];
        yield 'readSetEnd' => [
            'methodName' => 'readSetEnd',
            'methodArguments' => [],
        ];
        yield 'readBool' => [
            'methodName' => 'readBool',
            'methodArguments' => [true],
        ];
        yield 'readByte' => [
            'methodName' => 'readByte',
            'methodArguments' => [0x01],
        ];
        yield 'readI16' => [
            'methodName' => 'readI16',
            'methodArguments' => [1],
        ];
        yield 'readI32' => [
            'methodName' => 'readI32',
            'methodArguments' => [1],
        ];
        yield 'readI64' => [
            'methodName' => 'readI64',
            'methodArguments' => [1],
        ];
        yield 'readDouble' => [
            'methodName' => 'readDouble',
            'methodArguments' => [0.1],
        ];
        yield 'readString' => [
            'methodName' => 'readString',
            'methodArguments' => ['string'],
        ];
    }
}
