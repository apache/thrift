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
use Thrift\Protocol\TProtocol;
use Thrift\Protocol\TProtocolDecorator;

class TProtocolDecoratorTest extends TestCase
{

    /**
     * @dataProvider methodDecorationDataProvider
     */
    public function testMethodDecoration(
        $methodName,
        $methodArguments
    ) {
        $concreteProtocol = $this->createMock(TProtocol::class);
        $decorator = new class ($concreteProtocol) extends TProtocolDecorator {
            public function __construct(TProtocol $protocol)
            {
                parent::__construct($protocol);
            }
        };

        $concreteProtocol->expects($this->once())
                         ->method($methodName)
                         ->with(...$methodArguments);

        $decorator->$methodName(...$methodArguments);
    }

    public function methodDecorationDataProvider()
    {
        yield 'writeMessageBegin' => ['writeMessageBegin', ['name', 'type', 'seqid']];
        yield 'writeMessageEnd' => ['writeMessageEnd', []];
        yield 'writeStructBegin' => ['writeStructBegin', ['name']];
        yield 'writeStructEnd' => ['writeStructEnd', []];
        yield 'writeFieldBegin' => ['writeFieldBegin', ['name', 'type', 'id']];
        yield 'writeFieldEnd' => ['writeFieldEnd', []];
        yield 'writeFieldStop' => ['writeFieldStop', []];
        yield 'writeMapBegin' => ['writeMapBegin', ['keyType', 'valType', 'size']];
        yield 'writeMapEnd' => ['writeMapEnd', []];
        yield 'writeListBegin' => ['writeListBegin', ['elemType', 'size']];
        yield 'writeListEnd' => ['writeListEnd', []];
        yield 'writeSetBegin' => ['writeSetBegin', ['elemType', 'size']];
        yield 'writeSetEnd' => ['writeSetEnd', []];
        yield 'writeBool' => ['writeBool', ['value']];
        yield 'writeByte' => ['writeByte', ['value']];
        yield 'writeI16' => ['writeI16', ['value']];
        yield 'writeI32' => ['writeI32', ['value']];
        yield 'writeI64' => ['writeI64', ['value']];
        yield 'writeDouble' => ['writeDouble', ['value']];
        yield 'writeString' => ['writeString', ['value']];
        yield 'readMessageBegin' => ['readMessageBegin', ['name', 'type', 'seqid']];
        yield 'readMessageEnd' => ['readMessageEnd', []];
        yield 'readStructBegin' => ['readStructBegin', ['name']];
        yield 'readStructEnd' => ['readStructEnd', []];
        yield 'readFieldBegin' => ['readFieldBegin', ['name', 'type', 'id']];
        yield 'readFieldEnd' => ['readFieldEnd', []];
        yield 'readMapBegin' => ['readMapBegin', ['keyType', 'valType', 'size']];
        yield 'readMapEnd' => ['readMapEnd', []];
        yield 'readListBegin' => ['readListBegin', ['elemType', 'size']];
        yield 'readListEnd' => ['readListEnd', []];
        yield 'readSetBegin' => ['readSetBegin', ['elemType', 'size']];
        yield 'readSetEnd' => ['readSetEnd', []];
        yield 'readBool' => ['readBool', ['value']];
        yield 'readByte' => ['readByte', ['value']];
        yield 'readI16' => ['readI16', ['value']];
        yield 'readI32' => ['readI32', ['value']];
        yield 'readI64' => ['readI64', ['value']];
        yield 'readDouble' => ['readDouble', ['value']];
        yield 'readString' => ['readString', ['value']];
    }
}
