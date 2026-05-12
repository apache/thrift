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

declare(strict_types=1);

namespace Test\Thrift\Unit\Lib\Protocol;

use PHPUnit\Framework\TestCase;
use PHPUnit\Framework\Attributes\DataProvider;
use PHPUnit\Framework\MockObject\MockObject;
use Thrift\Protocol\TProtocol;
use Thrift\Protocol\TProtocolDecorator;

class TProtocolDecoratorTest extends TestCase
{
    private MockObject $concreteProtocol;
    private TProtocolDecorator $decorator;

    protected function setUp(): void
    {
        $this->concreteProtocol = $this->createMock(TProtocol::class);
        $this->decorator = new class ($this->concreteProtocol) extends TProtocolDecorator {
            public function __construct(TProtocol $protocol)
            {
                parent::__construct($protocol);
            }
        };
    }

    #[DataProvider('writeMethodDecorationDataProvider')]
    public function testWriteMethodDecoration(string $methodName, array $methodArguments): void
    {
        $this->concreteProtocol->expects($this->once())
                               ->method($methodName)
                               ->with(...$methodArguments);

        $this->decorator->$methodName(...$methodArguments);
    }

    public static function writeMethodDecorationDataProvider(): \Generator
    {
        yield 'writeMessageBegin' => ['writeMessageBegin', ['name', 1, 0]];
        yield 'writeMessageEnd' => ['writeMessageEnd', []];
        yield 'writeStructBegin' => ['writeStructBegin', ['name']];
        yield 'writeStructEnd' => ['writeStructEnd', []];
        yield 'writeFieldBegin' => ['writeFieldBegin', ['name', 1, 1]];
        yield 'writeFieldEnd' => ['writeFieldEnd', []];
        yield 'writeFieldStop' => ['writeFieldStop', []];
        yield 'writeMapBegin' => ['writeMapBegin', [1, 1, 0]];
        yield 'writeMapEnd' => ['writeMapEnd', []];
        yield 'writeListBegin' => ['writeListBegin', [1, 0]];
        yield 'writeListEnd' => ['writeListEnd', []];
        yield 'writeSetBegin' => ['writeSetBegin', [1, 0]];
        yield 'writeSetEnd' => ['writeSetEnd', []];
        yield 'writeBool' => ['writeBool', [true]];
        yield 'writeByte' => ['writeByte', [1]];
        yield 'writeI16' => ['writeI16', [1]];
        yield 'writeI32' => ['writeI32', [1]];
        yield 'writeI64' => ['writeI64', [1]];
        yield 'writeDouble' => ['writeDouble', [1.0]];
        yield 'writeString' => ['writeString', ['value']];
        yield 'writeUuid' => ['writeUuid', ['00000000-0000-0000-0000-000000000000']];
    }

    /**
     * Read methods take their results via nullable by-reference parameters
     * (e.g. readMessageBegin(?string &$name, ?int &$type, ?int &$seqid)).
     * Spreading an array preserves the by-ref binding into the array
     * element, which is all this delegation test needs to assert.
     */
    #[DataProvider('readMethodDecorationDataProvider')]
    public function testReadMethodDecoration(string $methodName, int $argCount): void
    {
        $this->concreteProtocol->expects($this->once())->method($methodName);

        $args = array_fill(0, $argCount, null);
        $this->decorator->$methodName(...$args);
    }

    public static function readMethodDecorationDataProvider(): \Generator
    {
        yield 'readMessageBegin' => ['readMessageBegin', 3];
        yield 'readMessageEnd' => ['readMessageEnd', 0];
        yield 'readStructBegin' => ['readStructBegin', 1];
        yield 'readStructEnd' => ['readStructEnd', 0];
        yield 'readFieldBegin' => ['readFieldBegin', 3];
        yield 'readFieldEnd' => ['readFieldEnd', 0];
        yield 'readMapBegin' => ['readMapBegin', 3];
        yield 'readMapEnd' => ['readMapEnd', 0];
        yield 'readListBegin' => ['readListBegin', 2];
        yield 'readListEnd' => ['readListEnd', 0];
        yield 'readSetBegin' => ['readSetBegin', 2];
        yield 'readSetEnd' => ['readSetEnd', 0];
        yield 'readBool' => ['readBool', 1];
        yield 'readByte' => ['readByte', 1];
        yield 'readI16' => ['readI16', 1];
        yield 'readI32' => ['readI32', 1];
        yield 'readI64' => ['readI64', 1];
        yield 'readDouble' => ['readDouble', 1];
        yield 'readString' => ['readString', 1];
        yield 'readUuid' => ['readUuid', 1];
    }
}
