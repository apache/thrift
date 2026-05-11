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

namespace Test\Thrift\Unit\Lib\Protocol;

use PHPUnit\Framework\TestCase;
use PHPUnit\Framework\Attributes\DataProvider;
use Thrift\Exception\TProtocolException;
use Thrift\Protocol\TBinaryProtocol;
use Thrift\Protocol\TProtocol;
use Thrift\Transport\TMemoryBuffer;
use Thrift\Type\TType;

class TProtocolTest extends TestCase
{
    #[DataProvider('skipScalarDataProvider')]
    public function testSkipScalarValues(int $type, string $writerMethod, $value): void
    {
        $buffer = $this->buildBinaryBuffer(
            function (TBinaryProtocol $protocol) use ($writerMethod, $value): void {
                $protocol->{$writerMethod}($value);
            }
        );
        $transport = new TMemoryBuffer($buffer);
        $protocol = new TBinaryProtocol($transport);

        $this->assertSame(strlen($buffer), $protocol->skip($type));
        $this->assertSame(0, (int)$transport->available());
    }

    public static function skipScalarDataProvider(): iterable
    {
        yield 'bool' => [
            TType::BOOL,
            'writeBool',
            true,
        ];
        yield 'byte' => [
            TType::BYTE,
            'writeByte',
            7,
        ];
        yield 'i16' => [
            TType::I16,
            'writeI16',
            1024,
        ];
        yield 'i32' => [
            TType::I32,
            'writeI32',
            65536,
        ];
        yield 'i64' => [
            TType::I64,
            'writeI64',
            1099511627776,
        ];
        yield 'double' => [
            TType::DOUBLE,
            'writeDouble',
            3.14,
        ];
        yield 'string' => [
            TType::STRING,
            'writeString',
            'skip me',
        ];
        yield 'uuid' => [
            TType::UUID,
            'writeUuid',
            '12345678-1234-5678-1234-567812345678',
        ];
    }

    public function testSkipStruct(): void
    {
        $buffer = $this->buildBinaryBuffer(
            function (TBinaryProtocol $protocol): void {
                $protocol->writeStructBegin('Example');
                $protocol->writeFieldBegin('flag', TType::BOOL, 1);
                $protocol->writeBool(true);
                $protocol->writeFieldEnd();
                $protocol->writeFieldBegin('name', TType::STRING, 2);
                $protocol->writeString('value');
                $protocol->writeFieldEnd();
                $protocol->writeFieldStop();
                $protocol->writeStructEnd();
            }
        );

        $transport = new TMemoryBuffer($buffer);
        $protocol = new TBinaryProtocol($transport);

        $this->assertSame(strlen($buffer), $protocol->skip(TType::STRUCT));
        $this->assertSame(0, (int)$transport->available());
    }

    public function testSkipMap(): void
    {
        $buffer = $this->buildBinaryBuffer(
            function (TBinaryProtocol $protocol): void {
                $protocol->writeMapBegin(TType::I32, TType::STRING, 2);
                $protocol->writeI32(1);
                $protocol->writeString('a');
                $protocol->writeI32(2);
                $protocol->writeString('bc');
                $protocol->writeMapEnd();
            }
        );

        $transport = new TMemoryBuffer($buffer);
        $protocol = new TBinaryProtocol($transport);

        $this->assertSame(strlen($buffer), $protocol->skip(TType::MAP));
        $this->assertSame(0, (int)$transport->available());
    }

    public function testSkipSet(): void
    {
        $buffer = $this->buildBinaryBuffer(
            function (TBinaryProtocol $protocol): void {
                $protocol->writeSetBegin(TType::I16, 2);
                $protocol->writeI16(10);
                $protocol->writeI16(20);
                $protocol->writeSetEnd();
            }
        );

        $transport = new TMemoryBuffer($buffer);
        $protocol = new TBinaryProtocol($transport);

        $this->assertSame(strlen($buffer), $protocol->skip(TType::SET));
        $this->assertSame(0, (int)$transport->available());
    }

    public function testSkipList(): void
    {
        $buffer = $this->buildBinaryBuffer(
            function (TBinaryProtocol $protocol): void {
                $protocol->writeListBegin(TType::STRING, 2);
                $protocol->writeString('first');
                $protocol->writeString('second');
                $protocol->writeListEnd();
            }
        );

        $transport = new TMemoryBuffer($buffer);
        $protocol = new TBinaryProtocol($transport);

        $this->assertSame(strlen($buffer), $protocol->skip(TType::LST));
        $this->assertSame(0, (int)$transport->available());
    }

    public function testSkipThrowsForUnknownType(): void
    {
        $protocol = new TBinaryProtocol(new TMemoryBuffer());

        $this->expectException(TProtocolException::class);
        $this->expectExceptionCode(TProtocolException::INVALID_DATA);

        $protocol->skip(999);
    }

    #[DataProvider('skipScalarDataProvider')]
    public function testSkipBinaryScalarValues(int $type, string $writerMethod, $value): void
    {
        $buffer = $this->buildBinaryBuffer(
            function (TBinaryProtocol $protocol) use ($writerMethod, $value): void {
                $protocol->{$writerMethod}($value);
            }
        );

        $this->assertSkipBinaryConsumesBuffer($buffer, $type);
    }

    public function testSkipBinaryStruct(): void
    {
        $buffer = $this->buildBinaryBuffer(
            function (TBinaryProtocol $protocol): void {
                $protocol->writeStructBegin('Example');
                $protocol->writeFieldBegin('flag', TType::BOOL, 1);
                $protocol->writeBool(true);
                $protocol->writeFieldEnd();
                $protocol->writeFieldBegin('name', TType::STRING, 2);
                $protocol->writeString('value');
                $protocol->writeFieldEnd();
                $protocol->writeFieldStop();
                $protocol->writeStructEnd();
            }
        );

        $this->assertSkipBinaryConsumesBuffer($buffer, TType::STRUCT);
    }

    public function testSkipBinaryMap(): void
    {
        $buffer = $this->buildBinaryBuffer(
            function (TBinaryProtocol $protocol): void {
                $protocol->writeMapBegin(TType::I32, TType::STRING, 2);
                $protocol->writeI32(1);
                $protocol->writeString('a');
                $protocol->writeI32(2);
                $protocol->writeString('bc');
                $protocol->writeMapEnd();
            }
        );

        $this->assertSkipBinaryConsumesBuffer($buffer, TType::MAP);
    }

    public function testSkipBinaryList(): void
    {
        $buffer = $this->buildBinaryBuffer(
            function (TBinaryProtocol $protocol): void {
                $protocol->writeListBegin(TType::STRING, 2);
                $protocol->writeString('first');
                $protocol->writeString('second');
                $protocol->writeListEnd();
            }
        );

        $this->assertSkipBinaryConsumesBuffer($buffer, TType::LST);
    }

    public function testSkipBinarySet(): void
    {
        $buffer = $this->buildBinaryBuffer(
            function (TBinaryProtocol $protocol): void {
                $protocol->writeSetBegin(TType::DOUBLE, 2);
                $protocol->writeDouble(3.14);
                $protocol->writeDouble(6.28);
                $protocol->writeSetEnd();
            }
        );

        $this->assertSkipBinaryConsumesBuffer($buffer, TType::SET);
    }

    public function testSkipBinaryThrowsForUnknownType(): void
    {
        $this->expectException(TProtocolException::class);
        $this->expectExceptionCode(TProtocolException::INVALID_DATA);

        TProtocol::skipBinary(new TMemoryBuffer(), 999);
    }

    private function buildBinaryBuffer(callable $writer): string
    {
        $transport = new TMemoryBuffer();
        $protocol = new TBinaryProtocol($transport);
        $writer($protocol);

        return $transport->getBuffer();
    }

    private function assertSkipBinaryConsumesBuffer(string $buffer, int $type): void
    {
        $transport = new TMemoryBuffer($buffer);

        $this->assertSame(strlen($buffer), TProtocol::skipBinary($transport, $type));
        $this->assertSame(0, (int)$transport->available());
    }
}
