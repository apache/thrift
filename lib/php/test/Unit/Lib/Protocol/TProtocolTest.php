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

namespace Test\Thrift\Unit\Lib\Protocol;

use PHPUnit\Framework\TestCase;
use Thrift\Exception\TProtocolException;
use Thrift\Protocol\TBinaryProtocol;
use Thrift\Protocol\TProtocol;
use Thrift\Transport\TMemoryBuffer;
use Thrift\Transport\TTransport;
use Thrift\Type\TType;

class TProtocolTest extends TestCase
{
    /**
     * @dataProvider skipScalarDataProvider
     */
    public function testSkipScalarValues(int $type, string $writerMethod, $value): void
    {
        $buffer = $this->buildBinaryBuffer(function (TBinaryProtocol $protocol) use ($writerMethod, $value): void {
            $protocol->{$writerMethod}($value);
        });
        $transport = new TMemoryBuffer($buffer);
        $protocol = new TBinaryProtocol($transport);

        $this->assertSame(strlen($buffer), $protocol->skip($type));
        $this->assertSame(0, (int)$transport->available());
    }

    public function skipScalarDataProvider(): iterable
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
        $buffer = $this->buildBinaryBuffer(function (TBinaryProtocol $protocol): void {
            $protocol->writeStructBegin('Example');
            $protocol->writeFieldBegin('flag', TType::BOOL, 1);
            $protocol->writeBool(true);
            $protocol->writeFieldEnd();
            $protocol->writeFieldBegin('name', TType::STRING, 2);
            $protocol->writeString('value');
            $protocol->writeFieldEnd();
            $protocol->writeFieldStop();
            $protocol->writeStructEnd();
        });

        $transport = new TMemoryBuffer($buffer);
        $protocol = new TBinaryProtocol($transport);

        $this->assertSame(strlen($buffer), $protocol->skip(TType::STRUCT));
        $this->assertSame(0, (int)$transport->available());
    }

    public function testSkipMap(): void
    {
        $buffer = $this->buildBinaryBuffer(function (TBinaryProtocol $protocol): void {
            $protocol->writeMapBegin(TType::I32, TType::STRING, 2);
            $protocol->writeI32(1);
            $protocol->writeString('a');
            $protocol->writeI32(2);
            $protocol->writeString('bc');
            $protocol->writeMapEnd();
        });

        $transport = new TMemoryBuffer($buffer);
        $protocol = new TBinaryProtocol($transport);

        $this->assertSame(strlen($buffer), $protocol->skip(TType::MAP));
        $this->assertSame(0, (int)$transport->available());
    }

    public function testSkipSet(): void
    {
        $buffer = $this->buildBinaryBuffer(function (TBinaryProtocol $protocol): void {
            $protocol->writeSetBegin(TType::I16, 2);
            $protocol->writeI16(10);
            $protocol->writeI16(20);
            $protocol->writeSetEnd();
        });

        $transport = new TMemoryBuffer($buffer);
        $protocol = new TBinaryProtocol($transport);

        $this->assertSame(strlen($buffer), $protocol->skip(TType::SET));
        $this->assertSame(0, (int)$transport->available());
    }

    public function testSkipList(): void
    {
        $buffer = $this->buildBinaryBuffer(function (TBinaryProtocol $protocol): void {
            $protocol->writeListBegin(TType::STRING, 2);
            $protocol->writeString('first');
            $protocol->writeString('second');
            $protocol->writeListEnd();
        });

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

    public function testSkipBinarySkipsBool(): void
    {
        $transport = $this->createMock(TTransport::class);
        $transport->expects($this->once())
            ->method('readAll')
            ->with(1)
            ->willReturn('1');

        TProtocol::skipBinary($transport, TType::BOOL);
    }

    public function testSkipBinarySkipsString(): void
    {
        $transport = $this->createMock(TTransport::class);
        $transport->expects($this->exactly(2))
            ->method('readAll')
            ->withConsecutive([4], [2])
            ->willReturnOnConsecutiveCalls(pack('N', 2), '12');

        TProtocol::skipBinary($transport, TType::STRING);
    }

    public function testSkipBinarySkipsStruct(): void
    {
        $transport = $this->createMock(TTransport::class);
        $transport->expects($this->exactly(4))
            ->method('readAll')
            ->withConsecutive([1], [2], [2], [1])
            ->willReturnOnConsecutiveCalls(
                chr(TType::I16),
                '2',
                '2',
                chr(TType::STOP)
            );

        TProtocol::skipBinary($transport, TType::STRUCT);
    }

    public function testSkipBinarySkipsMap(): void
    {
        $transport = $this->createMock(TTransport::class);
        $transport->expects($this->exactly(5))
            ->method('readAll')
            ->withConsecutive([1], [1], [4], [1], [2])
            ->willReturnOnConsecutiveCalls(
                chr(TType::BOOL),
                chr(TType::I16),
                pack('N', 1),
                '1',
                '2'
            );

        TProtocol::skipBinary($transport, TType::MAP);
    }

    public function testSkipBinarySkipsList(): void
    {
        $transport = $this->createMock(TTransport::class);
        $transport->expects($this->exactly(3))
            ->method('readAll')
            ->withConsecutive([1], [4], [1])
            ->willReturnOnConsecutiveCalls(
                chr(TType::BYTE),
                pack('N', 1),
                '1'
            );

        TProtocol::skipBinary($transport, TType::LST);
    }

    public function testSkipBinarySkipsSet(): void
    {
        $transport = $this->createMock(TTransport::class);
        $transport->expects($this->exactly(3))
            ->method('readAll')
            ->withConsecutive([1], [4], [8])
            ->willReturnOnConsecutiveCalls(
                chr(TType::DOUBLE),
                pack('N', 1),
                '8'
            );

        TProtocol::skipBinary($transport, TType::SET);
    }

    public function testSkipBinaryThrowsForUnknownType(): void
    {
        $this->expectException(TProtocolException::class);
        $this->expectExceptionCode(TProtocolException::INVALID_DATA);

        TProtocol::skipBinary($this->createMock(TTransport::class), 999);
    }

    private function buildBinaryBuffer(callable $writer): string
    {
        $transport = new TMemoryBuffer();
        $protocol = new TBinaryProtocol($transport);
        $writer($protocol);

        return $transport->getBuffer();
    }
}
