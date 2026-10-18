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
use Thrift\Exception\TTransportException;
use Thrift\Protocol\TBinaryProtocol;
use Thrift\Protocol\TProtocol;
use Thrift\Transport\TMemoryBuffer;
use Thrift\Transport\TTransport;
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

    public function testSkipBinaryAtTheDepthLimit(): void
    {
        $this->assertSkipBinaryConsumesBuffer(
            $this->buildNestedStructs(TProtocol::DEFAULT_RECURSION_DEPTH),
            TType::STRUCT
        );
    }

    public function testSkipBinaryPastTheDepthLimitThrows(): void
    {
        $buffer = $this->buildNestedStructs(TProtocol::DEFAULT_RECURSION_DEPTH + 1);

        $this->expectException(TProtocolException::class);
        $this->expectExceptionCode(TProtocolException::DEPTH_LIMIT);

        TProtocol::skipBinary(new TMemoryBuffer($buffer), TType::STRUCT);
    }

    public function testSkipBinaryStringUnderTheLimitReadsTheBytes(): void
    {
        // A STRING of 100 bytes: the 4-byte length prefix plus the body.
        $body = str_repeat('A', 100);
        $transport = $this->recordingTransport(pack('N', 100) . $body);

        $this->assertSame(4 + 100, TProtocol::skipBinary($transport, TType::STRING));
        // The body is requested from the transport, as it must be.
        $this->assertSame(104, $transport->totalRead);
    }

    public function testSkipBinaryStringOverTheLimitIsRefusedBeforeTheBytesAreRead(): void
    {
        // A STRING declaring the largest positive i32 length (~2 GiB) with
        // none of the bytes present.
        $declared = 0x7fffffff;
        $transport = $this->recordingTransport(pack('N', $declared));

        try {
            TProtocol::skipBinary($transport, TType::STRING);
            $this->fail('expected a TProtocolException for the oversize string length');
        } catch (TProtocolException $e) {
            $this->assertSame(TProtocolException::SIZE_LIMIT, $e->getCode());
        }

        // The declared length is held before any of the body is requested:
        // only the 4-byte length prefix is ever read from the transport.
        $this->assertSame(4, $transport->maxRequested);
        $this->assertSame(4, $transport->totalRead);
    }

    /**
     * A transport over a fixed buffer that records the lengths it is asked to
     * read, so a length that is refused can be told from one that is read.
     */
    private function recordingTransport(string $data): TTransport
    {
        return new class ($data) extends TTransport {
            public int $maxRequested = 0;
            public int $totalRead = 0;
            private int $pos = 0;

            public function __construct(private string $data)
            {
            }

            public function isOpen(): bool
            {
                return true;
            }

            public function open(): void
            {
            }

            public function close(): void
            {
            }

            public function read(int $len): string
            {
                $this->maxRequested = max($this->maxRequested, $len);
                $chunk = substr($this->data, $this->pos, $len);
                $this->pos += strlen($chunk);
                $this->totalRead += strlen($chunk);
                if ($chunk === '') {
                    throw new TTransportException('recording transport exhausted');
                }

                return $chunk;
            }

            public function write(string $buf): void
            {
            }
        };
    }

    /**
     * $levels nested structs, each but the last holding the next in a field.
     */
    private function buildNestedStructs(int $levels): string
    {
        return $this->buildBinaryBuffer(
            function (TBinaryProtocol $protocol) use ($levels): void {
                for ($i = 1; $i < $levels; $i++) {
                    $protocol->writeStructBegin('Nested');
                    $protocol->writeFieldBegin('inner', TType::STRUCT, 1);
                }
                $protocol->writeStructBegin('Nested');
                $protocol->writeFieldStop();
                $protocol->writeStructEnd();
                for ($i = 1; $i < $levels; $i++) {
                    $protocol->writeFieldEnd();
                    $protocol->writeFieldStop();
                    $protocol->writeStructEnd();
                }
            }
        );
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
