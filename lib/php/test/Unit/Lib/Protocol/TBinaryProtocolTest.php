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
use Thrift\Exception\TProtocolException;
use Thrift\Protocol\TBinaryProtocol;
use Thrift\Transport\TTransport;
use Thrift\Type\TType;

class TBinaryProtocolTest extends TestCase
{
    private const VERSION_MASK = 0xffff0000;
    private const VERSION_1 = 0x80010000;

    /**
     * @dataProvider writeMessageBeginDataProvider
     */
    public function testWriteMessageBegin(
        $strictWrite,
        $name,
        $type,
        $seqid,
        $writeCallsParams,
        $writeCallsResults,
        $expectedResult
    ) {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, $strictWrite);

        $transport->expects($this->exactly(count($writeCallsParams)))
                  ->method('write')
                  ->withConsecutive(...$writeCallsParams)
                  ->willReturnOnConsecutiveCalls(...$writeCallsResults);

        $result = $protocol->writeMessageBegin($name, $type, $seqid);
        $this->assertEquals($expectedResult, $result);
    }

    public function writeMessageBeginDataProvider()
    {
        $type = TType::STRING;
        $seqid = 555;

        yield 'strictWrite=true' => [
            'strictWrite' => true,
            'name' => 'testName',
            'type' => $type,
            'seqid' => $seqid,
            'writeCallsParams' => [
                [pack('N', self::VERSION_1 | $type), 4], #writeI32
                [pack('N', strlen('testName')), 4], #writeStringLen
                ['testName', 8], #writeString
                [pack('N', $seqid), 4], #writeI32
            ],
            'writeCallsResults' => [
                4,
                4,
                8,
                4,
            ],
            'expectedResult' => 20,
        ];

        yield 'strictWrite=false' => [
            'strictWrite' => false,
            'name' => 'testName',
            'type' => $type,
            'seqid' => $seqid,
            'writeCallsParams' => [
                [pack('N', strlen('testName')), 4], #writeStringLen
                ['testName', 8], #writeString
                [pack('c', $type), 1], #writeByte
                [pack('N', $seqid), 4], #writeI32
            ],
            'writeCallsResults' => [
                4,
                8,
                1,
                4,
            ],
            'expectedResult' => 17,
        ];
    }

    public function testWriteMessageEnd()
    {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $this->assertEquals(0, $protocol->writeMessageEnd());
    }

    public function testWriteStructBegin()
    {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $this->assertEquals(0, $protocol->writeStructBegin('testName'));
    }

    public function testWriteStructEnd()
    {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $this->assertEquals(0, $protocol->writeStructEnd());
    }

    public function testWriteFieldBegin()
    {
        $fieldName = 'testName';
        $fieldType = TType::STRING;
        $fieldId = 555;

        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $transport
            ->expects($this->exactly(2))
            ->method('write')
            ->withConsecutive(
                ...[
                       [pack('c', $fieldType), 1], #writeByte
                       [pack('n', $fieldId), 2], #writeI16
                   ]
            )->willReturnOnConsecutiveCalls([1, 2]);

        $this->assertEquals(3, $protocol->writeFieldBegin($fieldName, $fieldType, $fieldId));
    }

    public function testWriteFieldEnd()
    {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $this->assertEquals(0, $protocol->writeFieldEnd());
    }

    public function testWriteFieldStop()
    {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $transport
            ->expects($this->once())
            ->method('write')
            ->with(pack('c', TType::STOP), 1) #writeByte
            ->willReturn(1);

        $this->assertEquals(1, $protocol->writeFieldStop());
    }

    public function testWriteMapBegin()
    {
        $keyType = TType::I32;
        $valType = TType::STRING;
        $size = 99;

        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $transport
            ->expects($this->exactly(3))
            ->method('write')
            ->withConsecutive(
                ...[
                       [pack('c', $keyType), 1], #writeByte
                       [pack('c', $valType), 1], #writeByte
                       [pack('N', $size), 4], #writeI32
                   ]
            )->willReturnOnConsecutiveCalls([1, 1, 4]);

        $this->assertEquals(6, $protocol->writeMapBegin($keyType, $valType, $size));
    }

    public function testWriteMapEnd()
    {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $this->assertEquals(0, $protocol->writeMapEnd());
    }

    public function testWriteListBegin()
    {
        $elemType = TType::I32;
        $size = 99;

        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $transport
            ->expects($this->exactly(2))
            ->method('write')
            ->withConsecutive(
                ...[
                       [pack('c', $elemType), 1], #writeByte
                       [pack('N', $size), 4], #writeI32
                   ]
            )->willReturnOnConsecutiveCalls([1, 4]);

        $this->assertEquals(5, $protocol->writeListBegin($elemType, $size));
    }

    public function testWriteListEnd()
    {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $this->assertEquals(0, $protocol->writeListEnd());
    }

    public function testWriteSetBegin()
    {
        $elemType = TType::I32;
        $size = 99;

        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $transport
            ->expects($this->exactly(2))
            ->method('write')
            ->withConsecutive(
                ...[
                       [pack('c', $elemType), 1], #writeByte
                       [pack('N', $size), 4], #writeI32
                   ]
            )->willReturnOnConsecutiveCalls([1, 4]);

        $this->assertEquals(5, $protocol->writeSetBegin($elemType, $size));
    }

    public function testWriteSetEnd()
    {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $this->assertEquals(0, $protocol->writeSetEnd());
    }

    public function testWriteBool()
    {
        $value = true;
        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $transport
            ->expects($this->once())
            ->method('write')
            ->with(pack('c', (int)$value), 1) #writeByte
            ->willReturn(1);

        $this->assertEquals(1, $protocol->writeBool($value));
    }

    public function testWriteByte()
    {
        $value = 1;
        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $transport
            ->expects($this->once())
            ->method('write')
            ->with(pack('c', $value), 1) #writeByte
            ->willReturn(1);

        $this->assertEquals(1, $protocol->writeByte($value));
    }

    public function testWriteI16()
    {
        $value = 1;
        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $transport
            ->expects($this->once())
            ->method('write')
            ->with(pack('n', $value), 2) #writeI16
            ->willReturn(2);

        $this->assertEquals(2, $protocol->writeI16($value));
    }

    public function testWriteI32()
    {
        $value = 1;
        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $transport
            ->expects($this->once())
            ->method('write')
            ->with(pack('N', $value), 4) #writeI32
            ->willReturn(4);

        $this->assertEquals(4, $protocol->writeI32($value));
    }

    public function testWriteI64For32BitArchitecture()
    {
        if (PHP_INT_SIZE !== 4) {
            $this->markTestSkipped('Test is only for 32 bit architecture');
        }
        $value = 1;
        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $neg = $value < 0;

        if ($neg) {
            $value *= -1;
        }

        $hi = (int)($value / 4294967296);
        $lo = (int)$value;

        if ($neg) {
            $hi = ~$hi;
            $lo = ~$lo;
            if (($lo & (int)0xffffffff) == (int)0xffffffff) {
                $lo = 0;
                $hi++;
            } else {
                $lo++;
            }
        }

        $transport
            ->expects($this->once())
            ->method('write')
            ->with(pack('N2', $hi, $lo), 8) #writeI64
            ->willReturn(4);

        $this->assertEquals(8, $protocol->writeI64($value));
    }

    public function testWriteI64For64BitArchitecture()
    {
        if (PHP_INT_SIZE == 4) {
            $this->markTestSkipped('Test is only for 64 bit architecture');
        }
        $value = 1;
        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $hi = $value >> 32;
        $lo = $value & 0xFFFFFFFF;

        $transport
            ->expects($this->once())
            ->method('write')
            ->with(pack('N2', $hi, $lo), 8) #writeI64
            ->willReturn(8);

        $this->assertEquals(8, $protocol->writeI64($value));
    }

    public function testWriteDouble()
    {
        $value = 1;
        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $transport
            ->expects($this->once())
            ->method('write')
            ->with(strrev(pack('d', $value)), 8) #writeDouble
            ->willReturn(8);

        $this->assertEquals(8, $protocol->writeDouble($value));
    }

    public function testWriteString()
    {
        $value = 'string';
        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $transport
            ->expects($this->exactly(2))
            ->method('write')
            ->withConsecutive(
                ...[
                       [pack('N', strlen($value))], #writeI32,
                       [$value, strlen($value)], #write,
                   ]
            )->willReturnOnConsecutiveCalls([4, 6]);

        $this->assertEquals(10, $protocol->writeString($value));
    }

    /**
     * @dataProvider readMessageBeginDataProvider
     */
    public function testReadMessageBegin(
        $strictRead,
        $readCallsParams,
        $readCallsResults,
        $expectedReadLengthResult,
        $expectedName,
        $expectedType,
        $expectedSeqid,
        $expectedException = null,
        $expectedExceptionMessage = null,
        $expectedExceptionCode = null
    ) {
        if ($expectedException) {
            $this->expectException($expectedException);
            $this->expectExceptionMessage($expectedExceptionMessage);
            $this->expectExceptionCode($expectedExceptionCode);
        }

        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, $strictRead, true);

        $transport->expects($this->exactly(count($readCallsParams)))
                  ->method('readAll')
                  ->withConsecutive(...$readCallsParams)
                  ->willReturnOnConsecutiveCalls(...$readCallsResults);

        $result = $protocol->readMessageBegin($name, $type, $seqid);
        $this->assertEquals($expectedReadLengthResult, $result);
        $this->assertEquals($expectedName, $name);
        $this->assertEquals($expectedType, $type);
        $this->assertEquals($expectedSeqid, $seqid);
    }

    public function readMessageBeginDataProvider()
    {
        yield 'strictRead=true' => [
            'strictRead' => true,
            'readCallsParams' => [
                [4], #readI32
                [4], #readStringLen
                [8], #readString
                [4], #readI32
            ],
            'readCallsResults' => [
                pack('N', 0x80010000 | TType::STRING), #version
                pack('N', strlen('testName')),
                'testName',
                pack('N', 555),
            ],
            'expectedReadLengthResult' => 20,
            'expectedName' => 'testName',
            'expectedType' => TType::STRING,
            'expectedSeqid' => 555,
        ];

        yield 'incorrect version' => [
            'strictRead' => true,
            'readCallsParams' => [
                [4], #readI32
            ],
            'readCallsResults' => [
                pack('N', 0x80000000 | TType::STRING), #version
            ],
            'expectedReadLengthResult' => 4,
            'expectedName' => '',
            'expectedType' => 0,
            'expectedSeqid' => 0,
            'expectedException' => TProtocolException::class,
            'expectedExceptionMessage' => 'Bad version identifier: -2147483637',
            'expectedExceptionCode' => TProtocolException::BAD_VERSION,
        ];

        yield 'strictRead=false' => [
            'strictRead' => false,
            'readCallsParams' => [
                [4], #readStringLen
                [8], #readString
                [1], #readByte
                [4], #readI32
            ],
            'readCallsResults' => [
                pack('N', strlen('testName')),
                'testName',
                pack('c', TType::STRING),
                pack('N', 555),
            ],
            'expectedReadLengthResult' => 17,
            'expectedName' => 'testName',
            'expectedType' => TType::STRING,
            'expectedSeqid' => 555,
        ];

        yield 'strictRead=true without version' => [
            'strictRead' => true,
            'readCallsParams' => [
                [4], #readStringLen
            ],
            'readCallsResults' => [
                pack('N', strlen('testName')),
            ],
            'expectedReadLengthResult' => 17,
            'expectedName' => 'testName',
            'expectedType' => TType::STRING,
            'expectedSeqid' => 555,
            'expectedException' => TProtocolException::class,
            'expectedExceptionMessage' => 'No version identifier, old protocol client?',
            'expectedExceptionCode' => TProtocolException::BAD_VERSION,
        ];
    }

    public function testReadMessageEnd()
    {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $this->assertEquals(0, $protocol->readMessageEnd());
    }

    public function testReadStructBegin()
    {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $this->assertEquals(0, $protocol->readStructBegin($name));
        $this->assertEquals('', $name);
    }

    public function testReadStructEnd()
    {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $this->assertEquals(0, $protocol->readStructEnd());
    }

    /**
     * @dataProvider readFieldBeginDataProvider
     */
    public function testReadFieldBegin(
        $storedFieldType,
        $readCallsParams,
        $readCallsResults,
        $expectedResult,
        $expectedName,
        $expectedFieldType,
        $expectedFieldId
    ) {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $transport
            ->expects($this->exactly(count($readCallsParams)))
            ->method('readAll')
            ->withConsecutive(...$readCallsParams)
            ->willReturnOnConsecutiveCalls(...$readCallsResults);

        $this->assertEquals($expectedResult, $protocol->readFieldBegin($name, $fieldType, $fieldId));
        $this->assertEquals($expectedName, $name);
        $this->assertEquals($expectedFieldType, $fieldType);
        $this->assertEquals($expectedFieldId, $fieldId);
    }

    public function readFieldBeginDataProvider()
    {
        yield 'default' => [
            'storedFieldType' => TType::STRING,
            'readCallsParams' => [
                [1], #readByte
                [2], #readI16
            ],
            'readCallsResults' => [
                pack('c', TType::STRING),
                pack('n', 555),
            ],
            'expectedResult' => 3,
            'exprectedName' => '',
            'expectedFieldType' => TType::STRING,
            'expectedFieldId' => 555,
        ];

        yield 'stop field' => [
            'storedFieldType' => TType::STOP,
            'readCallsParams' => [
                [1], #readByte
            ],
            'readCallsResults' => [
                pack('c', TType::STOP),
            ],
            'expectedResult' => 1,
            'exprectedName' => '',
            'expectedFieldType' => 0,
            'expectedFieldId' => 0,
        ];
    }

    public function testReadFieldEnd()
    {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $this->assertEquals(0, $protocol->readFieldEnd());
    }

    public function testReadMapBegin()
    {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $transport
            ->expects($this->exactly(3))
            ->method('readAll')
            ->withConsecutive(
                ...[
                       [1], #readByte
                       [1], #readByte
                       [4], #readI32
                   ]
            )->willReturnOnConsecutiveCalls(
                pack('c', TType::I32),
                pack('c', TType::STRING),
                pack('N', 555)
            );

        $this->assertEquals(6, $protocol->readMapBegin($keyType, $valType, $size));
        $this->assertEquals(TType::I32, $keyType);
        $this->assertEquals(TType::STRING, $valType);
        $this->assertEquals(555, $size);
    }

    public function testReadMapEnd()
    {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $this->assertEquals(0, $protocol->readMapEnd());
    }

    public function testReadListBegin()
    {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $transport
            ->expects($this->exactly(2))
            ->method('readAll')
            ->withConsecutive(
                ...[
                       [1], #readByte
                       [4], #readI32
                   ]
            )->willReturnOnConsecutiveCalls(
                pack('c', TType::I32),
                pack('N', 555)
            );

        $this->assertEquals(5, $protocol->readListBegin($elemType, $size));
        $this->assertEquals(TType::I32, $elemType);
        $this->assertEquals(555, $size);
    }

    public function testReadListEnd()
    {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $this->assertEquals(0, $protocol->readListEnd());
    }

    public function testReadSetBegin()
    {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $transport
            ->expects($this->exactly(2))
            ->method('readAll')
            ->withConsecutive(
                ...[
                       [1], #readByte
                       [4], #readI32
                   ]
            )->willReturnOnConsecutiveCalls(
                pack('c', TType::I32),
                pack('N', 555)
            );

        $this->assertEquals(5, $protocol->readSetBegin($elemType, $size));
        $this->assertEquals(TType::I32, $elemType);
        $this->assertEquals(555, $size);
    }

    public function testReadSetEnd()
    {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $this->assertEquals(0, $protocol->readSetEnd());
    }

    public function testReadBool()
    {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $transport
            ->expects($this->once())
            ->method('readAll')
            ->with(1) #readByte
            ->willReturn(pack('c', 1));

        $this->assertEquals(1, $protocol->readBool($value));
        $this->assertTrue($value);
    }

    public function testReadByte()
    {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $transport
            ->expects($this->once())
            ->method('readAll')
            ->with(1) #readByte
            ->willReturn(pack('c', 1));

        $this->assertEquals(1, $protocol->readByte($value));
        $this->assertEquals(1, $value);
    }

    /**
     * @dataProvider readI16DataProvider
     */
    public function testReadI16(
        $storedValue,
        $expectedValue
    ) {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $transport
            ->expects($this->once())
            ->method('readAll')
            ->with(2) #readI16
            ->willReturn(pack('n', $storedValue));

        $this->assertEquals(2, $protocol->readI16($value));
        $this->assertEquals($expectedValue, $value);
    }

    public function readI16DataProvider()
    {
        yield 'positive' => [1, 1];
        yield 'negative' => [-1, -1];
    }

    /**
     * @dataProvider readI16DataProvider
     */
    public function testReadI32(
        $storedValue,
        $expectedValue
    ) {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $transport
            ->expects($this->once())
            ->method('readAll')
            ->with(4) #readI32
            ->willReturn(pack('N', $storedValue));

        $this->assertEquals(4, $protocol->readI32($value));
        $this->assertEquals($expectedValue, $value);
    }

    public function readI32DataProvider()
    {
        yield 'positive' => [1, 1];
        yield 'negative' => [-1, -1];
    }

    /**
     * @dataProvider readI64For32BitArchitectureDataProvider
     */
    public function testReadI64For32BitArchitecture(
        $storedValue,
        $expectedValue
    ) {
        if (PHP_INT_SIZE !== 4) {
            $this->markTestSkipped('Test is only for 32 bit architecture');
        }

        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $neg = $storedValue < 0;

        if ($neg) {
            $storedValue *= -1;
        }

        $hi = (int)($storedValue / 4294967296);
        $lo = (int)$storedValue;

        if ($neg) {
            $hi = ~$hi;
            $lo = ~$lo;
            if (($lo & (int)0xffffffff) == (int)0xffffffff) {
                $lo = 0;
                $hi++;
            } else {
                $lo++;
            }
        }

        $transport
            ->expects($this->once())
            ->method('write')
            ->with(pack('N2', $hi, $lo), 8) #writeI64
            ->willReturn(4);

        $this->assertEquals(8, $protocol->readI64($value));
        $this->assertEquals($expectedValue, $value);
    }

    public function readI64For32BitArchitectureDataProvider()
    {
        $storedValueRepresent = function ($value) {
            $neg = $value < 0;

            if ($neg) {
                $value *= -1;
            }

            $hi = (int)($value / 4294967296);
            $lo = (int)$value;

            if ($neg) {
                $hi = ~$hi;
                $lo = ~$lo;
                if (($lo & (int)0xffffffff) == (int)0xffffffff) {
                    $lo = 0;
                    $hi++;
                } else {
                    $lo++;
                }
            }

            return pack('N2', $hi, $lo);
        };

        yield 'positive' => [
            'storedValue' => $storedValueRepresent(1),
            'expectedValue' => 1,
        ];

        yield 'max' => [
            'storedValue' => $storedValueRepresent(PHP_INT_MAX),
            'expectedValue' => PHP_INT_MAX,
        ];

        yield 'min' => [
            'storedValue' => $storedValueRepresent(PHP_INT_MIN),
            'expectedValue' => PHP_INT_MIN,
        ];

        yield 'negative' => [
            'storedValue' => $storedValueRepresent(-1),
            'expectedValue' => -1,
        ];
    }

    /**
     * @dataProvider readI64For64BitArchitectureDataProvider
     */
    public function testReadI64For64BitArchitecture(
        $storedValue,
        $expectedValue
    ) {
        if (PHP_INT_SIZE == 4) {
            $this->markTestSkipped('Test is only for 64 bit architecture');
        }
        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $transport
            ->expects($this->once())
            ->method('readAll')
            ->with(8) #readI64
            ->willReturn($storedValue);

        $this->assertEquals(8, $protocol->readI64($value));
        $this->assertEquals($expectedValue, $value);
    }

    public function readI64For64BitArchitectureDataProvider()
    {
        $storedValueRepresent = function ($value) {
            $hi = $value >> 32;
            $lo = $value & 0xFFFFFFFF;

            return pack('N2', $hi, $lo);
        };

        yield 'positive' => [
            'storedValue' => $storedValueRepresent(1),
            'expectedValue' => 1,
        ];

        yield 'max' => [
            'storedValue' => $storedValueRepresent(PHP_INT_MAX),
            'expectedValue' => PHP_INT_MAX,
        ];

        yield 'min' => [
            'storedValue' => $storedValueRepresent(PHP_INT_MIN),
            'expectedValue' => PHP_INT_MIN,
        ];

        yield 'negative' => [
            'storedValue' => $storedValueRepresent(-1),
            'expectedValue' => -1,
        ];
    }

    public function testReadDouble()
    {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $transport
            ->expects($this->once())
            ->method('readAll')
            ->with(8) #readDouble
            ->willReturn(strrev(pack('d', 789)));

        $this->assertEquals(8, $protocol->readDouble($value));
        $this->assertEquals(789, $value);
    }

    /**
     * @dataProvider readStringDataProvider
     */
    public function testReadString(
        $readCallsParams,
        $readCallsResults,
        $expectedLength,
        $expectedValue
    ) {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TBinaryProtocol($transport, false, false);

        $transport
            ->expects($this->exactly(count($readCallsParams)))
            ->method('readAll')
            ->withConsecutive(...$readCallsParams)
            ->willReturnOnConsecutiveCalls(...$readCallsResults);

        $this->assertEquals($expectedLength, $protocol->readString($value));
        $this->assertEquals($expectedValue, $value);
    }

    public function readStringDataProvider()
    {
        $storedValue = '';
        yield 'empty' => [
            'readCallsParams' => [
                [4]
            ],
            'readCallsResults' => [
                pack('N', strlen($storedValue))
            ],
            'expectedLength' => 4,
            'expectedValue' => '',
        ];

        $storedValue = 'string';
        yield 'non-empty' => [
            'readCallsParams' => [
                [4],
                [strlen($storedValue)]
            ],
            'readCallsResults' => [
                pack('N', strlen($storedValue)),
                $storedValue
            ],
            'expectedLength' => 10,
            'expectedValue' => 'string',
        ];
    }
}
