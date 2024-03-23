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
use Thrift\Protocol\TCompactProtocol;
use Thrift\Transport\TTransport;
use Thrift\Type\TType;

class TCompactProtocolTest extends TestCase
{
    private const COMPACT_STOP = 0x00;
    private const COMPACT_TRUE = 0x01;
    private const COMPACT_FALSE = 0x02;
    private const COMPACT_BYTE = 0x03;
    private const COMPACT_I16 = 0x04;
    private const COMPACT_I32 = 0x05;
    private const COMPACT_I64 = 0x06;
    private const COMPACT_DOUBLE = 0x07;
    private const COMPACT_BINARY = 0x08;
    private const COMPACT_LIST = 0x09;
    private const COMPACT_SET = 0x0A;
    private const COMPACT_MAP = 0x0B;
    private const COMPACT_STRUCT = 0x0C;

    private const STATE_CLEAR = 0;
    private const STATE_FIELD_WRITE = 1;
    private const STATE_VALUE_WRITE = 2;
    private const STATE_CONTAINER_WRITE = 3;
    private const STATE_BOOL_WRITE = 4;
    private const STATE_FIELD_READ = 5;
    private const STATE_CONTAINER_READ = 6;
    private const STATE_VALUE_READ = 7;
    private const STATE_BOOL_READ = 8;

    private const VERSION_MASK = 0x1f;
    private const VERSION = 1;
    private const PROTOCOL_ID = 0x82;
    private const TYPE_MASK = 0xe0;
    private const TYPE_BITS = 0x07;
    private const TYPE_SHIFT_AMOUNT = 5;

    /**
     * @dataProvider toZigZagDataProvider
     */
    public function testToZigZag(
        $n,
        $bits,
        $expected
    ) {
        $protocol = new TCompactProtocol($this->createMock(TTransport::class));
        $this->assertSame($expected, $protocol->toZigZag($n, $bits));
    }

    public function toZigZagDataProvider()
    {
        yield ['n' => 0, 'bits' => 16, 'expected' => 0];
        yield ['n' => -1, 'bits' => 16, 'expected' => 1];
        yield ['n' => 1, 'bits' => 16, 'expected' => 2];
        yield ['n' => -2, 'bits' => 16, 'expected' => 3];
        yield ['n' => 2, 'bits' => 16, 'expected' => 4];
        yield ['n' => -1, 'bits' => 32, 'expected' => 1];
        yield ['n' => 1, 'bits' => 32, 'expected' => 2];
        yield ['n' => -1, 'bits' => 64, 'expected' => 1];
        yield ['n' => 1, 'bits' => 64, 'expected' => 2];
        yield ['n' => -0x7fffffff, 'bits' => 64, 'expected' => 4294967293];
        yield ['n' => 0x7fffffff, 'bits' => 64, 'expected' => 4294967294];
    }

    /**
     * @dataProvider fromZigZagDataProvider
     */
    public function testFromZigZag(
        $n,
        $expected
    ) {
        $protocol = new TCompactProtocol($this->createMock(TTransport::class));
        $this->assertSame($expected, $protocol->fromZigZag($n));
    }

    public function fromZigZagDataProvider()
    {
        yield ['n' => 0, 'expected' => 0];
        yield ['n' => 1, 'expected' => -1];
        yield ['n' => 2, 'expected' => 1];
        yield ['n' => 3, 'expected' => -2];
        yield ['n' => 4, 'expected' => 2];
        yield ['n' => 4294967293, 'expected' => -0x7fffffff];
        yield ['n' => 4294967294, 'expected' => 0x7fffffff];
    }

    /**
     * @dataProvider getVarintDataProvider
     */
    public function testGetVarint(
        $data,
        $expected
    ) {
        $protocol = new TCompactProtocol($this->createMock(TTransport::class));
        $this->assertSame($expected, $protocol->getVarint($data));
    }

    public function getVarintDataProvider()
    {
        yield ['data' => 0, 'expected' => "\x00"];
        yield ['data' => 1, 'expected' => "\x01"];
        yield ['data' => 97, 'expected' => "a"];
        yield ['data' => 100, 'expected' => "d"];
        yield ['data' => 1000, 'expected' => "\xe8\x07"];
    }

    public function testWriteVarint()
    {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TCompactProtocol($transport);

        $transport->expects($this->once())
                  ->method('write')
                  ->with("\xe8\x07", 2);

        $protocol->writeVarint(1000);
    }

    public function testReadVarint()
    {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TCompactProtocol($transport);

        $transport->expects($this->exactly(2))
                  ->method('readAll')
                  ->with(1)
                  ->willReturnOnConsecutiveCalls(
                      ...[
                             "\xe8",
                             "\x07",
                         ]
                  );

        $this->assertSame(2, $protocol->readVarint($result));
        $this->assertSame(1000, $result);
    }

    public function testWriteMessageBegin()
    {
        $name = 'testName';
        $type = TType::STRING;
        $seqid = 1;

        $transport = $this->createMock(TTransport::class);
        $protocol = new TCompactProtocol($transport);

        $transport
            ->expects($this->exactly(5))
            ->method('write')
            ->withConsecutive(
                ...[
                       [pack('C', self::PROTOCOL_ID), 1], #protocal id
                       [pack('C', self::VERSION | ($type << TCompactProtocol::TYPE_SHIFT_AMOUNT)), 1], #version
                       ["\x01", 1], #seqid
                       ["\x08", 1], #field name length
                       ["testName", 8], #field name
                   ]
            )->willReturnOnConsecutiveCalls(
                1,
                1,
                1,
                1,
                8
            );

        $result = $protocol->writeMessageBegin($name, $type, $seqid);
        $this->assertSame(12, $result);

        $ref = new \ReflectionClass($protocol);
        $state = $ref->getProperty('state');
        $state->setAccessible(true);
        $this->assertSame(self::STATE_VALUE_WRITE, $state->getValue($protocol));
    }

    public function testWriteMessageEnd()
    {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TCompactProtocol($transport);

        $this->assertSame(0, $protocol->writeMessageEnd());
        $ref = new \ReflectionClass($protocol);
        $state = $ref->getProperty('state');
        $state->setAccessible(true);
        $this->assertSame(self::STATE_CLEAR, $state->getValue($protocol));
    }

    public function testWriteStruct()
    {
        $name = 'testName';

        $transport = $this->createMock(TTransport::class);
        $protocol = new TCompactProtocol($transport);
        $ref = new \ReflectionClass($protocol);
        $state = $ref->getProperty('state');
        $state->setAccessible(true);
        $lastFid = $ref->getProperty('lastFid');
        $lastFid->setAccessible(true);
        $structs = $ref->getProperty('structs');
        $structs->setAccessible(true);

        $this->assertSame(0, $protocol->writeStructBegin($name));
        $this->assertSame([[self::STATE_CLEAR, 0]], $structs->getValue($protocol));
        $this->assertSame(self::STATE_FIELD_WRITE, $state->getValue($protocol));
        $this->assertSame(0, $lastFid->getValue($protocol));

        $this->assertSame(0, $protocol->writeStructBegin($name));
        $this->assertSame(self::STATE_FIELD_WRITE, $state->getValue($protocol));
        $this->assertSame(0, $lastFid->getValue($protocol));
        $this->assertSame([[self::STATE_CLEAR, 0], [self::STATE_FIELD_WRITE, 0]], $structs->getValue($protocol));

        $this->assertSame(0, $protocol->writeStructEnd());
        $this->assertSame(self::STATE_FIELD_WRITE, $state->getValue($protocol));
        $this->assertSame(0, $lastFid->getValue($protocol));
        $this->assertSame([[self::STATE_CLEAR, 0]], $structs->getValue($protocol));

        $this->assertSame(0, $protocol->writeStructEnd());
        $this->assertSame(self::STATE_CLEAR, $state->getValue($protocol));
        $this->assertSame(0, $lastFid->getValue($protocol));
        $this->assertSame([], $structs->getValue($protocol));
    }

    public function testWriteFieldStop()
    {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TCompactProtocol($transport);

        $transport->expects($this->once())
                  ->method('write')
                  ->with("\x00", 1);

        $this->assertSame(1, $protocol->writeFieldStop());
    }

    /**
     * @dataProvider writeFieldHeaderDataProvider
     */
    public function testWriteFieldHeader(
        $type,
        $fid,
        $writeCallParams,
        $writeCallResult,
        $expectedResult
    ) {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TCompactProtocol($transport);

        $transport
            ->expects($this->exactly(count($writeCallParams)))
            ->method('write')
            ->withConsecutive(...$writeCallParams)
            ->willReturnOnConsecutiveCalls(...$writeCallResult);

        $this->assertSame($expectedResult, $protocol->writeFieldHeader($type, $fid));
    }

    public function writeFieldHeaderDataProvider()
    {
        yield 'bool' => [
            'type' => TType::BOOL,
            'fid' => 1,
            'writeCallParams' => [
                ["\x12", 1], #writeUByte(pack('C', ($delta << 4) | $type)),
            ],
            'writeCallResult' => [
                1,
            ],
            'expectedResult' => 1,
        ];
        yield 'list' => [
            'type' => TType::LST,
            'fid' => 16,
            'writeCallParams' => [
                ["\x0f", 1], #writeUByte(pack('C', ($delta << 4) | $type)),
                [" ", 1], #writeI16($fid),
            ],
            'writeCallResult' => [
                1,
            ],
            'expectedResult' => 2,
        ];
    }

    /**
     * @dataProvider writeFieldBeginDataProvider
     */
    public function testWriteFieldBegin(
        $fieldName,
        $fieldType,
        $fieldId,
        $writeCallParams,
        $writeCallResult,
        $expectedState,
        $expectedBoolFid,
        $expectedLastFid,
        $expectedResult
    ) {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TCompactProtocol($transport);

        $transport
            ->expects($this->exactly(count($writeCallParams)))
            ->method('write')
            ->withConsecutive(...$writeCallParams)
            ->willReturnOnConsecutiveCalls(...$writeCallResult);

        $this->assertSame($expectedResult, $protocol->writeFieldBegin($fieldName, $fieldType, $fieldId));

        $ref = new \ReflectionClass($protocol);
        $state = $ref->getProperty('state');
        $state->setAccessible(true);
        $boolFid = $ref->getProperty('boolFid');
        $boolFid->setAccessible(true);
        $lastFid = $ref->getProperty('lastFid');
        $lastFid->setAccessible(true);
        $this->assertSame($expectedState, $state->getValue($protocol));
        $this->assertSame($expectedBoolFid, $boolFid->getValue($protocol));
        $this->assertSame($expectedLastFid, $lastFid->getValue($protocol));
    }

    public function writeFieldBeginDataProvider()
    {
        yield 'bool' => [
            'fieldName' => 'testName',
            'fieldType' => TType::BOOL,
            'fieldId' => 1,
            'writeCallParams' => [],
            'writeCallResult' => [],
            'expectedState' => self::STATE_BOOL_WRITE,
            'expectedBoolFid' => 1,
            'expectedLastFid' => 0,
            'expectedResult' => 0,
        ];
        yield 'list' => [
            'fieldName' => 'testName',
            'fieldType' => TType::LST,
            'fieldId' => 1,
            'writeCallParams' => [
                ["\x19", 1], #writeUByte(pack('C', ($delta << 4) | $type)),
            ],
            'writeCallResult' => [
                1,
            ],
            'expectedState' => self::STATE_VALUE_WRITE,
            'expectedBoolFid' => null,
            'expectedLastFid' => 1,
            'expectedResult' => 1,
        ];
    }

    public function testWriteFieldEnd()
    {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TCompactProtocol($transport);

        $this->assertSame(0, $protocol->writeFieldEnd());

        $ref = new \ReflectionClass($protocol);
        $state = $ref->getProperty('state');
        $state->setAccessible(true);
        $this->assertSame(self::STATE_FIELD_WRITE, $state->getValue($protocol));
    }

    /**
     * @dataProvider writeCollectionDataProvider
     */
    public function testWriteCollection(
        $etype,
        $size,
        $writeCallParams,
        $writeCallResult,
        $expectedState,
        $expectedContainers,
        $expectedResult
    ) {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TCompactProtocol($transport);

        $transport
            ->expects($this->exactly(count($writeCallParams)))
            ->method('write')
            ->withConsecutive(...$writeCallParams)
            ->willReturnOnConsecutiveCalls(...$writeCallResult);

        $this->assertSame($expectedResult, $protocol->writeCollectionBegin($etype, $size));

        $ref = new \ReflectionClass($protocol);
        $state = $ref->getProperty('state');
        $state->setAccessible(true);
        $containers = $ref->getProperty('containers');
        $containers->setAccessible(true);
        $this->assertSame($expectedState, $state->getValue($protocol));
        $this->assertSame($expectedContainers, $containers->getValue($protocol));

        $this->assertSame(0, $protocol->writeCollectionEnd());
        $this->assertSame(TCompactProtocol::STATE_CLEAR, $state->getValue($protocol));
    }

    public function writeCollectionDataProvider()
    {
        yield 'size < 14' => [
            'etype' => TType::STRING,
            'size' => 1,
            'writeCallParams' => [
                ["\x18", 1], #writeUByte(pack('C', ($size << 4 | self::$ctypes[$etype])),
            ],
            'writeCallResult' => [
                1,
            ],
            'expectedState' => self::STATE_CONTAINER_WRITE,
            'expectedContainers' => [
                0 => 0,
            ],
            'expectedResult' => 1,
        ];
        yield 'size > 14' => [
            'etype' => TType::STRING,
            'size' => 16,
            'writeCallParams' => [
                ["\xf8", 1], #writeUByte(pack('C', 0xf0 | self::$ctypes[$etype])),
                ["\x10", 1], #writeVarint(16),
            ],
            'writeCallResult' => [
                1,
                1,
            ],
            'expectedState' => self::STATE_CONTAINER_WRITE,
            'expectedContainers' => [
                0 => 0,
            ],
            'expectedResult' => 2,
        ];
    }

    /**
     * @dataProvider writeMapDataProvider
     */
    public function testWriteMap(
        $keyType,
        $valType,
        $size,
        $writeCallParams,
        $writeCallResult,
        $expectedContainers,
        $expectedResult
    ) {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TCompactProtocol($transport);

        $transport
            ->expects($this->exactly(count($writeCallParams)))
            ->method('write')
            ->withConsecutive(...$writeCallParams)
            ->willReturnOnConsecutiveCalls(...$writeCallResult);

        $this->assertSame($expectedResult, $protocol->writeMapBegin($keyType, $valType, $size));

        $ref = new \ReflectionClass($protocol);
        $containers = $ref->getProperty('containers');
        $containers->setAccessible(true);
        $state = $ref->getProperty('state');
        $state->setAccessible(true);
        $this->assertSame($expectedContainers, $containers->getValue($protocol));
        $this->assertSame(TCompactProtocol::STATE_CLEAR, $state->getValue($protocol));

        $this->assertSame(0, $protocol->writeMapEnd());
        $this->assertSame(TCompactProtocol::STATE_CLEAR, $state->getValue($protocol));
        $this->assertSame([], $containers->getValue($protocol));
    }

    public function writeMapDataProvider()
    {
        yield 'size zero' => [
            'keyType' => TType::STRING,
            'valType' => TType::STRING,
            'size' => 0,
            'writeCallParams' => [
                ["\x00", 1], #writeByte(0),
            ],
            'writeCallResult' => [
                1,
            ],
            'expectedContainers' => [
                0 => 0,
            ],
            'expectedResult' => 1,
        ];
        yield 'size non zero' => [
            'keyType' => TType::STRING,
            'valType' => TType::STRING,
            'size' => 16,
            'writeCallParams' => [
                ["\x10", 1], #writeVarint(16),
                ["\x88", 1], #writeUByte(pack('C', self::$ctypes[$key_type] << 4 | self::$ctypes[$val_type])),
            ],
            'writeCallResult' => [
                1,
                1,
            ],
            'expectedContainers' => [
                0 => 0,
            ],
            'expectedResult' => 2,
        ];
    }

    public function testWriteListBegin()
    {
        $protocol = $this->createPartialMock(TCompactProtocol::class, ['writeCollectionBegin']);

        $protocol->expects($this->once())
                 ->method('writeCollectionBegin')
                 ->with(TType::STRING, 1)
                 ->willReturn(1);

        $this->assertSame(1, $protocol->writeListBegin(TType::STRING, 1));
    }

    public function testWriteListEnd()
    {
        $protocol = $this->createPartialMock(TCompactProtocol::class, ['writeCollectionEnd']);

        $protocol->expects($this->once())
                 ->method('writeCollectionEnd')
                 ->willReturn(1);

        $this->assertSame(1, $protocol->writeListEnd());
    }

    public function testWriteSettBegin()
    {
        $protocol = $this->createPartialMock(TCompactProtocol::class, ['writeCollectionBegin']);

        $protocol->expects($this->once())
                 ->method('writeCollectionBegin')
                 ->with(TType::STRING, 1)
                 ->willReturn(1);

        $this->assertSame(1, $protocol->writeSetBegin(TType::STRING, 1));
    }

    public function testWriteSetEnd()
    {
        $protocol = $this->createPartialMock(TCompactProtocol::class, ['writeCollectionEnd']);

        $protocol->expects($this->once())
                 ->method('writeCollectionEnd')
                 ->willReturn(1);

        $this->assertSame(1, $protocol->writeSetEnd());
    }

    /**
     * @dataProvider writeBinaryDataProvider
     */
    public function testWriteBool(
        $value,
        $startState,
        $writeCallParams,
        $writeCallResult,
        $expectedResult,
        $expectedException,
        $expectedExceptionMessage
    ) {
        if ($expectedException) {
            $this->expectException($expectedException);
            $this->expectExceptionMessage($expectedExceptionMessage);
        }

        $transport = $this->createMock(TTransport::class);
        $protocol = new TCompactProtocol($transport);
        if (!is_null($startState)) {
            $ref = new \ReflectionClass($protocol);
            $state = $ref->getProperty('state');
            $state->setAccessible(true);
            $state->setValue($protocol, $startState);
        }

        $transport
            ->expects($this->exactly(count($writeCallParams)))
            ->method('write')
            ->withConsecutive(...$writeCallParams)
            ->willReturnOnConsecutiveCalls(...$writeCallResult);

        $this->assertSame($expectedResult, $protocol->writeBool($value));
    }

    public function writeBinaryDataProvider()
    {
        yield 'invalid state' => [
            'value' => true,
            'startState' => null,
            'writeCallParams' => [],
            'writeCallResult' => [],
            'expectedResult' => 0,
            'expectedException' => TProtocolException::class,
            'expectedExceptionMessage' => 'Invalid state in compact protocol',
        ];

        yield 'true' => [
            'value' => true,
            'startState' => TCompactProtocol::STATE_BOOL_WRITE,
            'writeCallParams' => [
                ["\x01", 1], #writeByte
                ["\x00", 1], #writeI16
            ],
            'writeCallResult' => [
                1,
                1,
            ],
            'expectedResult' => 2,
            'expectedException' => null,
            'expectedExceptionMessage' => null,
        ];

        yield 'false' => [
            'value' => false,
            'startState' => TCompactProtocol::STATE_BOOL_WRITE,
            'writeCallParams' => [
                ["\x02", 1], #writeByte
                ["\x00", 1], #writeI16
            ],
            'writeCallResult' => [
                1,
                1,
            ],
            'expectedResult' => 2,
            'expectedException' => null,
            'expectedExceptionMessage' => null,
        ];

        yield 'container true' => [
            'value' => true,
            'startState' => TCompactProtocol::STATE_CONTAINER_WRITE,
            'writeCallParams' => [
                ["\x01", 1], #writeByte
            ],
            'writeCallResult' => [
                1,
            ],
            'expectedResult' => 1,
            'expectedException' => null,
            'expectedExceptionMessage' => null,
        ];
    }

    /**
     * @dataProvider writeByteDataProvider
     */
    public function testWriteByte(
        $value,
        $expectedWriteCallParam
    ) {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TCompactProtocol($transport);

        $transport->expects($this->once())
                  ->method('write')
                  ->with($expectedWriteCallParam, 1);

        $this->assertSame(1, $protocol->writeByte($value));
    }

    public function writeByteDataProvider()
    {
        yield 'signed' => [
            'value' => -1,
            'expectedWriteCallParam' => "\xff",
        ];
        yield 'unsigned' => [
            'value' => 1,
            'expectedWriteCallParam' => "\x01",
        ];
        yield 'lowercase' => [
            'value' => 'a',
            'expectedWriteCallParam' => "\x00",
        ];
        yield 'upercase' => [
            'value' => 'A',
            'expectedWriteCallParam' => "\x00",
        ];
    }

    /**
     * @dataProvider writeUByteDataProvider
     */
    public function testWriteUByte(
        $value,
        $expectedWriteCallParam
    ) {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TCompactProtocol($transport);

        $transport->expects($this->once())
                  ->method('write')
                  ->with($expectedWriteCallParam, 1);

        $this->assertSame(1, $protocol->writeUByte($value));
    }

    public function writeUByteDataProvider()
    {
        yield 'signed' => [
            'value' => -1,
            'expectedWriteCallParam' => "\xff",
        ];
        yield 'unsigned' => [
            'value' => 1,
            'expectedWriteCallParam' => "\x01",
        ];
        yield 'lowercase' => [
            'value' => 'a',
            'expectedWriteCallParam' => "\x00",
        ];
        yield 'upercase' => [
            'value' => 'A',
            'expectedWriteCallParam' => "\x00",
        ];
    }

    public function testWriteI16()
    {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TCompactProtocol($transport);

        $transport->expects($this->once())
                  ->method('write')
                  ->with("\x00", 1);

        $this->assertSame(1, $protocol->writeI16(0));
    }

    public function testWriteI32()
    {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TCompactProtocol($transport);

        $transport->expects($this->once())
                  ->method('write')
                  ->with("\x00", 1);

        $this->assertSame(1, $protocol->writeI32(0));
    }

    public function testWriteDouble()
    {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TCompactProtocol($transport);

        $transport->expects($this->once())
                  ->method('write')
                  ->with(pack('d', 0), 8);

        $this->assertSame(8, $protocol->writeDouble(0));
    }

    public function testWriteString()
    {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TCompactProtocol($transport);

        $transport->expects($this->exactly(2))
                  ->method('write')
                  ->withConsecutive(
                      ["\x04", 1],
                      ["test", 4]
                  );

        $this->assertSame(5, $protocol->writeString('test'));
    }

    /**
     * @dataProvider writeI64DataProvider
     */
    public function testWriteI64(
        $value,
        $expectedWriteCallParam,
        $expectedResult
    ) {
        $transport = $this->createMock(TTransport::class);
        $protocol = new TCompactProtocol($transport);

        $transport->expects($this->once())
                  ->method('write')
                  ->with(...$expectedWriteCallParam);

        $this->assertSame($expectedResult, $protocol->writeI64($value));
    }

    public function writeI64DataProvider()
    {
        yield 'simple' => [
            'value' => 0,
            'expectedWriteCallParam' => ["\x00", 1],
            'expectedResult' => 1,
        ];
        yield 'negative' => [
            'value' => -1,
            'expectedWriteCallParam' => ["\x01", 1],
            'expectedResult' => 1,
        ];
        yield 'big' => [
            'value' => 5000000000,
            'expectedWriteCallParam' => [hex2bin("80c8afa025"), 5],
            'expectedResult' => 5,
        ];
        yield 'small' => [
            'value' => -5000000000,
            'expectedWriteCallParam' => [hex2bin("ffc7afa025"), 5],
            'expectedResult' => 5,
        ];
        yield 'max simple' => [
            'value' => 0xffffffff,
            'expectedWriteCallParam' => [hex2bin("feffffff1f"), 5],
            'expectedResult' => 5,
        ];
    }
}
