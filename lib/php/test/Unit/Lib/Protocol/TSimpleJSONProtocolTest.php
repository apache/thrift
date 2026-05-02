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
use PHPUnit\Framework\Attributes\DataProvider;
use Thrift\Exception\TException;
use Thrift\Protocol\TSimpleJSONProtocol;
use Thrift\Protocol\SimpleJSON\CollectionMapKeyException;
use Thrift\Transport\TMemoryBuffer;
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
     */
    #[DataProvider('readDataProvider')]
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

    public static function readDataProvider()
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

    #[DataProvider('writeScalarProvider')]
    public function testWriteScalar(string $writeMethod, $value, string $expectedJson)
    {
        $transport = new TMemoryBuffer();
        $protocol = new TSimpleJSONProtocol($transport);

        $protocol->writeListBegin(TType::STRING, 1);
        $protocol->$writeMethod($value);
        $protocol->writeListEnd();

        $this->assertSame('[' . $expectedJson . ']', $transport->getBuffer());
    }

    public static function writeScalarProvider()
    {
        yield 'bool true' => [
            'writeMethod' => 'writeBool',
            'value' => true,
            'expectedJson' => '1',
        ];
        yield 'bool false' => [
            'writeMethod' => 'writeBool',
            'value' => false,
            'expectedJson' => '0',
        ];
        yield 'byte zero' => [
            'writeMethod' => 'writeByte',
            'value' => 0,
            'expectedJson' => '0',
        ];
        yield 'byte max' => [
            'writeMethod' => 'writeByte',
            'value' => 127,
            'expectedJson' => '127',
        ];
        yield 'byte min' => [
            'writeMethod' => 'writeByte',
            'value' => -128,
            'expectedJson' => '-128',
        ];
        yield 'i16 max' => [
            'writeMethod' => 'writeI16',
            'value' => 32767,
            'expectedJson' => '32767',
        ];
        yield 'i16 min' => [
            'writeMethod' => 'writeI16',
            'value' => -32768,
            'expectedJson' => '-32768',
        ];
        yield 'i16 zero' => [
            'writeMethod' => 'writeI16',
            'value' => 0,
            'expectedJson' => '0',
        ];
        yield 'i32 max' => [
            'writeMethod' => 'writeI32',
            'value' => 2147483647,
            'expectedJson' => '2147483647',
        ];
        yield 'i32 zero' => [
            'writeMethod' => 'writeI32',
            'value' => 0,
            'expectedJson' => '0',
        ];
        yield 'i32 negative' => [
            'writeMethod' => 'writeI32',
            'value' => -1,
            'expectedJson' => '-1',
        ];
        yield 'i64 large' => [
            'writeMethod' => 'writeI64',
            'value' => 1000000000,
            'expectedJson' => '1000000000',
        ];
        yield 'i64 zero' => [
            'writeMethod' => 'writeI64',
            'value' => 0,
            'expectedJson' => '0',
        ];
        yield 'i64 negative' => [
            'writeMethod' => 'writeI64',
            'value' => -1000000000,
            'expectedJson' => '-1000000000',
        ];
        yield 'double pi' => [
            'writeMethod' => 'writeDouble',
            'value' => 3.14,
            'expectedJson' => json_encode(3.14),
        ];
        yield 'double negative' => [
            'writeMethod' => 'writeDouble',
            'value' => -2.5,
            'expectedJson' => json_encode(-2.5),
        ];
        yield 'double large' => [
            'writeMethod' => 'writeDouble',
            'value' => 1.0e10,
            'expectedJson' => json_encode(1.0e10),
        ];
        yield 'double zero' => [
            'writeMethod' => 'writeDouble',
            'value' => 0.0,
            'expectedJson' => json_encode(0.0),
        ];
        yield 'string simple' => [
            'writeMethod' => 'writeString',
            'value' => 'hello',
            'expectedJson' => '"hello"',
        ];
        yield 'string with quotes' => [
            'writeMethod' => 'writeString',
            'value' => 'quote "inside"',
            'expectedJson' => '"quote \"inside\""',
        ];
        yield 'string with path' => [
            'writeMethod' => 'writeString',
            'value' => 'path/to/file',
            'expectedJson' => '"path/to/file"',
        ];
        yield 'string empty' => [
            'writeMethod' => 'writeString',
            'value' => '',
            'expectedJson' => '""',
        ];
        yield 'string unicode' => [
            'writeMethod' => 'writeString',
            'value' => 'привіт',
            'expectedJson' => json_encode('привіт'),
        ];
        yield 'uuid' => [
            'writeMethod' => 'writeUuid',
            'value' => '12345678-1234-5678-1234-567812345678',
            'expectedJson' => '"12345678-1234-5678-1234-567812345678"',
        ];
    }

    #[DataProvider('writeContainerProvider')]
    public function testWriteContainer(array $operations, string $expectedJson)
    {
        $transport = new TMemoryBuffer();
        $protocol = new TSimpleJSONProtocol($transport);

        foreach ($operations as [$method, $args]) {
            $protocol->$method(...$args);
        }

        $this->assertSame($expectedJson, $transport->getBuffer());
    }

    public static function writeContainerProvider()
    {
        yield 'list of integers' => [
            'operations' => [
                ['writeListBegin', [TType::I32, 3]],
                ['writeI32', [1]],
                ['writeI32', [2]],
                ['writeI32', [3]],
                ['writeListEnd', []],
            ],
            'expectedJson' => '[1,2,3]',
        ];
        yield 'empty list' => [
            'operations' => [
                ['writeListBegin', [TType::I32, 0]],
                ['writeListEnd', []],
            ],
            'expectedJson' => '[]',
        ];
        yield 'set of integers' => [
            'operations' => [
                ['writeSetBegin', [TType::I32, 2]],
                ['writeI32', [10]],
                ['writeI32', [20]],
                ['writeSetEnd', []],
            ],
            'expectedJson' => '[10,20]',
        ];
        yield 'map string to i32' => [
            'operations' => [
                ['writeMapBegin', [TType::STRING, TType::I32, 2]],
                ['writeString', ['key1']],
                ['writeI32', [100]],
                ['writeString', ['key2']],
                ['writeI32', [200]],
                ['writeMapEnd', []],
            ],
            'expectedJson' => '{"key1":100,"key2":200}',
        ];
        yield 'empty map' => [
            'operations' => [
                ['writeMapBegin', [TType::STRING, TType::I32, 0]],
                ['writeMapEnd', []],
            ],
            'expectedJson' => '{}',
        ];
        yield 'nested list' => [
            'operations' => [
                ['writeListBegin', [TType::LST, 2]],
                ['writeListBegin', [TType::I32, 2]],
                ['writeI32', [1]],
                ['writeI32', [2]],
                ['writeListEnd', []],
                ['writeListBegin', [TType::I32, 2]],
                ['writeI32', [3]],
                ['writeI32', [4]],
                ['writeListEnd', []],
                ['writeListEnd', []],
            ],
            'expectedJson' => '[[1,2],[3,4]]',
        ];
    }

    #[DataProvider('writeStructuralProvider')]
    public function testWriteStructural(array $operations, string $expectedJson)
    {
        $transport = new TMemoryBuffer();
        $protocol = new TSimpleJSONProtocol($transport);

        foreach ($operations as [$method, $args]) {
            $protocol->$method(...$args);
        }

        $this->assertSame($expectedJson, $transport->getBuffer());
    }

    public static function writeStructuralProvider()
    {
        yield 'message begin' => [
            'operations' => [
                ['writeMessageBegin', ['name', 1, 42]],
            ],
            'expectedJson' => '["name",1,42',
        ];
        yield 'message begin and end' => [
            'operations' => [
                ['writeMessageBegin', ['name', 1, 42]],
                ['writeMessageEnd', []],
            ],
            'expectedJson' => '["name",1,42]',
        ];
        yield 'struct with i32 field' => [
            'operations' => [
                ['writeStructBegin', ['MyStruct']],
                ['writeFieldBegin', ['field_name', TType::I32, 1]],
                ['writeI32', [42]],
                ['writeFieldEnd', []],
                ['writeFieldStop', []],
                ['writeStructEnd', []],
            ],
            'expectedJson' => '{"field_name":42}',
        ];
        yield 'struct with multiple fields' => [
            'operations' => [
                ['writeStructBegin', ['MyStruct']],
                ['writeFieldBegin', ['name', TType::STRING, 1]],
                ['writeString', ['test']],
                ['writeFieldEnd', []],
                ['writeFieldBegin', ['value', TType::I32, 2]],
                ['writeI32', [99]],
                ['writeFieldEnd', []],
                ['writeFieldStop', []],
                ['writeStructEnd', []],
            ],
            'expectedJson' => '{"name":"test","value":99}',
        ];
    }

    public function testMapKeyCannotBeCollection()
    {
        $this->expectException(CollectionMapKeyException::class);

        $transport = new TMemoryBuffer();
        $protocol = new TSimpleJSONProtocol($transport);

        $protocol->writeMapBegin(TType::STRING, TType::I32, 1);
        $protocol->writeMapBegin(TType::STRING, TType::I32, 1);
    }
}
