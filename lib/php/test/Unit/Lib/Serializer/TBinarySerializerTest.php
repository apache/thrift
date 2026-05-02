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

namespace Test\Thrift\Unit\Lib\Serializer;

use phpmock\phpunit\PHPMock;
use PHPUnit\Framework\TestCase;
use Test\Thrift\Unit\Lib\Fixture\TestSerializerStruct;
use Thrift\Serializer\TBinarySerializer;

class TBinarySerializerTest extends TestCase
{
    use PHPMock;

    /**
     * @dataProvider serializeDeserializeDataProvider
     */
    public function testSerializeAndDeserialize($stringVal, $intVal)
    {
        $object = new TestSerializerStruct();
        $object->stringField = $stringVal;
        $object->intField = $intVal;

        $serialized = TBinarySerializer::serialize($object);

        $this->assertNotEmpty($serialized);
        $this->assertIsString($serialized);

        $deserialized = TBinarySerializer::deserialize($serialized, TestSerializerStruct::class);

        $this->assertInstanceOf(TestSerializerStruct::class, $deserialized);
        $this->assertEquals($stringVal, $deserialized->stringField);
        $this->assertEquals($intVal, $deserialized->intField);
    }

    public function serializeDeserializeDataProvider()
    {
        yield 'both fields' => [
            'stringVal' => 'hello',
            'intVal' => 42,
        ];
        yield 'empty struct' => [
            'stringVal' => null,
            'intVal' => null,
        ];
        yield 'only string field' => [
            'stringVal' => 'test value',
            'intVal' => null,
        ];
        yield 'only int field' => [
            'stringVal' => null,
            'intVal' => 12345,
        ];
        yield 'empty string' => [
            'stringVal' => '',
            'intVal' => 0,
        ];
        yield 'special characters' => [
            'stringVal' => "line1\nline2\ttab \"quotes\"",
            'intVal' => -1,
        ];
        yield 'large int' => [
            'stringVal' => 'max',
            'intVal' => 2147483647,
        ];
        yield 'negative int' => [
            'stringVal' => 'min',
            'intVal' => -2147483648,
        ];
    }

    public function testDeserializeWithCustomBufferSize()
    {
        $object = new TestSerializerStruct();
        $object->stringField = 'buffer test';
        $object->intField = 99;

        $serialized = TBinarySerializer::serialize($object);
        $deserialized = TBinarySerializer::deserialize($serialized, TestSerializerStruct::class, 1024);

        $this->assertInstanceOf(TestSerializerStruct::class, $deserialized);
        $this->assertEquals('buffer test', $deserialized->stringField);
        $this->assertEquals(99, $deserialized->intField);
    }

    public function testSerializeWithAcceleratedExtension()
    {
        $object = new TestSerializerStruct();
        $object->stringField = 'accel';
        $object->intField = 7;

        $funcExists = $this->getFunctionMock('Thrift\Serializer', 'function_exists');
        $funcExists->expects($this->atLeastOnce())
             ->willReturn(true);

        $writeFunc = $this->getFunctionMock('Thrift\Serializer', 'thrift_protocol_write_binary');
        $writeFunc->expects($this->once())
             ->willReturnCallback(function ($protocol, $name, $type, $object, $seqid, $strictWrite) {
                 // Simulate C extension: write message header + struct
                 $protocol->writeMessageBegin($name, $type, $seqid);
                 $object->write($protocol);
                 $protocol->writeMessageEnd();
             });

        $serialized = TBinarySerializer::serialize($object);

        $this->assertNotEmpty($serialized);
        $this->assertIsString($serialized);
    }

    public function testDeserializeWithAcceleratedExtension()
    {
        $object = new TestSerializerStruct();
        $object->stringField = 'accel';
        $object->intField = 7;

        $serialized = TBinarySerializer::serialize($object);

        $funcExists = $this->getFunctionMock('Thrift\Serializer', 'function_exists');
        $funcExists->expects($this->atLeastOnce())
             ->willReturn(true);

        $expectedResult = new TestSerializerStruct();
        $expectedResult->stringField = 'accel';
        $expectedResult->intField = 7;

        $readFunc = $this->getFunctionMock('Thrift\Serializer', 'thrift_protocol_read_binary');
        $readFunc->expects($this->once())
             ->willReturn($expectedResult);

        $deserialized = TBinarySerializer::deserialize($serialized, TestSerializerStruct::class);

        $this->assertInstanceOf(TestSerializerStruct::class, $deserialized);
        $this->assertEquals('accel', $deserialized->stringField);
        $this->assertEquals(7, $deserialized->intField);
    }
}
