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

namespace Test\Thrift\Unit\Lib\Exception;

use PHPUnit\Framework\TestCase;
use Test\Thrift\Unit\Lib\Fixture\TestRichException;
use Thrift\Exception\TException;
use Thrift\Protocol\TBinaryProtocol;
use Thrift\Transport\TMemoryBuffer;
use Thrift\Type\TType;

class TExceptionTest extends TestCase
{
    public function testExceptionWithMessageAndCode()
    {
        $message = 'Test exception message';
        $code = 42;

        $exception = new TException($message, $code);

        $this->assertInstanceOf(TException::class, $exception);
        $this->assertSame($message, $exception->getMessage());
        $this->assertSame($code, $exception->getCode());
    }

    public function testExceptionWithSpecAndVals()
    {
        $spec = [
            ['var' => 'string'],
            ['var' => 'int'],
            ['var' => 'bool'],
        ];

        $vals = [
            'string' => 'Test value',
            'int' => 123456,
            'bool' => true,
        ];
        $exception = new TException($spec, $vals);

        $this->assertEquals('Test value', $exception->string);
        $this->assertEquals(123456, $exception->int);
        $this->assertEquals(true, $exception->bool);
    }

    public function testExceptionWithDefaultParams()
    {
        $exception = new TException();
        $this->assertSame('', $exception->getMessage());
        $this->assertSame(0, $exception->getCode());
    }

    public function testExceptionSpecIgnoresUnsetVals()
    {
        $spec = [
            ['var' => 'field1'],
            ['var' => 'field2'],
        ];
        $vals = ['field1' => 'set'];

        $exception = new TException($spec, $vals);
        $this->assertEquals('set', $exception->field1);
    }

    /**
     * @dataProvider writeAndReadFieldDataProvider
     */
    public function testWriteAndReadField($field, $value)
    {
        $exception = new TestRichException();
        $exception->$field = $value;

        $result = $this->roundtrip($exception);

        $this->assertEquals($value, $result->$field);
    }

    public function writeAndReadFieldDataProvider()
    {
        // scalars
        yield 'string' => ['field' => 'stringField', 'value' => 'hello world'];
        yield 'int' => ['field' => 'intField', 'value' => 42];
        yield 'bool true' => ['field' => 'boolField', 'value' => true];
        yield 'bool false' => ['field' => 'boolField', 'value' => false];
        yield 'double' => ['field' => 'doubleField', 'value' => 3.14];

        // containers
        yield 'map' => ['field' => 'mapField', 'value' => ['key1' => 100, 'key2' => 200]];
        yield 'list' => ['field' => 'listField', 'value' => ['alpha', 'beta', 'gamma']];
        yield 'set' => ['field' => 'setField', 'value' => [10 => true, 20 => true, 30 => true]];

        // empty containers
        yield 'empty map' => ['field' => 'mapField', 'value' => []];
        yield 'empty list' => ['field' => 'listField', 'value' => []];
        yield 'empty set' => ['field' => 'setField', 'value' => []];
    }

    public function testWriteAndReadAllFields()
    {
        $exception = new TestRichException();
        $exception->stringField = 'test';
        $exception->intField = 99;
        $exception->boolField = true;
        $exception->doubleField = 2.718;
        $exception->mapField = ['a' => 1];
        $exception->listField = ['x', 'y'];
        $exception->setField = [5 => true];

        $result = $this->roundtrip($exception);

        $this->assertEquals('test', $result->stringField);
        $this->assertEquals(99, $result->intField);
        $this->assertTrue($result->boolField);
        $this->assertEquals(2.718, $result->doubleField);
        $this->assertEquals(['a' => 1], $result->mapField);
        $this->assertEquals(['x', 'y'], $result->listField);
        $this->assertEquals([5 => true], $result->setField);
    }

    public function testWriteSkipsNullFields()
    {
        $exception = new TestRichException();
        $exception->stringField = 'only this';

        $transport = new TMemoryBuffer();
        $protocol = new TBinaryProtocol($transport);
        $exception->write($protocol);

        $result = new TestRichException();
        $readTransport = new TMemoryBuffer($transport->getBuffer());
        $readProtocol = new TBinaryProtocol($readTransport);
        $result->read($readProtocol);

        $this->assertEquals('only this', $result->stringField);
        $this->assertNull($result->intField);
        $this->assertNull($result->boolField);
        $this->assertNull($result->mapField);
        $this->assertNull($result->listField);
        $this->assertNull($result->setField);
    }

    public function testReadSkipsUnknownField()
    {
        // Write a struct with field id=99 (unknown to our spec)
        $transport = new TMemoryBuffer();
        $protocol = new TBinaryProtocol($transport);

        $protocol->writeStructBegin('Test');
        $protocol->writeFieldBegin('unknown', TType::STRING, 99);
        $protocol->writeString('should be skipped');
        $protocol->writeFieldEnd();
        $protocol->writeFieldBegin('stringField', TType::STRING, 1);
        $protocol->writeString('known');
        $protocol->writeFieldEnd();
        $protocol->writeFieldStop();
        $protocol->writeStructEnd();

        $result = new TestRichException();
        $readTransport = new TMemoryBuffer($transport->getBuffer());
        $readProtocol = new TBinaryProtocol($readTransport);
        $result->read($readProtocol);

        $this->assertEquals('known', $result->stringField);
    }

    public function testReadSkipsMismatchedFieldType()
    {
        // Write field id=1 as I32 instead of STRING
        $transport = new TMemoryBuffer();
        $protocol = new TBinaryProtocol($transport);

        $protocol->writeStructBegin('Test');
        $protocol->writeFieldBegin('stringField', TType::I32, 1);
        $protocol->writeI32(999);
        $protocol->writeFieldEnd();
        $protocol->writeFieldBegin('intField', TType::I32, 2);
        $protocol->writeI32(42);
        $protocol->writeFieldEnd();
        $protocol->writeFieldStop();
        $protocol->writeStructEnd();

        $result = new TestRichException();
        $readTransport = new TMemoryBuffer($transport->getBuffer());
        $readProtocol = new TBinaryProtocol($readTransport);
        $result->read($readProtocol);

        $this->assertNull($result->stringField);
        $this->assertEquals(42, $result->intField);
    }

    private function roundtrip(TestRichException $exception): TestRichException
    {
        $transport = new TMemoryBuffer();
        $protocol = new TBinaryProtocol($transport);
        $exception->write($protocol);

        $result = new TestRichException();
        $readTransport = new TMemoryBuffer($transport->getBuffer());
        $readProtocol = new TBinaryProtocol($readTransport);
        $result->read($readProtocol);

        return $result;
    }
}
