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
use Test\Thrift\Unit\Lib\ReflectionHelper;
use Thrift\Exception\TProtocolException;
use Thrift\Protocol\JSON\BaseContext;
use Thrift\Protocol\TJSONProtocol;
use Thrift\Transport\TMemoryBuffer;
use Thrift\Type\TMessageType;
use Thrift\Type\TType;

class TJSONProtocolTest extends TestCase
{
    use ReflectionHelper;

    #[DataProvider('writeAndReadMessageBeginDataProvider')]
    public function testWriteAndReadMessageBegin(string $name, int $type, int $seqid)
    {
        $transport = new TMemoryBuffer();
        $protocol = new TJSONProtocol($transport);

        $protocol->writeMessageBegin($name, $type, $seqid);
        $protocol->writeMessageEnd();

        $protocol->reset();
        $transport = new TMemoryBuffer($transport->getBuffer());
        $protocol = new TJSONProtocol($transport);

        $readName = null;
        $readType = null;
        $readSeqid = null;
        $protocol->readMessageBegin($readName, $readType, $readSeqid);
        $protocol->readMessageEnd();

        $this->assertSame($name, $readName);
        $this->assertSame($type, $readType);
        $this->assertSame($seqid, $readSeqid);
    }

    public static function writeAndReadMessageBeginDataProvider()
    {
        yield 'call message' => [
            'name' => 'testMethod',
            'type' => TMessageType::CALL,
            'seqid' => 1,
        ];
        yield 'reply message' => [
            'name' => 'getResult',
            'type' => TMessageType::REPLY,
            'seqid' => 42,
        ];
        yield 'exception message' => [
            'name' => 'failMethod',
            'type' => TMessageType::EXCEPTION,
            'seqid' => 100,
        ];
        yield 'oneway message' => [
            'name' => 'fireAndForget',
            'type' => TMessageType::ONEWAY,
            'seqid' => 0,
        ];
    }

    public function testReadMessageBeginBadVersion()
    {
        // Manually craft a JSON message with wrong version (99 instead of 1)
        $json = '[99,"testMethod",1,1]';
        $transport = new TMemoryBuffer($json);
        $protocol = new TJSONProtocol($transport);

        $this->expectException(TProtocolException::class);
        $this->expectExceptionCode(TProtocolException::BAD_VERSION);

        $name = null;
        $type = null;
        $seqid = null;
        $protocol->readMessageBegin($name, $type, $seqid);
    }

    #[DataProvider('writeAndReadScalarDataProvider')]
    public function testWriteAndReadScalar(int $fieldType, string $writeMethod, $value, string $readMethod)
    {
        if ($fieldType === TType::I64 && PHP_INT_SIZE === 4) {
            $this->markTestSkipped('64-bit integer tests require 64-bit PHP');
        }

        $transport = new TMemoryBuffer();
        $protocol = new TJSONProtocol($transport);

        $protocol->writeStructBegin('Test');
        $protocol->writeFieldBegin('f', $fieldType, 1);
        $protocol->$writeMethod($value);
        $protocol->writeFieldEnd();
        $protocol->writeFieldStop();
        $protocol->writeStructEnd();

        $transport = new TMemoryBuffer($transport->getBuffer());
        $protocol = new TJSONProtocol($transport);

        $protocol->readStructBegin($name);
        $protocol->readFieldBegin($fname, $ftype, $fid);
        $result = null;
        $protocol->$readMethod($result);
        $protocol->readFieldEnd();
        $protocol->readStructEnd();

        if (is_float($value) && is_nan($value)) {
            $this->assertNan($result);
        } else {
            $this->assertSame($value, $result);
        }
    }

    public static function writeAndReadScalarDataProvider()
    {
        yield 'bool true' => [
            'fieldType' => TType::BOOL,
            'writeMethod' => 'writeBool',
            'value' => true,
            'readMethod' => 'readBool',
        ];
        yield 'bool false' => [
            'fieldType' => TType::BOOL,
            'writeMethod' => 'writeBool',
            'value' => false,
            'readMethod' => 'readBool',
        ];
        yield 'byte zero' => [
            'fieldType' => TType::BYTE,
            'writeMethod' => 'writeByte',
            'value' => 0,
            'readMethod' => 'readByte',
        ];
        yield 'byte positive' => [
            'fieldType' => TType::BYTE,
            'writeMethod' => 'writeByte',
            'value' => 127,
            'readMethod' => 'readByte',
        ];
        yield 'byte negative' => [
            'fieldType' => TType::BYTE,
            'writeMethod' => 'writeByte',
            'value' => -128,
            'readMethod' => 'readByte',
        ];
        yield 'byte one' => [
            'fieldType' => TType::BYTE,
            'writeMethod' => 'writeByte',
            'value' => 1,
            'readMethod' => 'readByte',
        ];
        yield 'i16 zero' => [
            'fieldType' => TType::I16,
            'writeMethod' => 'writeI16',
            'value' => 0,
            'readMethod' => 'readI16',
        ];
        yield 'i16 positive' => [
            'fieldType' => TType::I16,
            'writeMethod' => 'writeI16',
            'value' => 32767,
            'readMethod' => 'readI16',
        ];
        yield 'i16 negative' => [
            'fieldType' => TType::I16,
            'writeMethod' => 'writeI16',
            'value' => -32768,
            'readMethod' => 'readI16',
        ];
        yield 'i16 small positive' => [
            'fieldType' => TType::I16,
            'writeMethod' => 'writeI16',
            'value' => 256,
            'readMethod' => 'readI16',
        ];
        yield 'i32 zero' => [
            'fieldType' => TType::I32,
            'writeMethod' => 'writeI32',
            'value' => 0,
            'readMethod' => 'readI32',
        ];
        yield 'i32 positive' => [
            'fieldType' => TType::I32,
            'writeMethod' => 'writeI32',
            'value' => 2147483647,
            'readMethod' => 'readI32',
        ];
        yield 'i32 negative' => [
            'fieldType' => TType::I32,
            'writeMethod' => 'writeI32',
            'value' => -2147483648,
            'readMethod' => 'readI32',
        ];
        yield 'i32 small negative' => [
            'fieldType' => TType::I32,
            'writeMethod' => 'writeI32',
            'value' => -1,
            'readMethod' => 'readI32',
        ];
        yield 'i32 medium value' => [
            'fieldType' => TType::I32,
            'writeMethod' => 'writeI32',
            'value' => 100000,
            'readMethod' => 'readI32',
        ];
        yield 'i64 zero' => [
            'fieldType' => TType::I64,
            'writeMethod' => 'writeI64',
            'value' => 0,
            'readMethod' => 'readI64',
        ];
        yield 'i64 positive' => [
            'fieldType' => TType::I64,
            'writeMethod' => 'writeI64',
            'value' => 1099511627776,
            'readMethod' => 'readI64',
        ];
        yield 'i64 negative' => [
            'fieldType' => TType::I64,
            'writeMethod' => 'writeI64',
            'value' => -1099511627776,
            'readMethod' => 'readI64',
        ];
        yield 'i64 max int32' => [
            'fieldType' => TType::I64,
            'writeMethod' => 'writeI64',
            'value' => 2147483647,
            'readMethod' => 'readI64',
        ];
        yield 'i64 small value' => [
            'fieldType' => TType::I64,
            'writeMethod' => 'writeI64',
            'value' => 42,
            'readMethod' => 'readI64',
        ];
        yield 'double zero' => [
            'fieldType' => TType::DOUBLE,
            'writeMethod' => 'writeDouble',
            'value' => 0.0,
            'readMethod' => 'readDouble',
        ];
        yield 'double positive' => [
            'fieldType' => TType::DOUBLE,
            'writeMethod' => 'writeDouble',
            'value' => 3.14159265358979,
            'readMethod' => 'readDouble',
        ];
        yield 'double negative' => [
            'fieldType' => TType::DOUBLE,
            'writeMethod' => 'writeDouble',
            'value' => -2.718281828,
            'readMethod' => 'readDouble',
        ];
        yield 'double large' => [
            'fieldType' => TType::DOUBLE,
            'writeMethod' => 'writeDouble',
            'value' => 1.7976931348623e+100,
            'readMethod' => 'readDouble',
        ];
        yield 'double small' => [
            'fieldType' => TType::DOUBLE,
            'writeMethod' => 'writeDouble',
            'value' => 1.0e-10,
            'readMethod' => 'readDouble',
        ];
        yield 'double NaN round-trips via "NaN" token' => [
            'fieldType' => TType::DOUBLE,
            'writeMethod' => 'writeDouble',
            'value' => NAN,
            'readMethod' => 'readDouble',
        ];
        yield 'double +Infinity round-trips via "Infinity" token' => [
            'fieldType' => TType::DOUBLE,
            'writeMethod' => 'writeDouble',
            'value' => INF,
            'readMethod' => 'readDouble',
        ];
        yield 'double -Infinity round-trips via "-Infinity" token' => [
            'fieldType' => TType::DOUBLE,
            'writeMethod' => 'writeDouble',
            'value' => -INF,
            'readMethod' => 'readDouble',
        ];
        yield 'string empty' => [
            'fieldType' => TType::STRING,
            'writeMethod' => 'writeString',
            'value' => '',
            'readMethod' => 'readString',
        ];
        yield 'string simple' => [
            'fieldType' => TType::STRING,
            'writeMethod' => 'writeString',
            'value' => 'hello world',
            'readMethod' => 'readString',
        ];
        yield 'string special characters' => [
            'fieldType' => TType::STRING,
            'writeMethod' => 'writeString',
            'value' => "line1\nline2\ttab",
            'readMethod' => 'readString',
        ];
        yield 'string unicode' => [
            'fieldType' => TType::STRING,
            'writeMethod' => 'writeString',
            'value' => 'héllo wörld',
            'readMethod' => 'readString',
        ];
        yield 'string quotes and backslash' => [
            'fieldType' => TType::STRING,
            'writeMethod' => 'writeString',
            'value' => 'say "hello" and use \\path',
            'readMethod' => 'readString',
        ];
        yield 'string json special chars' => [
            'fieldType' => TType::STRING,
            'writeMethod' => 'writeString',
            'value' => '{"key": "value"}',
            'readMethod' => 'readString',
        ];
        yield 'uuid standard' => [
            'fieldType' => TType::UUID,
            'writeMethod' => 'writeUuid',
            'value' => '12345678-1234-5678-1234-567812345678',
            'readMethod' => 'readUuid',
        ];
        yield 'uuid nil' => [
            'fieldType' => TType::UUID,
            'writeMethod' => 'writeUuid',
            'value' => '00000000-0000-0000-0000-000000000000',
            'readMethod' => 'readUuid',
        ];
        yield 'uuid random' => [
            'fieldType' => TType::UUID,
            'writeMethod' => 'writeUuid',
            'value' => 'a1b2c3d4-e5f6-7890-abcd-ef1234567890',
            'readMethod' => 'readUuid',
        ];
    }

    public function testWriteAndReadStruct()
    {
        $transport = new TMemoryBuffer();
        $protocol = new TJSONProtocol($transport);

        $protocol->writeStructBegin('TestStruct');
        $protocol->writeFieldBegin('name', TType::STRING, 1);
        $protocol->writeString('hello');
        $protocol->writeFieldEnd();
        $protocol->writeFieldBegin('age', TType::I32, 2);
        $protocol->writeI32(25);
        $protocol->writeFieldEnd();
        $protocol->writeFieldStop();
        $protocol->writeStructEnd();

        $transport = new TMemoryBuffer($transport->getBuffer());
        $protocol = new TJSONProtocol($transport);

        $name = null;
        $protocol->readStructBegin($name);

        $fieldName = null;
        $fieldType = null;
        $fieldId = null;
        $protocol->readFieldBegin($fieldName, $fieldType, $fieldId);
        $this->assertSame(TType::STRING, $fieldType);
        $this->assertSame(1, $fieldId);
        $str = null;
        $protocol->readString($str);
        $this->assertSame('hello', $str);
        $protocol->readFieldEnd();

        $protocol->readFieldBegin($fieldName, $fieldType, $fieldId);
        $this->assertSame(TType::I32, $fieldType);
        $this->assertSame(2, $fieldId);
        $i32 = null;
        $protocol->readI32($i32);
        $this->assertSame(25, $i32);
        $protocol->readFieldEnd();

        // Read field stop
        $protocol->readFieldBegin($fieldName, $fieldType, $fieldId);
        $this->assertSame(TType::STOP, $fieldType);

        $protocol->readStructEnd();
    }

    public function testWriteAndReadMap()
    {
        $transport = new TMemoryBuffer();
        $protocol = new TJSONProtocol($transport);

        $protocol->writeMapBegin(TType::STRING, TType::I32, 2);
        $protocol->writeString('key1');
        $protocol->writeI32(100);
        $protocol->writeString('key2');
        $protocol->writeI32(200);
        $protocol->writeMapEnd();

        $transport = new TMemoryBuffer($transport->getBuffer());
        $protocol = new TJSONProtocol($transport);

        $keyType = null;
        $valType = null;
        $size = null;
        $protocol->readMapBegin($keyType, $valType, $size);
        $this->assertSame(TType::STRING, $keyType);
        $this->assertSame(TType::I32, $valType);
        $this->assertSame(2, $size);

        $key = null;
        $val = null;
        $protocol->readString($key);
        $protocol->readI32($val);
        $this->assertSame('key1', $key);
        $this->assertSame(100, $val);

        $protocol->readString($key);
        $protocol->readI32($val);
        $this->assertSame('key2', $key);
        $this->assertSame(200, $val);

        $protocol->readMapEnd();
    }

    public function testWriteAndReadList()
    {
        $transport = new TMemoryBuffer();
        $protocol = new TJSONProtocol($transport);

        $protocol->writeListBegin(TType::I32, 3);
        $protocol->writeI32(10);
        $protocol->writeI32(20);
        $protocol->writeI32(30);
        $protocol->writeListEnd();

        $transport = new TMemoryBuffer($transport->getBuffer());
        $protocol = new TJSONProtocol($transport);

        $elemType = null;
        $size = null;
        $protocol->readListBegin($elemType, $size);
        $this->assertSame(TType::I32, $elemType);
        $this->assertSame(3, $size);

        $val = null;
        $protocol->readI32($val);
        $this->assertSame(10, $val);
        $protocol->readI32($val);
        $this->assertSame(20, $val);
        $protocol->readI32($val);
        $this->assertSame(30, $val);

        $protocol->readListEnd();
    }

    public function testWriteAndReadSet()
    {
        $transport = new TMemoryBuffer();
        $protocol = new TJSONProtocol($transport);

        $protocol->writeSetBegin(TType::STRING, 2);
        $protocol->writeString('alpha');
        $protocol->writeString('beta');
        $protocol->writeSetEnd();

        $transport = new TMemoryBuffer($transport->getBuffer());
        $protocol = new TJSONProtocol($transport);

        $elemType = null;
        $size = null;
        $protocol->readSetBegin($elemType, $size);
        $this->assertSame(TType::STRING, $elemType);
        $this->assertSame(2, $size);

        $val = null;
        $protocol->readString($val);
        $this->assertSame('alpha', $val);
        $protocol->readString($val);
        $this->assertSame('beta', $val);

        $protocol->readSetEnd();
    }

    public function testWriteAndReadEmptyMap()
    {
        $transport = new TMemoryBuffer();
        $protocol = new TJSONProtocol($transport);

        $protocol->writeMapBegin(TType::STRING, TType::I32, 0);
        $protocol->writeMapEnd();

        $transport = new TMemoryBuffer($transport->getBuffer());
        $protocol = new TJSONProtocol($transport);

        $keyType = null;
        $valType = null;
        $size = null;
        $protocol->readMapBegin($keyType, $valType, $size);
        $this->assertSame(TType::STRING, $keyType);
        $this->assertSame(TType::I32, $valType);
        $this->assertSame(0, $size);

        $protocol->readMapEnd();
    }

    public function testWriteAndReadEmptyList()
    {
        $transport = new TMemoryBuffer();
        $protocol = new TJSONProtocol($transport);

        $protocol->writeListBegin(TType::BOOL, 0);
        $protocol->writeListEnd();

        $transport = new TMemoryBuffer($transport->getBuffer());
        $protocol = new TJSONProtocol($transport);

        $elemType = null;
        $size = null;
        $protocol->readListBegin($elemType, $size);
        $this->assertSame(TType::BOOL, $elemType);
        $this->assertSame(0, $size);

        $protocol->readListEnd();
    }

    public function testGetTypeNameForUnknownType()
    {
        $transport = new TMemoryBuffer();
        $protocol = new TJSONProtocol($transport);

        $this->expectException(TProtocolException::class);
        $this->expectExceptionCode(TProtocolException::UNKNOWN);

        // Use writeFieldBegin with an invalid type to trigger getTypeNameForTypeID
        $protocol->writeFieldBegin('invalid', 99, 1);
    }

    public function testGetTypeIDForUnknownTypeName()
    {
        // Craft JSON that has an unknown type name in field position
        // A struct with one field: fieldId=1, then object start, then unknown type name
        $json = '{1:{"zzz"';
        $transport = new TMemoryBuffer($json);
        $protocol = new TJSONProtocol($transport);

        $this->expectException(TProtocolException::class);
        $this->expectExceptionCode(TProtocolException::INVALID_DATA);

        $name = null;
        $protocol->readStructBegin($name);

        $fieldName = null;
        $fieldType = null;
        $fieldId = null;
        $protocol->readFieldBegin($fieldName, $fieldType, $fieldId);
    }

    public function testWriteAndReadCompleteMessage()
    {
        $transport = new TMemoryBuffer();
        $protocol = new TJSONProtocol($transport);

        // Write a complete message with a struct containing various types
        $protocol->writeMessageBegin('testFunc', TMessageType::CALL, 7);
        $protocol->writeStructBegin('Args');
        $protocol->writeFieldBegin('flag', TType::BOOL, 1);
        $protocol->writeBool(true);
        $protocol->writeFieldEnd();
        $protocol->writeFieldBegin('count', TType::I32, 2);
        $protocol->writeI32(42);
        $protocol->writeFieldEnd();
        $protocol->writeFieldStop();
        $protocol->writeStructEnd();
        $protocol->writeMessageEnd();

        $transport = new TMemoryBuffer($transport->getBuffer());
        $protocol = new TJSONProtocol($transport);

        $name = null;
        $type = null;
        $seqid = null;
        $protocol->readMessageBegin($name, $type, $seqid);
        $this->assertSame('testFunc', $name);
        $this->assertSame(TMessageType::CALL, $type);
        $this->assertSame(7, $seqid);

        $structName = null;
        $protocol->readStructBegin($structName);

        $fieldName = null;
        $fieldType = null;
        $fieldId = null;

        $protocol->readFieldBegin($fieldName, $fieldType, $fieldId);
        $this->assertSame(TType::BOOL, $fieldType);
        $this->assertSame(1, $fieldId);
        $boolVal = null;
        $protocol->readBool($boolVal);
        $this->assertTrue($boolVal);
        $protocol->readFieldEnd();

        $protocol->readFieldBegin($fieldName, $fieldType, $fieldId);
        $this->assertSame(TType::I32, $fieldType);
        $this->assertSame(2, $fieldId);
        $i32Val = null;
        $protocol->readI32($i32Val);
        $this->assertSame(42, $i32Val);
        $protocol->readFieldEnd();

        $protocol->readFieldBegin($fieldName, $fieldType, $fieldId);
        $this->assertSame(TType::STOP, $fieldType);

        $protocol->readStructEnd();
        $protocol->readMessageEnd();
    }

    public function testWriteAndReadNestedList()
    {
        $transport = new TMemoryBuffer();
        $protocol = new TJSONProtocol($transport);

        // Write a list of lists
        $protocol->writeListBegin(TType::LST, 2);

        $protocol->writeListBegin(TType::I32, 2);
        $protocol->writeI32(1);
        $protocol->writeI32(2);
        $protocol->writeListEnd();

        $protocol->writeListBegin(TType::I32, 2);
        $protocol->writeI32(3);
        $protocol->writeI32(4);
        $protocol->writeListEnd();

        $protocol->writeListEnd();

        $transport = new TMemoryBuffer($transport->getBuffer());
        $protocol = new TJSONProtocol($transport);

        $elemType = null;
        $size = null;
        $protocol->readListBegin($elemType, $size);
        $this->assertSame(TType::LST, $elemType);
        $this->assertSame(2, $size);

        $protocol->readListBegin($elemType, $size);
        $this->assertSame(TType::I32, $elemType);
        $this->assertSame(2, $size);
        $val = null;
        $protocol->readI32($val);
        $this->assertSame(1, $val);
        $protocol->readI32($val);
        $this->assertSame(2, $val);
        $protocol->readListEnd();

        $protocol->readListBegin($elemType, $size);
        $this->assertSame(TType::I32, $elemType);
        $this->assertSame(2, $size);
        $protocol->readI32($val);
        $this->assertSame(3, $val);
        $protocol->readI32($val);
        $this->assertSame(4, $val);
        $protocol->readListEnd();

        $protocol->readListEnd();
    }

    public function testWriteAndReadMapWithIntegerKeys()
    {
        $transport = new TMemoryBuffer();
        $protocol = new TJSONProtocol($transport);

        $protocol->writeMapBegin(TType::I32, TType::STRING, 2);
        $protocol->writeI32(1);
        $protocol->writeString('one');
        $protocol->writeI32(2);
        $protocol->writeString('two');
        $protocol->writeMapEnd();

        $transport = new TMemoryBuffer($transport->getBuffer());
        $protocol = new TJSONProtocol($transport);

        $keyType = null;
        $valType = null;
        $size = null;
        $protocol->readMapBegin($keyType, $valType, $size);
        $this->assertSame(TType::I32, $keyType);
        $this->assertSame(TType::STRING, $valType);
        $this->assertSame(2, $size);

        $key = null;
        $val = null;
        $protocol->readI32($key);
        $protocol->readString($val);
        $this->assertSame(1, $key);
        $this->assertSame('one', $val);

        $protocol->readI32($key);
        $protocol->readString($val);
        $this->assertSame(2, $key);
        $this->assertSame('two', $val);

        $protocol->readMapEnd();
    }

    /**
     * Guards the popContext underflow recovery added when the `$context`
     * property was tightened to non-nullable: an empty stack now yields a
     * default BaseContext instead of returning null and tripping the typed
     * property assignment.
     */
    public function testPopContextOnEmptyStackFallsBackToBaseContext(): void
    {
        $protocol = new TJSONProtocol(new TMemoryBuffer());

        // Reflect into the private popContext()/context to assert the
        // underflow path completes without TypeError.
        $popContext = $this->getAccessibleMethod($protocol, 'popContext');
        $popContext->invoke($protocol);

        $context = $this->getPropertyValue($protocol, 'context');
        $this->assertInstanceOf(BaseContext::class, $context);
    }
}
