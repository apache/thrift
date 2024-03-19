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

namespace Test\Thrift\Integration;

use PHPUnit\Framework\TestCase;
use Thrift\Exception\TProtocolException;
use Thrift\Protocol\TBinaryProtocol;
use Thrift\Transport\TMemoryBuffer;

abstract class BaseValidatorTest extends TestCase
{
    abstract public function getNsGlobal();

    public function testEmptyStructValidator()
    {
        $this->assertNoReadValidator($this->getNsGlobal() . '\ThriftTest\EmptyStruct');
        $this->assertNoWriteValidator($this->getNsGlobal() . '\ThriftTest\EmptyStruct');
    }

    public function testBonkValidator()
    {
        $this->assertNoReadValidator($this->getNsGlobal() . '\ThriftTest\Bonk');
        $this->assertHasWriteValidator($this->getNsGlobal() . '\ThriftTest\Bonk');
    }

    public function testStructAValidator()
    {
        $this->assertHasReadValidator($this->getNsGlobal() . '\ThriftTest\StructA');
        $this->assertHasWriteValidator($this->getNsGlobal() . '\ThriftTest\StructA');
    }

    public function testUnionOfStringsValidator()
    {
        $this->assertNoWriteValidator($this->getNsGlobal() . '\TestValidators\UnionOfStrings');
    }

    public function testServiceResultValidator()
    {
        $this->assertNoReadValidator($this->getNsGlobal() . '\TestValidators\TestService_test_result');
        $this->assertNoWriteValidator($this->getNsGlobal() . '\TestValidators\TestService_test_result');
    }

    public function testReadEmpty()
    {
        $className = $this->getNsGlobal() . '\ThriftTest\Bonk';
        $bonk = new $className();
        $transport = new TMemoryBuffer("\000");
        $protocol = new TBinaryProtocol($transport);
        $bonk->read($protocol);
        $this->assertTrue(true);
    }

    public function testWriteEmpty()
    {
        $className = $this->getNsGlobal() . '\ThriftTest\Bonk';
        $bonk = new $className();
        $transport = new TMemoryBuffer();
        $protocol = new TBinaryProtocol($transport);
        try {
            $bonk->write($protocol);
            $this->fail('Bonk was able to write an empty object');
        } catch (TProtocolException $e) {
            $this->expectExceptionMessage('Required field Bonk.message is unset!');
            throw $e;
        }
    }

    public function testWriteWithMissingRequired()
    {
        // Check that we are not able to write StructA with a missing required field
        $className = $this->getNsGlobal() . '\ThriftTest\StructA';
        $structa = new $className();
        $transport = new TMemoryBuffer();
        $protocol = new TBinaryProtocol($transport);

        try {
            $structa->write($protocol);
            $this->fail('StructA was able to write an empty object');
        } catch (TProtocolException $e) {
            $this->expectExceptionMessage('Required field StructA.s is unset!');
            throw $e;
        }
    }

    public function testReadStructA()
    {
        $transport = new TMemoryBuffer(base64_decode('CwABAAAAA2FiYwA='));
        $protocol = new TBinaryProtocol($transport);
        $className = $this->getNsGlobal() . '\ThriftTest\StructA';
        $structa = new $className();
        $structa->read($protocol);
        $this->assertEquals("abc", $structa->s);
    }

    public function testWriteStructA()
    {
        $transport = new TMemoryBuffer();
        $protocol = new TBinaryProtocol($transport);
        $className = $this->getNsGlobal() . '\ThriftTest\StructA';
        $structa = new $className();
        $structa->s = "abc";
        $structa->write($protocol);
        $writeResult = base64_encode($transport->getBuffer());
        $this->assertEquals('CwABAAAAA2FiYwA=', $writeResult);
    }

    protected static function assertHasReadValidator($class)
    {
        if (!static::hasReadValidator($class)) {
            static::fail($class . ' class should have a read validator');
        } else {
            static::assertTrue(true);
        }
    }

    protected static function assertNoReadValidator($class)
    {
        if (static::hasReadValidator($class)) {
            static::fail($class . ' class should not have a write validator');
        } else {
            static::assertTrue(true);
        }
    }

    protected static function assertHasWriteValidator($class)
    {
        if (!static::hasWriteValidator($class)) {
            static::fail($class . ' class should have a write validator');
        } else {
            static::assertTrue(true);
        }
    }

    protected static function assertNoWriteValidator($class)
    {
        if (static::hasWriteValidator($class)) {
            static::fail($class . ' class should not have a write validator');
        } else {
            static::assertTrue(true);
        }
    }

    private static function hasReadValidator($class)
    {
        $rc = new \ReflectionClass($class);

        return $rc->hasMethod('_validateForRead');
    }

    private static function hasWriteValidator($class)
    {
        $rc = new \ReflectionClass($class);

        return $rc->hasMethod('_validateForWrite');
    }
}
