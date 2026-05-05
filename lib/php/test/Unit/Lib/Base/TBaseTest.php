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

namespace Test\Thrift\Unit\Lib\Base;

use PHPUnit\Framework\TestCase;
use Test\Thrift\Unit\Lib\Base\Fixture\ComplexStruct;
use Test\Thrift\Unit\Lib\Base\Fixture\NestedStruct;
use Thrift\Protocol\TBinaryProtocol;
use Thrift\Transport\TMemoryBuffer;
use Thrift\Type\TType;

class TBaseTest extends TestCase
{
    public function testConstructorHydratesKnownFieldsFromSpec(): void
    {
        $struct = new ComplexStruct(
            ComplexStruct::$tspec,
            [
                'flag' => true,
                'name' => 'hydrated',
            ]
        );

        $this->assertTrue($struct->flag);
        $this->assertSame('hydrated', $struct->name);
        $this->assertNull($struct->child);
    }

    public function testWakeupPreservesExistingState(): void
    {
        $struct = $this->createComplexStruct();

        /** @var ComplexStruct $restored */
        $restored = unserialize(serialize($struct));

        $this->assertEquals($struct, $restored);
    }

    public function testReadAndWriteRoundTripNestedContainers(): void
    {
        $restored = $this->roundTrip($this->createComplexStruct());

        $this->assertTrue($restored->flag);
        $this->assertSame('root', $restored->name);
        $this->assertInstanceOf(NestedStruct::class, $restored->child);
        $this->assertSame('child', $restored->child->value);
        $this->assertSame(['alpha' => 1, 'beta' => 2], $restored->mapField);
        $this->assertCount(2, $restored->listField);
        $this->assertSame('first', $restored->listField[0]->value);
        $this->assertSame('second', $restored->listField[1]->value);
        $this->assertSame([10 => true, 20 => true], $restored->setField);
        $this->assertSame([1 => [3, 4], 2 => [5]], $restored->mapOfLists);
        $this->assertNull($restored->optionalField);
    }

    public function testReadSkipsUnknownAndUnexpectedFields(): void
    {
        $transport = new TMemoryBuffer();
        $protocol = new TBinaryProtocol($transport);

        $protocol->writeStructBegin('ComplexStruct');

        $protocol->writeFieldBegin('flag', TType::STRING, 1);
        $protocol->writeString('ignored');
        $protocol->writeFieldEnd();

        $protocol->writeFieldBegin('unknown', TType::I32, 99);
        $protocol->writeI32(123);
        $protocol->writeFieldEnd();

        $protocol->writeFieldBegin('name', TType::STRING, 2);
        $protocol->writeString('kept');
        $protocol->writeFieldEnd();

        $protocol->writeFieldStop();
        $protocol->writeStructEnd();

        $struct = new ComplexStruct();
        $struct->read(new TBinaryProtocol($transport));

        $this->assertNull($struct->flag);
        $this->assertSame('kept', $struct->name);
        $this->assertNull($struct->child);
    }

    private function roundTrip(ComplexStruct $struct): ComplexStruct
    {
        $transport = new TMemoryBuffer();
        $writer = new TBinaryProtocol($transport);
        $struct->write($writer);

        $copy = new ComplexStruct();
        $copy->read(new TBinaryProtocol($transport));

        return $copy;
    }

    private function createComplexStruct(): ComplexStruct
    {
        $child = new NestedStruct(NestedStruct::$tspec, ['value' => 'child']);
        $first = new NestedStruct(NestedStruct::$tspec, ['value' => 'first']);
        $second = new NestedStruct(NestedStruct::$tspec, ['value' => 'second']);

        return new ComplexStruct(
            ComplexStruct::$tspec,
            [
                'flag' => true,
                'name' => 'root',
                'child' => $child,
                'mapField' => ['alpha' => 1, 'beta' => 2],
                'listField' => [$first, $second],
                'setField' => [10 => true, 20 => true],
                'mapOfLists' => [1 => [3, 4], 2 => [5]],
            ]
        );
    }
}
