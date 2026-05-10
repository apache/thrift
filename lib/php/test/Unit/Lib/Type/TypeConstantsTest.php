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

namespace Test\Thrift\Unit\Lib\Type;

use PHPUnit\Framework\TestCase;
use Test\Thrift\Unit\Lib\Type\Fixture\CachedConstantStub;
use Thrift\Type\TMessageType;
use Thrift\Type\TType;

class TypeConstantsTest extends TestCase
{
    public function testConstantValuesAreInitializedLazilyAndCached(): void
    {
        $first = CachedConstantStub::get('ITEM');
        $second = CachedConstantStub::get('ITEM');

        $this->assertSame(1, CachedConstantStub::$initCalls);
        $this->assertSame($first, $second);
        $this->assertSame(1, $first->value);
    }

    public function testMessageTypeConstantsRemainStable(): void
    {
        $this->assertSame(1, TMessageType::CALL);
        $this->assertSame(2, TMessageType::REPLY);
        $this->assertSame(3, TMessageType::EXCEPTION);
        $this->assertSame(4, TMessageType::ONEWAY);
    }

    public function testTypeConstantsRemainStable(): void
    {
        $this->assertSame(0, TType::STOP);
        $this->assertSame(1, TType::VOID);
        $this->assertSame(2, TType::BOOL);
        $this->assertSame(3, TType::BYTE);
        $this->assertSame(TType::BYTE, TType::I08);
        $this->assertSame(4, TType::DOUBLE);
        $this->assertSame(6, TType::I16);
        $this->assertSame(8, TType::I32);
        $this->assertSame(10, TType::I64);
        $this->assertSame(11, TType::STRING);
        $this->assertSame(TType::STRING, TType::UTF7);
        $this->assertSame(12, TType::STRUCT);
        $this->assertSame(13, TType::MAP);
        $this->assertSame(14, TType::SET);
        $this->assertSame(15, TType::LST);
        $this->assertSame(16, TType::UUID);
        $this->assertSame(TType::UUID, TType::UTF8);
        $this->assertSame(17, TType::UTF16);
    }
}
