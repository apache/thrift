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
use Thrift\Protocol\TBinaryProtocolAccelerated;
use Thrift\Transport\TBufferedTransport;
use Thrift\Transport\TMemoryBuffer;
use Thrift\Transport\TSocket;
use Thrift\Type\TType;

class TBinaryProtocolAcceleratedTest extends TestCase
{
    #[DataProvider('constructDataProvider')]
    public function testConstruct(
        $transport,
        $expectedTransport
    ) {
        $protocol = new TBinaryProtocolAccelerated($transport);
        $this->assertInstanceOf($expectedTransport, $protocol->getTransport());
    }

    public static function constructDataProvider()
    {
        yield 'not buffered transport' => [
            'transport' => new TMemoryBuffer(),
            'expectedTransport' => TMemoryBuffer::class,
        ];
        yield 'buffered transport' => [
            'transport' => new TSocket(),
            'expectedTransport' => TBufferedTransport::class,
        ];
    }

    #[DataProvider('strictParamsDataProvider')]
    public function testStrictParams($strictRead, $strictWrite)
    {
        $protocol = new TBinaryProtocolAccelerated(new TMemoryBuffer(), $strictRead, $strictWrite);
        $this->assertEquals($strictRead, $protocol->isStrictRead());
        $this->assertEquals($strictWrite, $protocol->isStrictWrite());
    }

    public static function strictParamsDataProvider()
    {
        yield 'strict read and write' => [true, true];
        yield 'not strict read and write' => [false, false];
        yield 'strict read and not strict write' => [true, false];
        yield 'not strict read and strict write' => [false, true];
    }

    /**
     * THRIFT-796: the extension converts values to the declared Thrift type
     * and must do so on a copy, not on the caller's property.
     */
    public function testWriteDoesNotChangeFieldTypes(): void
    {
        if (!function_exists('thrift_protocol_write_binary')) {
            $this->markTestSkipped('thrift_protocol extension not loaded');
        }

        $struct = new class {
            public static $tspec = [
                1 => ['var' => 'num', 'type' => TType::I32],
                2 => ['var' => 'text', 'type' => TType::STRING],
                3 => ['var' => 'real', 'type' => TType::DOUBLE],
                4 => [
                    'var' => 'items',
                    'type' => TType::LST,
                    'etype' => TType::I32,
                    'elem' => ['type' => TType::I32],
                ],
                5 => [
                    'var' => 'scalar',
                    'type' => TType::LST,
                    'etype' => TType::STRING,
                    'elem' => ['type' => TType::STRING],
                ],
            ];
            public static $isValidate = false;
            public $num = '100';
            public $text = 100;
            public $real = '1.5';
            public $items = ['1', 2.0];
            public $scalar = '5';
        };

        $protocol = new TBinaryProtocolAccelerated(new TMemoryBuffer());
        thrift_protocol_write_binary($protocol, 'foo', 1, $struct, 0, true);

        $this->assertSame('100', $struct->num);
        $this->assertSame(100, $struct->text);
        $this->assertSame('1.5', $struct->real);
        $this->assertSame(['1', 2.0], $struct->items);
        $this->assertSame('5', $struct->scalar);
    }
}
