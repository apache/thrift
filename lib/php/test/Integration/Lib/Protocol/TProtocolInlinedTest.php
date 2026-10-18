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

namespace Test\Thrift\Integration\Lib\Protocol;

use BasicInline\ThriftTest\Bonk;
use BasicInline\ThriftTest\VersioningTestV1;
use BasicInline\ThriftTest\VersioningTestV2;
use PHPUnit\Framework\TestCase;
use Thrift\Exception\TProtocolException;
use Thrift\Exception\TTransportException;
use Thrift\Transport\TMemoryBuffer;
use Thrift\Transport\TTransport;
use Thrift\Type\TType;

/***
 * This test suite depends on running the compiler against the ./Resources/ThriftTest.thrift file:
 * lib/php/test$ ../../../compiler/cpp/thrift --gen php:inlined,nsglobal="BasicInline" -r --out ./Resources/packages/phpi ./Resources/ThriftTest.thrift
 */
class TProtocolInlinedTest extends TestCase
{
    public static function setUpBeforeClass(): void
    {
        if (!is_dir(__DIR__ . '/../../../Resources/packages/phpi')) {
            self::fail(
                'Before running Integration test suite, you must run the Thrift compiler against the ThriftTest.thrift file in the ./Resources directory with the php:inlined generator.'
            );
        }
    }

    public function testInlinedReaderSkipsUnknownAndMismatchedFields(): void
    {
        $writer = new VersioningTestV2([
            'begin_in_both' => 1,
            'newint' => 2,
            'newbyte' => 3,
            'newshort' => 4,
            'newlong' => 5,
            'newdouble' => 6.25,
            'newstruct' => new Bonk([
                'message' => 'skip me',
                'type' => 7,
            ]),
            'newlist' => [8, 9],
            'newset' => [10, 11],
            'newmap' => [12 => 13],
            'newstring' => 'skip me too',
            'end_in_both' => 14,
        ]);

        $buffer = '';
        $writer->write($buffer);

        $transport = new TMemoryBuffer($buffer);
        $reader = new VersioningTestV1();
        $reader->read($transport);

        $this->assertSame(1, $reader->begin_in_both);
        $this->assertNull($reader->old_string);
        $this->assertSame(14, $reader->end_in_both);
        $this->assertSame(0, (int)$transport->available());
    }

    public function testInlinedReaderRefusesAnOversizeStringBeforeReadingIt(): void
    {
        // Bonk field 1 is a string. Declare the largest positive i32 length
        // (~2 GiB) for it with none of the bytes present: field type, field
        // id, then the length.
        $declared = 0x7fffffff;
        $transport = new class (
            pack('c', TType::STRING) . pack('n', 1) . pack('N', $declared)
        ) extends TTransport {
            public int $maxRequested = 0;
            private int $pos = 0;

            public function __construct(private string $data)
            {
            }

            public function isOpen(): bool
            {
                return true;
            }

            public function open(): void
            {
            }

            public function close(): void
            {
            }

            public function read(int $len): string
            {
                $this->maxRequested = max($this->maxRequested, $len);
                $chunk = substr($this->data, $this->pos, $len);
                $this->pos += strlen($chunk);
                if ($chunk === '') {
                    throw new TTransportException('recording transport exhausted');
                }

                return $chunk;
            }

            public function write(string $buf): void
            {
            }
        };

        try {
            (new Bonk())->read($transport);
            $this->fail('expected a TProtocolException for the oversize string length');
        } catch (TProtocolException $e) {
            $this->assertSame(TProtocolException::SIZE_LIMIT, $e->getCode());
        }

        // The field header and length are read (at most 4 bytes at a time);
        // the declared body is never requested from the transport.
        $this->assertSame(4, $transport->maxRequested);
    }
}
