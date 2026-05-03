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

use PHPUnit\Framework\Attributes\DataProvider;
use PHPUnit\Framework\TestCase;
use Thrift\Protocol\TBinaryProtocol;
use Thrift\Protocol\TCompactProtocol;
use Thrift\Transport\TFramedTransport;
use Thrift\Transport\TMemoryBuffer;

/**
 * Pins the byte-count contract for string serialization across protocols
 * and transports.
 *
 * Thrift's binary protocols length-prefix strings with the *byte* count of
 * the UTF-8 payload, not the character count. Until THRIFT-5759 this was
 * guaranteed by a TStringFunc abstraction that detected the (since-removed)
 * mbstring.func_overload PHP ini setting. After the cleanup, native
 * strlen()/substr() must continue to deliver the same byte-oriented
 * behavior — these tests fail if anything regresses to character counting.
 */
class StringByteCountTest extends TestCase
{
    #[DataProvider('utf8StringsProvider')]
    public function testTBinaryProtocolWritesByteLengthPrefix(string $value, int $expectedByteLength): void
    {
        $transport = new TMemoryBuffer();
        $protocol = new TBinaryProtocol($transport);

        $protocol->writeString($value);

        $wire = $transport->getBuffer();
        $this->assertSame($expectedByteLength + 4, strlen($wire));

        $unpacked = unpack('N', substr($wire, 0, 4));
        $this->assertSame($expectedByteLength, $unpacked[1]);
        $this->assertSame($value, substr($wire, 4));
    }

    #[DataProvider('utf8StringsProvider')]
    public function testTBinaryProtocolRoundTripsUtf8(string $value, int $expectedByteLength): void
    {
        $transport = new TMemoryBuffer();
        $protocol = new TBinaryProtocol($transport);

        $protocol->writeString($value);

        $read = '';
        $protocol->readString($read);

        $this->assertSame($value, $read);
        $this->assertSame($expectedByteLength, strlen($read));
    }

    #[DataProvider('utf8StringsProvider')]
    public function testTCompactProtocolRoundTripsUtf8(string $value, int $expectedByteLength): void
    {
        $transport = new TMemoryBuffer();
        $protocol = new TCompactProtocol($transport);

        $protocol->writeString($value);

        $read = '';
        $protocol->readString($read);

        $this->assertSame($value, $read);
        $this->assertSame($expectedByteLength, strlen($read));
    }

    #[DataProvider('utf8StringsProvider')]
    public function testTFramedTransportRoundTripsUtf8(string $value, int $expectedByteLength): void
    {
        $inner = new TMemoryBuffer();
        $framed = new TFramedTransport($inner);
        $protocol = new TBinaryProtocol($framed);

        $protocol->writeString($value);
        $framed->flush();

        $reader = new TFramedTransport(new TMemoryBuffer($inner->getBuffer()));
        $readProtocol = new TBinaryProtocol($reader);

        $read = '';
        $readProtocol->readString($read);

        $this->assertSame($value, $read);
        $this->assertSame($expectedByteLength, strlen($read));
    }

    public static function utf8StringsProvider(): iterable
    {
        yield 'ascii' => ['Afrikaans', 9];
        yield 'latin-extended' => ['Aragonés', 9];
        yield 'arabic' => ['العربية', 14];
        yield 'malayalam' => ['മലയാളം', 18];
        yield 'slavic-diacritics' => ['Slovenščina', 13];
        yield 'cyrillic' => ['Українська', 20];
        yield 'urdu' => ['اردو', 8];
        yield 'cjk-han' => ['中文', 6];
        yield 'cjk-cantonese' => ['粵語', 6];
        yield 'emoji-4byte' => ['🚀', 4];
    }
}
