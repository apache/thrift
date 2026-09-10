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

use PHPUnit\Framework\Attributes\DataProvider;
use PHPUnit\Framework\TestCase;
use Thrift\Exception\TException;
use Thrift\Exception\TProtocolException;
use Thrift\Factory\TBinaryProtocolAcceleratedFactory;
use Thrift\Factory\TBinaryProtocolFactory;
use Thrift\Factory\TCompactProtocolFactory;
use Thrift\Protocol\TBinaryProtocol;
use Thrift\Protocol\TBinaryProtocolAccelerated;
use Thrift\Protocol\TCompactProtocol;
use Thrift\Protocol\TProtocol;
use Thrift\Transport\TMemoryBuffer;

/**
 * The binary and compact protocols read a string's length off the wire and ask
 * the transport for that many bytes. These tests hold them to a maximum string
 * size, and check that a refused length is never asked of the transport.
 */
class StringSizeLimitTest extends TestCase
{
    private const LIMIT = 16384000;
    private const HUGE = 0x7fffffff;

    public static function kinds(): array
    {
        return [
            'binary' => ['binary'],
            'binary accelerated' => ['accelerated'],
            'compact' => ['compact'],
        ];
    }

    public static function binaryKinds(): array
    {
        return [
            'binary' => ['binary'],
            'binary accelerated' => ['accelerated'],
        ];
    }

    public function testTheDefaultIsExported(): void
    {
        $this->assertSame(self::LIMIT, \constant(TProtocol::class . '::DEFAULT_MAX_STRING_SIZE'));
    }

    #[DataProvider('kinds')]
    public function testAStringOverTheDefaultMaximumIsRefusedUnread(string $kind): void
    {
        $trans = self::recording(self::length($kind, self::HUGE));
        $proto = self::make($kind, $trans);
        $this->assertRefusedUnread($trans, self::HUGE, static function () use ($proto): void {
            $proto->readString($str);
        });
    }

    #[DataProvider('binaryKinds')]
    public function testAnOldStyleMessageNameIsBounded(string $kind): void
    {
        // Without strictRead a non-negative first word is the method name's length.
        $trans = self::recording(pack('N', self::HUGE));
        $proto = self::make($kind, $trans);
        $this->assertRefusedUnread($trans, self::HUGE, static function () use ($proto): void {
            $proto->readMessageBegin($name, $type, $seqid);
        });
    }

    public function testTheDefaultMaximumIsTheBoundary(): void
    {
        $trans = self::recording(pack('N', self::LIMIT + 1));
        $proto = self::make('binary', $trans);
        $this->assertRefusedUnread($trans, self::LIMIT + 1, static function () use ($proto): void {
            $proto->readString($str);
        });

        $trans = self::recording(pack('N', self::LIMIT));
        try {
            self::make('binary', $trans)->readString($str);
        } catch (TException $e) {
            // runs out of data; what matters is that the length was asked for
        }
        $this->assertContains(self::LIMIT, $trans->requested);
    }

    #[DataProvider('kinds')]
    public function testACallerSetMaximumIsHonoured(string $kind): void
    {
        $trans = self::recording(self::length($kind, 101) . str_repeat('x', 101));
        $proto = self::make($kind, $trans, 100);
        $this->assertRefusedUnread($trans, 101, static function () use ($proto): void {
            $proto->readString($str);
        });

        $trans = self::recording(self::length($kind, 100) . str_repeat('x', 100));
        self::make($kind, $trans, 100)->readString($str);
        $this->assertSame(str_repeat('x', 100), $str);
    }

    public function testAMaximumOfZeroReadsAnyLength(): void
    {
        $trans = self::recording(pack('N', self::LIMIT + 1));
        try {
            self::make('binary', $trans, 0)->readString($str);
        } catch (TException $e) {
            // runs out of data; what matters is that the length was asked for
        }
        $this->assertContains(self::LIMIT + 1, $trans->requested);
    }

    public function testFactoriesPassTheirMaximum(): void
    {
        $protocols = [
            'binary' => (new TBinaryProtocolFactory(false, false, 100))->getProtocol(self::recording(pack('N', 101))),
            'binary accelerated' => (new TBinaryProtocolAcceleratedFactory(false, true, 100))->getProtocol(self::recording(pack('N', 101))),
            'compact' => (new TCompactProtocolFactory(100))->getProtocol(self::recording(chr(101))),
        ];
        foreach ($protocols as $name => $proto) {
            try {
                $proto->readString($str);
                $this->fail("$name: a length over the factory's maximum was not refused");
            } catch (TProtocolException $e) {
                $this->assertSame(TProtocolException::SIZE_LIMIT, $e->getCode(), $name);
            }
        }
    }

    private function assertRefusedUnread(TMemoryBuffer $trans, int $declared, callable $read): void
    {
        try {
            $read();
            $this->fail('the declared length was not refused');
        } catch (TProtocolException $e) {
            $this->assertSame(TProtocolException::SIZE_LIMIT, $e->getCode());
        }
        foreach ($trans->requested as $len) {
            $this->assertLessThan($declared, $len, 'the refused length was asked of the transport');
        }
    }

    private static function recording(string $bytes): TMemoryBuffer
    {
        return new class ($bytes) extends TMemoryBuffer {
            /** @var int[] */
            public array $requested = [];

            public function readAll(int $len): string
            {
                $this->requested[] = $len;

                return parent::readAll($len);
            }
        };
    }

    private static function make(string $kind, TMemoryBuffer $trans, ?int $max = null): TProtocol
    {
        $extra = $max === null ? [] : [$max];

        return match ($kind) {
            'binary' => new TBinaryProtocol($trans, false, true, ...$extra),
            'accelerated' => new TBinaryProtocolAccelerated($trans, false, true, ...$extra),
            'compact' => new TCompactProtocol($trans, ...$extra),
        };
    }

    private static function length(string $kind, int $len): string
    {
        if ($kind !== 'compact') {
            return pack('N', $len);
        }
        $out = '';
        do {
            $byte = $len & 0x7f;
            $len >>= 7;
            $out .= chr($len ? ($byte | 0x80) : $byte);
        } while ($len);

        return $out;
    }
}
