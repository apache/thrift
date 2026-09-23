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

namespace Test\Thrift\Unit\Lib\Protocol;

use PHPUnit\Framework\TestCase;
use ReflectionProperty;
use Thrift\Exception\TException;
use Thrift\Exception\TProtocolException;
use Thrift\Exception\TTransportException;
use Thrift\Factory\TJSONProtocolFactory;
use Thrift\Protocol\TJSONProtocol;
use Thrift\Protocol\TProtocol;
use Thrift\Transport\TTransport;

/**
 * A JSON string or number carries no length up front: it ends where its
 * closing quote or its last digit is. TJSONProtocol holds each one to the
 * maximum string size of TBinaryProtocol and TCompactProtocol while it reads
 * it, counting the bytes between the quotes as they stand on the wire.
 */
class JsonStringSizeLimitTest extends TestCase
{
    private const MAX = 1024;

    /**
     * Bytes a refused read may take beyond the maximum: the syntax around the
     * value and the byte that went over.
     */
    private const SLACK = 8;

    public function testTheDefaultIsTheBinaryDefault(): void
    {
        $protocol = new TJSONProtocol(self::transport(''));
        $this->assertSame(
            TProtocol::DEFAULT_MAX_STRING_SIZE,
            (new ReflectionProperty($protocol, 'maxStringSize'))->getValue($protocol)
        );

        $protocol = (new TJSONProtocolFactory())->getProtocol(self::transport(''));
        $this->assertSame(
            TProtocol::DEFAULT_MAX_STRING_SIZE,
            (new ReflectionProperty($protocol, 'maxStringSize'))->getValue($protocol)
        );
    }

    public function testAMethodNameOverTheMaximumIsRefusedWhileItIsRead(): void
    {
        $trans = self::transport('[1,"' . str_repeat('A', 2000) . '",1,1]');
        $code = self::readFails(static function () use ($trans): void {
            (new TJSONProtocol($trans, self::MAX))->readMessageBegin($name, $type, $seqid);
        });

        $this->assertLessThanOrEqual(self::MAX + self::SLACK, $trans->consumed, 'bytes read');
        $this->assertSame(TProtocolException::SIZE_LIMIT, $code);
    }

    public function testANumberOverTheMaximumIsRefusedWhileItIsRead(): void
    {
        $trans = self::transport('[' . str_repeat('1', 5000) . ']');
        $code = self::readFails(static function () use ($trans): void {
            (new TJSONProtocol($trans, self::MAX))->readMessageBegin($name, $type, $seqid);
        });

        $this->assertLessThanOrEqual(self::MAX + self::SLACK, $trans->consumed, 'bytes read');
        $this->assertSame(TProtocolException::SIZE_LIMIT, $code);
    }

    public function testTheMaximumIsTheBoundary(): void
    {
        $protocol = new TJSONProtocol(self::transport('"' . str_repeat('A', self::MAX) . '"'), self::MAX);
        $protocol->readString($str);
        $this->assertSame(str_repeat('A', self::MAX), $str);

        $trans = self::transport('"' . str_repeat('A', self::MAX + 1) . '"');
        $code = self::readFails(static function () use ($trans): void {
            (new TJSONProtocol($trans, self::MAX))->readString($str);
        });
        $this->assertSame(TProtocolException::SIZE_LIMIT, $code);

        $protocol = new TJSONProtocol(self::transport(str_repeat('0', self::MAX) . ','), self::MAX);
        $protocol->readI32($i32);
        $this->assertSame(0, $i32);

        $trans = self::transport(str_repeat('0', self::MAX + 1) . ',');
        $code = self::readFails(static function () use ($trans): void {
            (new TJSONProtocol($trans, self::MAX))->readI32($i32);
        });
        $this->assertSame(TProtocolException::SIZE_LIMIT, $code);
    }

    public function testEscapesCountAsTheyStandOnTheWire(): void
    {
        // Each \n is two bytes on the wire and one in the value.
        $wire = '"' . str_repeat('\n', self::MAX / 2) . '"';
        $protocol = new TJSONProtocol(self::transport($wire), self::MAX);
        $protocol->readString($str);
        $this->assertSame(str_repeat("\n", self::MAX / 2), $str);

        $trans = self::transport('"' . str_repeat('\n', self::MAX / 2) . 'A"');
        $code = self::readFails(static function () use ($trans): void {
            (new TJSONProtocol($trans, self::MAX))->readString($str);
        });
        $this->assertSame(TProtocolException::SIZE_LIMIT, $code);
    }

    public function testAMaximumOfZeroReadsAnyLength(): void
    {
        $long = str_repeat('A', 20000);
        $protocol = new TJSONProtocol(self::transport('"' . $long . '"'), 0);
        $protocol->readString($str);
        $this->assertSame($long, $str);

        $protocol = new TJSONProtocol(self::transport(str_repeat('0', 20000) . ','), 0);
        $protocol->readI32($i32);
        $this->assertSame(0, $i32);
    }

    public function testTheFactoryPassesItsMaximum(): void
    {
        $factory = new TJSONProtocolFactory(100);

        $factory->getProtocol(self::transport('"' . str_repeat('A', 100) . '"'))->readString($str);
        $this->assertSame(str_repeat('A', 100), $str);

        $trans = self::transport('"' . str_repeat('A', 101) . '"');
        $code = self::readFails(static function () use ($factory, $trans): void {
            $factory->getProtocol($trans)->readString($str);
        });
        $this->assertSame(TProtocolException::SIZE_LIMIT, $code);
    }

    /**
     * Runs a read that has to fail and returns the TProtocolException code;
     * a read that succeeds or fails otherwise gives -1.
     */
    private static function readFails(callable $read): int
    {
        try {
            $read();
        } catch (TProtocolException $e) {
            return $e->getCode();
        } catch (TException $e) {
            return -1;
        }

        return -1;
    }

    /**
     * A transport over the given bytes that counts how many were read from it.
     */
    private static function transport(string $bytes): TTransport
    {
        return new class ($bytes) extends TTransport {
            public int $consumed = 0;

            public function __construct(private string $bytes)
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
                $out = substr($this->bytes, $this->consumed, $len);
                if ($out === '') {
                    throw new TTransportException('no more data');
                }
                $this->consumed += strlen($out);

                return $out;
            }

            public function write(string $buf): void
            {
            }
        };
    }
}
