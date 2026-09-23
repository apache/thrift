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

use PHPUnit\Framework\Attributes\DataProvider;
use PHPUnit\Framework\TestCase;
use Thrift\Exception\TProtocolException;
use Thrift\Protocol\TBinaryProtocol;
use Thrift\Protocol\TProtocol;
use Thrift\Transport\TMemoryBuffer;
use Thrift\Type\TType;

/**
 * The recursion-depth limit through the code "--gen php:inlined" emits, over
 * the recursive struct (RecTree), union (RecUnion) and exception (RecError)
 * from RecursionDepth.thrift.
 *
 * Inlined readers decode the binary encoding straight from a TTransport, with
 * no TProtocol in between, and hand fields they do not know to
 * TProtocol::skipBinary(). Both draw on a depth budget the transport keeps,
 * which is as large as the one RecursionDepthTest covers for the protocol.
 */
class RecursionDepthInlinedTest extends TestCase
{
    private const LIMIT = TProtocol::DEFAULT_RECURSION_DEPTH; // 64

    /** Each type's recursive field (list<self>) has id 1; no type declares 99. */
    private const RECURSIVE_FIELD = 1;
    private const UNKNOWN_FIELD = 99;

    /**
     * @return array<string, array{0: class-string, 1: string}>
     */
    public static function classProvider(): array
    {
        return [
            'struct' => ['\PhpRecInline\RecTree', 'item'],
            'union' => ['\PhpRecInline\RecUnion', 'leaf'],
            'exception' => ['\PhpRecInline\RecError', 'leaf'],
        ];
    }

    #[DataProvider('classProvider')]
    public function testRoundTripsAtTheDepthLimit(string $class, string $leafField): void
    {
        $buffer = '';
        $this->makeChain($class, $leafField, self::LIMIT)->write($buffer);

        $decoded = new $class();
        $decoded->read(new TMemoryBuffer($buffer));

        $this->assertSame(self::LIMIT, $this->chainDepth($decoded));
    }

    #[DataProvider('classProvider')]
    public function testReadingPastTheDepthLimitThrows(string $class, string $leafField): void
    {
        $payload = $this->nestInLists(self::LIMIT + 1, self::RECURSIVE_FIELD);

        $error = $this->captureThrowable(function () use ($class, $payload) {
            (new $class())->read(new TMemoryBuffer($payload));
        });

        $this->assertDepthLimitExceeded($error);
    }

    #[DataProvider('classProvider')]
    public function testSkippingAtTheDepthLimitSucceeds(string $class, string $leafField): void
    {
        // The unknown field nests structs in structs, a level each, so the
        // skipped nesting meets the limit where the generated reader does.
        $transport = new TMemoryBuffer($this->nestInStructs(self::LIMIT, self::UNKNOWN_FIELD));

        $decoded = new $class();
        $decoded->read($transport);

        // the unknown field was skipped whole, leaving an otherwise empty object
        $this->assertSame(1, $this->chainDepth($decoded));
        $this->assertSame(0, (int)$transport->available());
    }

    #[DataProvider('classProvider')]
    public function testSkippingPastTheDepthLimitThrows(string $class, string $leafField): void
    {
        $payload = $this->nestInStructs(self::LIMIT + 1, self::UNKNOWN_FIELD);

        $error = $this->captureThrowable(function () use ($class, $payload) {
            (new $class())->read(new TMemoryBuffer($payload));
        });

        $this->assertDepthLimitExceeded($error);
    }

    #[DataProvider('classProvider')]
    public function testSkippingChargesALevelForEachList(string $class, string $leafField): void
    {
        // Like TProtocol::skip(), skipBinary() charges a level for a list as
        // well as for a struct, so a list at every level halves the nesting.
        $half = intdiv(self::LIMIT, 2);

        $decoded = new $class();
        $decoded->read(new TMemoryBuffer($this->nestInLists($half, self::UNKNOWN_FIELD)));
        $this->assertSame(1, $this->chainDepth($decoded));

        $payload = $this->nestInLists($half + 1, self::UNKNOWN_FIELD);
        $error = $this->captureThrowable(function () use ($class, $payload) {
            (new $class())->read(new TMemoryBuffer($payload));
        });

        $this->assertDepthLimitExceeded($error);
    }

    /**
     * @return array<string, array{0: int, 1: string}>
     */
    public static function refusedReadProvider(): array
    {
        return [
            'read' => [self::RECURSIVE_FIELD, 'nestInLists'],
            'skip' => [self::UNKNOWN_FIELD, 'nestInStructs'],
        ];
    }

    #[DataProvider('refusedReadProvider')]
    public function testTheTransportReadsAgainAfterARefusedRead(int $fieldId, string $nest): void
    {
        $transport = new TMemoryBuffer($this->$nest(self::LIMIT + 1, $fieldId));
        $error = $this->captureThrowable(function () use ($transport) {
            (new \PhpRecInline\RecTree())->read($transport);
        });
        $this->assertDepthLimitExceeded($error);

        // Drop what the refused read left behind; the next message on the same
        // transport gets the whole budget again.
        $transport->read((int)$transport->available());
        $buffer = '';
        $this->makeChain('\PhpRecInline\RecTree', 'item', self::LIMIT)->write($buffer);
        $transport->write($buffer);

        $decoded = new \PhpRecInline\RecTree();
        $decoded->read($transport);

        $this->assertSame(self::LIMIT, $this->chainDepth($decoded));
    }

    private function assertDepthLimitExceeded(?\Throwable $error): void
    {
        $this->assertInstanceOf(TProtocolException::class, $error);
        $this->assertSame(TProtocolException::DEPTH_LIMIT, $error->getCode());
    }

    private function makeChain(string $class, string $leafField, int $depth): object
    {
        $node = new $class([$leafField => 1]);
        for ($i = 1; $i < $depth; $i++) {
            $node = new $class(['children' => [$node]]);
        }

        return $node;
    }

    private function chainDepth(?object $node): int
    {
        $depth = 0;
        while ($node !== null) {
            $depth++;
            $node = empty($node->children) ? null : $node->children[0];
        }

        return $depth;
    }

    /**
     * Binary-encode $levels nested structs, each but the last holding the
     * next as the only element of a list<struct> in field $fieldId.
     */
    private function nestInLists(int $levels, int $fieldId): string
    {
        $transport = new TMemoryBuffer();
        $output = new TBinaryProtocol($transport);
        for ($i = 1; $i < $levels; $i++) {
            $output->writeStructBegin('Rec');
            $output->writeFieldBegin('children', TType::LST, $fieldId);
            $output->writeListBegin(TType::STRUCT, 1);
        }
        $output->writeStructBegin('Rec');
        $output->writeFieldStop();
        $output->writeStructEnd();
        for ($i = 1; $i < $levels; $i++) {
            $output->writeListEnd();
            $output->writeFieldEnd();
            $output->writeFieldStop();
            $output->writeStructEnd();
        }

        return $transport->getBuffer();
    }

    /**
     * Binary-encode $levels nested structs, each but the last holding the
     * next directly in struct field $fieldId.
     */
    private function nestInStructs(int $levels, int $fieldId): string
    {
        $transport = new TMemoryBuffer();
        $output = new TBinaryProtocol($transport);
        for ($i = 1; $i < $levels; $i++) {
            $output->writeStructBegin('Rec');
            $output->writeFieldBegin('inner', TType::STRUCT, $fieldId);
        }
        $output->writeStructBegin('Rec');
        $output->writeFieldStop();
        $output->writeStructEnd();
        for ($i = 1; $i < $levels; $i++) {
            $output->writeFieldEnd();
            $output->writeFieldStop();
            $output->writeStructEnd();
        }

        return $transport->getBuffer();
    }

    private function captureThrowable(callable $fn): ?\Throwable
    {
        try {
            $fn();
        } catch (\Throwable $e) {
            return $e;
        }

        return null;
    }
}
