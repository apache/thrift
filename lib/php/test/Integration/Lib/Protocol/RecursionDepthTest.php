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
use Thrift\Protocol\TCompactProtocol;
use Thrift\Protocol\TJSONProtocol;
use Thrift\Protocol\TProtocol;
use Thrift\Transport\TMemoryBuffer;
use Thrift\Type\TType;

/**
 * Round-trips the struct/union/exception recursion-depth limit through the
 * generated read/write code (not the protocol counter in isolation), over a
 * recursive struct (RecTree), union (RecUnion) and exception (RecError) from
 * RecursionDepth.thrift.
 *
 * Both generated code paths are covered: the default mode emits an inline
 * read/write loop guarded by the generator, while the "oop" mode delegates to
 * TBase::readStruct/writeStruct which carry the guard. Round-tripping a chain
 * of exactly DEFAULT_RECURSION_DEPTH levels also proves the two guards do not
 * double-count (which would halve the effective limit).
 */
class RecursionDepthTest extends TestCase
{
    private const LIMIT = TProtocol::DEFAULT_RECURSION_DEPTH; // 64

    /**
     * @return array<string, array{0: class-string<TProtocol>, 1: class-string, 2: string}>
     */
    public static function caseProvider(): array
    {
        $modes = [
            'inline' => ['\PhpRec\RecTree', '\PhpRec\RecUnion', '\PhpRec\RecError'],
            'oop' => ['\PhpRecOop\RecTree', '\PhpRecOop\RecUnion', '\PhpRecOop\RecError'],
        ];
        $protocols = [
            'binary' => TBinaryProtocol::class,
            'compact' => TCompactProtocol::class,
            'json' => TJSONProtocol::class,
        ];

        $cases = [];
        foreach ($modes as $mode => [$treeClass, $unionClass, $errorClass]) {
            foreach ($protocols as $protocol => $protocolClass) {
                $cases["$mode/$protocol/struct"] = [$protocolClass, $treeClass, 'item'];
                $cases["$mode/$protocol/union"] = [$protocolClass, $unionClass, 'leaf'];
                $cases["$mode/$protocol/exception"] = [$protocolClass, $errorClass, 'leaf'];
            }
        }

        return $cases;
    }

    #[DataProvider('caseProvider')]
    public function testRoundTripsAtTheDepthLimit(string $protocolClass, string $class, string $leafField): void
    {
        $original = $this->makeChain($class, $leafField, self::LIMIT);

        $buffer = new TMemoryBuffer();
        $original->write(new $protocolClass($buffer));

        $decoded = new $class();
        $decoded->read(new $protocolClass(new TMemoryBuffer($buffer->getBuffer())));

        $this->assertSame(self::LIMIT, $this->chainDepth($decoded));
    }

    #[DataProvider('caseProvider')]
    public function testWritingPastTheDepthLimitThrows(string $protocolClass, string $class, string $leafField): void
    {
        $tooDeep = $this->makeChain($class, $leafField, self::LIMIT + 5);

        $error = $this->captureThrowable(function () use ($tooDeep, $protocolClass) {
            $tooDeep->write(new $protocolClass(new TMemoryBuffer()));
        });

        $this->assertInstanceOf(TProtocolException::class, $error);
        $this->assertSame(TProtocolException::DEPTH_LIMIT, $error->getCode());
    }

    #[DataProvider('caseProvider')]
    public function testReadingPastTheDepthLimitThrows(string $protocolClass, string $class, string $leafField): void
    {
        // Craft the over-limit payload with raw protocol primitives, which are
        // not depth-bounded, so the generated reader recurses through the
        // guarded struct path (field id 1 / list / struct), not skip().
        $writeBuffer = new TMemoryBuffer();
        $this->writeDeepStruct(new $protocolClass($writeBuffer), self::LIMIT + 5);

        $readProtocol = new $protocolClass(new TMemoryBuffer($writeBuffer->getBuffer()));
        $error = $this->captureThrowable(function () use ($readProtocol, $class) {
            (new $class())->read($readProtocol);
        });

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
     * Emit "$levels" nested structs whose recursive field (id 1, list<struct>)
     * holds exactly one child, so a generated reader recurses $levels deep
     * through the guarded struct path.
     */
    private function writeDeepStruct(TProtocol $output, int $levels): void
    {
        for ($i = 0; $i < $levels - 1; $i++) {
            $output->writeStructBegin('Rec');
            $output->writeFieldBegin('children', TType::LST, 1);
            $output->writeListBegin(TType::STRUCT, 1);
        }
        $output->writeStructBegin('Rec');
        $output->writeFieldStop();
        $output->writeStructEnd();
        for ($i = 0; $i < $levels - 1; $i++) {
            $output->writeListEnd();
            $output->writeFieldEnd();
            $output->writeFieldStop();
            $output->writeStructEnd();
        }
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
