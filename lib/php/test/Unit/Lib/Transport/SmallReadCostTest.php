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

namespace Test\Thrift\Unit\Lib\Transport;

use Nyholm\Psr7\Factory\Psr17Factory;
use PHPUnit\Framework\Attributes\DataProvider;
use PHPUnit\Framework\TestCase;
use Psr\Http\Client\ClientInterface;
use ReflectionProperty;
use Thrift\Transport\TCurlClient;
use Thrift\Transport\TFramedTransport;
use Thrift\Transport\TMemoryBuffer;
use Thrift\Transport\TPsrHttpClient;
use Thrift\Transport\TTransport;

/**
 * The transports that hold a whole frame or response in memory serve reads
 * from it, and protocols read a few bytes at a time. Draining such a buffer
 * one byte per read must take time in proportion to its size: the same number
 * of reads takes about as long from one buffer as from four buffers a quarter
 * of its size.
 */
class SmallReadCostTest extends TestCase
{
    private const SIZE = 131072;

    /**
     * The ratio is 1 when the time is in proportion to the size; the margin
     * absorbs timing noise.
     */
    private const MAX_RATIO = 2.0;

    public static function transportDataProvider(): array
    {
        return [
            'TFramedTransport' => ['framed'],
            'TMemoryBuffer' => ['memory'],
            'TCurlClient' => ['curl'],
            'TPsrHttpClient' => ['psr'],
        ];
    }

    #[DataProvider('transportDataProvider')]
    public function testDrainingOneByteAtATimeTakesLinearTime(string $kind): void
    {
        $fourBuffers = $this->drainSeconds($kind, 4, self::SIZE);
        $oneBuffer = $this->drainSeconds($kind, 1, 4 * self::SIZE);

        $this->assertLessThan(
            self::MAX_RATIO,
            $oneBuffer / $fourBuffers,
            sprintf(
                '%.3f s for four buffers of %d bytes, %.3f s for one buffer of %d bytes',
                $fourBuffers,
                self::SIZE,
                $oneBuffer,
                4 * self::SIZE
            )
        );
    }

    /**
     * The best of three runs, to keep a busy machine from skewing one of them.
     */
    private function drainSeconds(string $kind, int $buffers, int $size): float
    {
        $best = INF;
        for ($run = 0; $run < 3; $run++) {
            $transports = [];
            for ($i = 0; $i < $buffers; $i++) {
                $transports[] = $this->transport($kind, str_repeat('x', $size));
            }
            $read = 0;
            $start = hrtime(true);
            foreach ($transports as $transport) {
                for ($i = 0; $i < $size; $i++) {
                    $read += strlen($transport->read(1));
                }
            }
            $best = min($best, (hrtime(true) - $start) / 1e9);
            $this->assertSame($buffers * $size, $read);
        }

        return $best;
    }

    private function transport(string $kind, string $data): TTransport
    {
        switch ($kind) {
            case 'framed':
                return new TFramedTransport(new TMemoryBuffer(pack('N', strlen($data)) . $data));
            case 'memory':
                return new TMemoryBuffer($data);
            case 'curl':
                $transport = new TCurlClient('localhost');
                (new ReflectionProperty($transport, 'response'))->setValue($transport, $data);

                return $transport;
            default:
                $psr17 = new Psr17Factory();
                $transport = new TPsrHttpClient(
                    'http://localhost',
                    $this->createStub(ClientInterface::class),
                    $psr17,
                    $psr17
                );
                (new ReflectionProperty($transport, 'response'))->setValue($transport, $data);

                return $transport;
        }
    }
}
