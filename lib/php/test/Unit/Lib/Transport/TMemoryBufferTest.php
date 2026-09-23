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

use PHPUnit\Framework\TestCase;
use PHPUnit\Framework\Attributes\DataProvider;
use Thrift\Exception\TTransportException;
use Thrift\Transport\TMemoryBuffer;

class TMemoryBufferTest extends TestCase
{
    public function testIsOpen()
    {
        $transport = new TMemoryBuffer();
        $this->assertTrue($transport->isOpen());
    }

    public function testOpen()
    {
        $transport = new TMemoryBuffer();
        $this->assertNull($transport->open());
    }

    public function testClose()
    {
        $transport = new TMemoryBuffer();
        $this->assertNull($transport->close());
    }

    public function testReadEmptyBuffer()
    {
        $transport = new TMemoryBuffer();
        $this->expectException(\Thrift\Exception\TTransportException::class);
        $this->expectExceptionMessage("TMemoryBuffer: Could not read 1 bytes from buffer.");
        $this->expectExceptionCode(TTransportException::UNKNOWN);
        $transport->read(1);
    }

    #[DataProvider('readDataProvider')]
    public function testRead(
        $startBuffer,
        $readLength,
        $expectedRead,
        $expectedBuffer
    ) {
        $transport = new TMemoryBuffer($startBuffer);
        $this->assertEquals($expectedRead, $transport->read($readLength));
        $this->assertEquals($expectedBuffer, $transport->getBuffer());
    }

    public static function readDataProvider()
    {
        yield 'Read part of buffer' => [
            'startBuffer' => '1234567890',
            'readLength' => 5,
            'expectedRead' => '12345',
            'expectedBuffer' => '67890',
        ];
        yield 'Read part of buffer UTF' => [
            'startBuffer' => 'Slovenščina',
            'readLength' => 6,
            'expectedRead' => 'Sloven',
            'expectedBuffer' => 'ščina',
        ];
        yield 'Read part of buffer UTF 2' => [
            'startBuffer' => 'Українська',
            'readLength' => 6,
            'expectedRead' => 'Укр',
            'expectedBuffer' => 'аїнська',
        ];
        yield 'Read full' => [
            'startBuffer' => '123456789',
            'readLength' => 10,
            'expectedRead' => '123456789',
            'expectedBuffer' => '',
        ];
    }

    #[DataProvider('writeDataProvider')]
    public function testWrite(
        $startBuffer,
        $writeData,
        $expectedBuffer
    ) {
        $transport = new TMemoryBuffer($startBuffer);
        $transport->write($writeData);
        $this->assertEquals($expectedBuffer, $transport->getBuffer());
    }

    public static function writeDataProvider()
    {
        yield 'empty start buffer' => [
            'startBuffer' => '',
            'writeData' => '12345',
            'expectedBuffer' => '12345',
        ];
        yield 'not empty start buffer' => [
            'startBuffer' => '67890',
            'writeData' => '12345',
            'expectedBuffer' => '6789012345',
        ];
        yield 'not empty start buffer UTF' => [
            'startBuffer' => 'Slovenščina',
            'writeData' => 'Українська',
            'expectedBuffer' => 'SlovenščinaУкраїнська',
        ];
    }

    public function testAvailable()
    {
        $transport = new TMemoryBuffer('12345');
        $this->assertEquals('5', $transport->available());
    }

    public function testPutBack()
    {
        $transport = new TMemoryBuffer('12345');
        $transport->putBack('67890');
        $this->assertEquals('6789012345', $transport->getBuffer());
    }

    public function testWritesAndReadsInTurn()
    {
        $transport = new TMemoryBuffer('abc');
        $this->assertSame('a', $transport->read(1));
        $transport->write('def');
        $this->assertSame(5, $transport->available());
        $this->assertSame('bcdef', $transport->getBuffer());
        $this->assertSame('bc', $transport->read(2));
        $transport->putBack('XY');
        $this->assertSame(5, $transport->available());
        $this->assertSame('XYdef', $transport->getBuffer());
        $this->assertSame('XYdef', $transport->read(10));
        $this->assertSame(0, $transport->available());
        $this->assertSame('', $transport->getBuffer());
        $transport->write('g');
        $this->assertSame('g', $transport->readAll(1));

        $this->expectException(TTransportException::class);
        $this->expectExceptionMessage('TMemoryBuffer: Could not read 1 bytes from buffer.');
        $transport->read(1);
    }

    /**
     * A long run of writes, reads and put-backs of varying sizes must give the
     * same results as a plain string that is appended to and cut from the front.
     */
    public function testWritesAndReadsInTurnMatchAPlainString()
    {
        $transport = new TMemoryBuffer();
        $model = '';
        for ($step = 1; $step <= 600; $step++) {
            $data = str_repeat(chr(65 + $step % 26), $step % 29 + 1);
            $transport->write($data);
            $model .= $data;

            $len = $step % 23 + 1;
            $this->assertSame(substr($model, 0, $len), $transport->read($len), "read at step $step");
            $model = (string) substr($model, $len);

            if ($step % 7 === 0) {
                $transport->putBack("<$step>");
                $model = "<$step>" . $model;
            }
            $this->assertSame(strlen($model), $transport->available(), "available at step $step");
            $this->assertSame($model, $transport->getBuffer(), "buffer at step $step");
        }
    }
}
