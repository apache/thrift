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

namespace Test\Thrift\Unit\Lib\Transport;

use PHPUnit\Framework\TestCase;
use PHPUnit\Framework\Constraint\Constraint;
use PHPUnit\Framework\Attributes\DataProvider;
use Test\Thrift\Unit\Lib\ReflectionHelper;
use Thrift\Transport\TFramedTransport;
use Thrift\Transport\TTransport;

class TFramedTransportTest extends TestCase
{
    use ReflectionHelper;

    public function testIsOpen()
    {
        $transport = $this->createMock(TTransport::class);
        $framedTransport = new TFramedTransport($transport);

        $transport
            ->expects($this->once())
            ->method('isOpen')
            ->willReturn(true);

        $this->assertTrue($framedTransport->isOpen());
    }

    public function testOpen()
    {
        $transport = $this->createMock(TTransport::class);
        $framedTransport = new TFramedTransport($transport);

        $transport
            ->expects($this->once())
            ->method('open')
            ->willReturn(null);

        $this->assertNull($framedTransport->open());
    }

    public function testClose()
    {
        $transport = $this->createMock(TTransport::class);
        $framedTransport = new TFramedTransport($transport);

        $transport
            ->expects($this->once())
            ->method('close')
            ->willReturn(null);

        $this->assertNull($framedTransport->close());
    }

    public function testPutBack()
    {
        $transport = $this->createStub(TTransport::class);
        $framedTransport = new TFramedTransport($transport);
        $framedTransport->putBack('test');

        $this->assertEquals('test', $this->getPropertyValue($framedTransport, 'rBuf'));

        $framedTransport->putBack('abcde');
        $this->assertEquals('abcdetest', $this->getPropertyValue($framedTransport, 'rBuf'));
    }

    #[DataProvider('readDataProvider')]
    public function testRead(
        $readAllowed,
        $readBuffer,
        $lowLevelTransportReadResult,
        $lowLevelTransportReadAllParams,
        $lowLevelTransportReadAllResult,
        $readLength,
        $expectedReadResult
    ) {
        $transport = $this->createMock(TTransport::class);
        $framedTransport = new TFramedTransport($transport, $readAllowed);
        $framedTransport->putBack($readBuffer);

        $transport
            ->expects($readAllowed ? $this->never() : $this->once())
            ->method('read')
            ->with($readLength)
            ->willReturn($lowLevelTransportReadResult);

        $transport
            ->expects($this->exactly(count($lowLevelTransportReadAllParams)))
            ->method('readAll')
            ->willReturnCallback(function (...$args) use ($lowLevelTransportReadAllParams, $lowLevelTransportReadAllResult) {
                static $iteration = 0;
                $expected = $lowLevelTransportReadAllParams[$iteration];
                foreach ($expected as $i => $exp) {
                    if ($exp instanceof Constraint) {
                        $this->assertThat($args[$i], $exp);
                    } else {
                        $this->assertSame($exp, $args[$i]);
                    }
                }

                return $lowLevelTransportReadAllResult[$iteration++];
            });

        $this->assertEquals($expectedReadResult, $framedTransport->read($readLength));
    }

    public static function readDataProvider()
    {
        yield 'read not allowed' => [
            'readAllowed' => false,
            'readBuffer' => '',
            'lowLevelTransportReadResult' => '12345',
            'lowLevelTransportReadAllParams' => [],
            'lowLevelTransportReadAllResult' => [],
            'readLength' => 5,
            'expectedReadResult' => '12345',
        ];
        yield 'read fully buffered item' => [
            'readAllowed' => true,
            'readBuffer' => '',
            'lowLevelTransportReadResult' => '',
            'lowLevelTransportReadAllParams' => [[4], [5]],
            'lowLevelTransportReadAllResult' => [pack('N', '5'), '12345'],
            'readLength' => 5,
            'expectedReadResult' => '12345',
        ];
        yield 'read partly buffered item' => [
            'readAllowed' => true,
            'readBuffer' => '',
            'lowLevelTransportReadResult' => '',
            'lowLevelTransportReadAllParams' => [[4], [10]],
            'lowLevelTransportReadAllResult' => [pack('N', '10'), '1234567890'],
            'readLength' => 5,
            'expectedReadResult' => '12345',
        ];
    }

    #[DataProvider('writeDataProvider')]
    public function testWrite(
        $writeAllowed,
        $writeData,
        $writeLength,
        $expectedWriteBufferValue
    ) {
        $transport = $this->createMock(TTransport::class);
        $framedTransport = new TFramedTransport($transport, true, $writeAllowed);

        $transport
            ->expects($writeAllowed ? $this->never() : $this->once())
            ->method('write')
            ->with('12345', 5)
            ->willReturn(5);

        $framedTransport->write($writeData, $writeLength);

        $this->assertEquals($expectedWriteBufferValue, $this->getPropertyValue($framedTransport, 'wBuf'));
    }

    public static function writeDataProvider()
    {
        yield 'write not allowed' => [
            'writeAllowed' => false,
            'writeData' => '12345',
            'writeLength' => 5,
            'expectedWriteBufferValue' => '',
        ];
        yield 'write full' => [
            'writeAllowed' => true,
            'writeData' => '12345',
            'writeLength' => 5,
            'expectedWriteBufferValue' => '12345',
        ];
        yield 'write partly' => [
            'writeAllowed' => true,
            'writeData' => '1234567890',
            'writeLength' => 5,
            'expectedWriteBufferValue' => '12345',
        ];
    }

    #[DataProvider('flushDataProvider')]
    public function testFlush(
        $writeAllowed,
        $writeBuffer,
        $lowLevelTransportWrite
    ) {
        $transport = $this->createMock(TTransport::class);
        $framedTransport = new TFramedTransport($transport, true, $writeAllowed);
        $this->setPropertyValue($framedTransport, 'wBuf', $writeBuffer);

        $transport
            ->expects($this->once())
            ->method('flush');

        $transport
            ->expects($writeAllowed && !empty($writeBuffer) ? $this->once() : $this->never())
            ->method('write')
            ->with($lowLevelTransportWrite)
            ->willReturn(null);

        $this->assertNull($framedTransport->flush());
    }

    public static function flushDataProvider()
    {
        yield 'write not allowed' => [
            'writeAllowed' => false,
            'writeBuffer' => '12345',
            'lowLevelTransportWrite' => '',
        ];
        yield 'empty buffer' => [
            'writeAllowed' => true,
            'writeBuffer' => '',
            'lowLevelTransportWrite' => '',
        ];
        yield 'write full' => [
            'writeAllowed' => true,
            'writeBuffer' => '12345',
            'lowLevelTransportWrite' => pack('N', strlen('12345')) . '12345',
        ];
    }
}
