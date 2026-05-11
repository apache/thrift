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
use Thrift\Exception\TTransportException;
use Thrift\Transport\TBufferedTransport;
use Thrift\Transport\TFramedTransport;
use Thrift\Transport\TMemoryBuffer;
use Thrift\Transport\TTransport;

class TransportErrorScenariosTest extends TestCase
{
    // =========================================================================
    // TMemoryBuffer edge cases
    // =========================================================================

    public function testMemoryBufferReadFromEmptyBufferThrowsException()
    {
        $buffer = new TMemoryBuffer('');

        $this->expectException(TTransportException::class);
        $this->expectExceptionCode(TTransportException::UNKNOWN);

        $buffer->read(10);
    }

    public function testMemoryBufferWriteEmptyStringAvailableReturnsZero()
    {
        $buffer = new TMemoryBuffer();
        $buffer->write('');

        $this->assertEquals(0, $buffer->available());
    }

    public function testMemoryBufferReadExactBufferSizeWorks()
    {
        $data = 'hello';
        $buffer = new TMemoryBuffer($data);

        $result = $buffer->read(5);

        $this->assertEquals('hello', $result);
        $this->assertEquals(0, $buffer->available());
    }

    public function testMemoryBufferReadAllWithInsufficientDataThrowsException()
    {
        $buffer = new TMemoryBuffer('abc');

        $this->expectException(TTransportException::class);

        // readAll needs 10 bytes but only 3 are available;
        // after reading 3 bytes the buffer is empty and the next read() throws
        $buffer->readAll(10);
    }

    public function testMemoryBufferGetBufferReturnsWhatWasWritten()
    {
        $buffer = new TMemoryBuffer();
        $buffer->write('first');
        $buffer->write('second');

        $this->assertEquals('firstsecond', $buffer->getBuffer());
    }

    // =========================================================================
    // TBufferedTransport error cases
    // =========================================================================

    public function testBufferedTransportReadAfterCloseThrowsException()
    {
        $transport = $this->createMock(TTransport::class);
        $bufferedTransport = new TBufferedTransport($transport);

        $transport
            ->expects($this->once())
            ->method('close');

        $transport
            ->method('read')
            ->willThrowException(
                new TTransportException('Transport not open', TTransportException::NOT_OPEN)
            );

        $bufferedTransport->close();

        $this->expectException(TTransportException::class);
        $this->expectExceptionCode(TTransportException::NOT_OPEN);

        $bufferedTransport->read(10);
    }

    public function testBufferedTransportWriteOnUnderlyingTransportErrorThrowsException()
    {
        $transport = $this->createStub(TTransport::class);
        // Use a small write buffer so that write triggers a flush to underlying transport
        $bufferedTransport = new TBufferedTransport($transport, 512, 10);

        $transport
            ->method('write')
            ->willThrowException(
                new TTransportException('Write failed', TTransportException::UNKNOWN)
            );

        $this->expectException(TTransportException::class);

        // Write more than wBufSize_ to trigger underlying write
        $bufferedTransport->write('this string is longer than ten bytes');
    }

    public function testBufferedTransportFlushOnUnderlyingTransportErrorThrowsException()
    {
        $transport = $this->createStub(TTransport::class);
        $bufferedTransport = new TBufferedTransport($transport);

        $transport
            ->method('write')
            ->willThrowException(
                new TTransportException('Flush write failed', TTransportException::UNKNOWN)
            );

        // Write some data to the buffer first
        $bufferedTransport->write('data');

        $this->expectException(TTransportException::class);

        $bufferedTransport->flush();
    }

    // =========================================================================
    // TFramedTransport error cases
    // =========================================================================

    public function testFramedTransportReadWithNoFrameAvailableThrowsException()
    {
        $transport = $this->createStub(TTransport::class);
        $framedTransport = new TFramedTransport($transport);

        // readFrame calls transport_->readAll(4) which will throw when no data available
        $transport
            ->method('readAll')
            ->willThrowException(
                new TTransportException(
                    'TMemoryBuffer: Could not read 4 bytes from buffer.',
                    TTransportException::UNKNOWN
                )
            );

        $this->expectException(TTransportException::class);

        $framedTransport->read(10);
    }

    public function testFramedTransportReadFrameOnClosedUnderlyingTransportThrowsException()
    {
        $transport = $this->createMock(TTransport::class);
        $framedTransport = new TFramedTransport($transport);

        $transport
            ->expects($this->once())
            ->method('close');

        $transport
            ->method('readAll')
            ->willThrowException(
                new TTransportException('Transport not open', TTransportException::NOT_OPEN)
            );

        $framedTransport->close();

        $this->expectException(TTransportException::class);
        $this->expectExceptionCode(TTransportException::NOT_OPEN);

        $framedTransport->read(10);
    }
}
