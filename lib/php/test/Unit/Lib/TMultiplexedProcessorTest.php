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

namespace Test\Thrift\Unit\Lib;

use PHPUnit\Framework\TestCase;
use Test\Thrift\Unit\Lib\Fixture\ProcessorSpy;
use Thrift\Exception\TException;
use Thrift\Protocol\TProtocol;
use Thrift\StoredMessageProtocol;
use Thrift\TMultiplexedProcessor;
use Thrift\Transport\TTransport;
use Thrift\Type\TMessageType;

class TMultiplexedProcessorTest extends TestCase
{
    public function testProcessDispatchesToRegisteredService(): void
    {
        $transport = $this->createMock(TTransport::class);
        $input = $this->createMock(TProtocol::class);
        $output = $this->createMock(TProtocol::class);
        $processor = $this->createMock(ProcessorSpy::class);

        $input->expects($this->once())
            ->method('readMessageBegin')
            ->willReturnCallback(function (&$name, &$type, &$seqid): int {
                $name = 'Calculator:add';
                $type = TMessageType::CALL;
                $seqid = 42;

                return 0;
            });
        $input->expects($this->once())
            ->method('getTransport')
            ->willReturn($transport);

        $processor->expects($this->once())
            ->method('process')
            ->with(
                $this->isInstanceOf(StoredMessageProtocol::class),
                $this->identicalTo($output)
            )
            ->willReturnCallback(function (StoredMessageProtocol $decoratedInput, TProtocol $decoratedOutput) use ($output) {
                $name = '';
                $type = 0;
                $seqid = 0;
                $decoratedInput->readMessageBegin($name, $type, $seqid);

                TestCase::assertSame('add', $name);
                TestCase::assertSame(TMessageType::CALL, $type);
                TestCase::assertSame(42, $seqid);
                TestCase::assertSame($output, $decoratedOutput);

                return 'processed';
            });

        $multiplexedProcessor = new TMultiplexedProcessor();
        $multiplexedProcessor->registerProcessor('Calculator', $processor);

        $this->assertSame('processed', $multiplexedProcessor->process($input, $output));
    }

    public function testProcessRejectsUnexpectedMessageType(): void
    {
        $input = $this->createMock(TProtocol::class);
        $output = $this->createMock(TProtocol::class);

        $input->expects($this->once())
            ->method('readMessageBegin')
            ->willReturnCallback(function (&$name, &$type, &$seqid): int {
                $name = 'Calculator:add';
                $type = TMessageType::REPLY;
                $seqid = 1;

                return 0;
            });

        $this->expectException(TException::class);
        $this->expectExceptionMessage('This should not have happened!?');

        (new TMultiplexedProcessor())->process($input, $output);
    }

    public function testProcessRequiresServiceSeparator(): void
    {
        $input = $this->createMock(TProtocol::class);
        $output = $this->createMock(TProtocol::class);

        $input->expects($this->once())
            ->method('readMessageBegin')
            ->willReturnCallback(function (&$name, &$type, &$seqid): int {
                $name = 'add';
                $type = TMessageType::CALL;
                $seqid = 1;

                return 0;
            });

        $this->expectException(TException::class);
        $this->expectExceptionMessage('Service name not found in message name: add.');

        (new TMultiplexedProcessor())->process($input, $output);
    }

    public function testProcessRequiresKnownServiceName(): void
    {
        $input = $this->createMock(TProtocol::class);
        $output = $this->createMock(TProtocol::class);

        $input->expects($this->once())
            ->method('readMessageBegin')
            ->willReturnCallback(function (&$name, &$type, &$seqid): int {
                $name = 'Missing:add';
                $type = TMessageType::CALL;
                $seqid = 1;

                return 0;
            });

        $multiplexedProcessor = new TMultiplexedProcessor();
        $multiplexedProcessor->registerProcessor('Other', $this->createMock(ProcessorSpy::class));

        $this->expectException(TException::class);
        $this->expectExceptionMessage('Service name not found: Missing.');

        $multiplexedProcessor->process($input, $output);
    }
}
