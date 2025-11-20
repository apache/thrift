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

namespace Test\Thrift\Unit\Lib\Server;

use PHPUnit\Framework\MockObject\MockObject;
use PHPUnit\Framework\TestCase;
use Test\Thrift\Unit\Lib\Server\Fixture\TestProcessor;
use Thrift\Factory\TProtocolFactory;
use Thrift\Factory\TTransportFactoryInterface;
use Thrift\Server\TServerTransport;
use Thrift\Server\TSimpleServer;
use Thrift\Transport\TTransport;

class TSimpleServerTest extends TestCase
{
    /**
     * @var object
     */
    private $processor;
    /**
     * @var MockObject|TServerTransport
     */
    private $transport;
    /**
     * @var MockObject|TTransportFactoryInterface
     */
    private $inputTransportFactory;
    /**
     * @var MockObject|TTransportFactoryInterface
     */
    private $outputTransportFactory;
    /**
     * @var MockObject|TProtocolFactory
     */
    private $inputProtocolFactory;
    /**
     * @var MockObject|TProtocolFactory
     */
    private $outputProtocolFactory;
    /**
     * @var TSimpleServer
     */
    private $server;

    protected function setUp(): void
    {
        $this->processor = $this->createMock(TestProcessor::class);
        $this->transport = $this->createMock(TServerTransport::class);
        $this->inputTransportFactory = $this->createMock(TTransportFactoryInterface::class);
        $this->outputTransportFactory = $this->createMock(TTransportFactoryInterface::class);
        $this->inputProtocolFactory = $this->createMock(TProtocolFactory::class);
        $this->outputProtocolFactory = $this->createMock(TProtocolFactory::class);

        $this->server = new TSimpleServer(
            $this->processor,
            $this->transport,
            $this->inputTransportFactory,
            $this->outputTransportFactory,
            $this->inputProtocolFactory,
            $this->outputProtocolFactory
        );
    }

    protected function tearDown(): void
    {
        unset(
            $this->processor,
            $this->transport,
            $this->inputTransportFactory,
            $this->outputTransportFactory,
            $this->inputProtocolFactory,
            $this->outputProtocolFactory,
            $this->server
        );
    }

    /**
     * @dataProvider serveDataProvider
     */
    public function testServe(
        $serveLoopCount,
        array $processLoopResult
    ): void {
        $transport = $this->createMock(TTransport::class);

        $this->transport->expects($this->once())
            ->method('listen');
        $this->transport->expects($this->exactly($serveLoopCount))
            ->method('accept')
            ->willReturn($transport);

        $this->inputTransportFactory->expects($this->exactly($serveLoopCount))
            ->method('getTransport')
            ->willReturn($this->createMock(TServerTransport::class));
        $this->outputTransportFactory->expects($this->exactly($serveLoopCount))
            ->method('getTransport')
            ->willReturn($this->createMock(TServerTransport::class));

        $inputProtocol = $this->createMock(TServerTransport::class);
        $this->inputProtocolFactory->expects($this->exactly($serveLoopCount))
            ->method('getProtocol')
            ->willReturn($inputProtocol);

        $outputProtocol = $this->createMock(TServerTransport::class);
        $this->outputProtocolFactory->expects($this->exactly($serveLoopCount))
            ->method('getProtocol')
            ->willReturn($outputProtocol);

        /**
         * ATTENTION!
         * it is a hack to stop the server loop in unit test
         * last call of process can return any value, but should stop server for removing infinite loop
         **/
        $processLoopResult[] = $this->returnCallback(function () {
            $this->server->stop();

            return false;
        });

        $this->processor->expects($this->exactly(count($processLoopResult)))
            ->method('process')
            ->with(
                $this->equalTo($inputProtocol),
                $this->equalTo($outputProtocol)
            )
            ->willReturnOnConsecutiveCalls(...$processLoopResult);

        $this->server->serve();
    }

    public function serveDataProvider()
    {
        yield 'one serve loop' => [
            'serveLoopCount' => 1,
            'processLoopResult' => [
                true,
            ]
        ];
        yield 'two serve loop' => [
            'serveLoopCount' => 2,
            'processLoopResult' => [
                true,
                false,
            ]
        ];
    }
}
