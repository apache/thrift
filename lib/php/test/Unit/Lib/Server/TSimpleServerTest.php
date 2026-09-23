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

namespace Test\Thrift\Unit\Lib\Server;

use PHPUnit\Framework\MockObject\MockObject;
use PHPUnit\Framework\TestCase;
use PHPUnit\Framework\Attributes\DataProvider;
use Test\Thrift\Unit\Lib\Server\Fixture\ConnectionStub;
use Test\Thrift\Unit\Lib\Server\Fixture\CountingProcessor;
use Test\Thrift\Unit\Lib\Server\Fixture\QueuedServerTransport;
use Test\Thrift\Unit\Lib\Server\Fixture\TestProcessor;
use Thrift\Factory\TBinaryProtocolFactory;
use Thrift\Factory\TJSONProtocolFactory;
use Thrift\Factory\TProtocolFactory;
use Thrift\Factory\TTransportFactory;
use Thrift\Factory\TTransportFactoryInterface;
use Thrift\Protocol\TProtocol;
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

    #[DataProvider('serveDataProvider')]
    public function testServe(
        $serveLoopCount,
        array $processLoopResult
    ): void {
        $transport = $this->createStub(TTransport::class);

        $this->transport->expects($this->once())
            ->method('listen');
        $this->transport->expects($this->exactly($serveLoopCount))
            ->method('accept')
            ->willReturn($transport);

        $this->inputTransportFactory->expects($this->exactly($serveLoopCount))
            ->method('getTransport')
            ->willReturn($this->createStub(TTransport::class));
        $this->outputTransportFactory->expects($this->exactly($serveLoopCount))
            ->method('getTransport')
            ->willReturn($this->createStub(TTransport::class));

        $inputProtocol = $this->createStub(TProtocol::class);
        $this->inputProtocolFactory->expects($this->exactly($serveLoopCount))
            ->method('getProtocol')
            ->willReturn($inputProtocol);

        $outputProtocol = $this->createStub(TProtocol::class);
        $this->outputProtocolFactory->expects($this->exactly($serveLoopCount))
            ->method('getProtocol')
            ->willReturn($outputProtocol);

        /**
         * ATTENTION!
         * it is a hack to stop the server loop in unit test
         * last call of process can return any value, but should stop server for removing infinite loop
         **/
        $totalCalls = count($processLoopResult) + 1;
        $this->processor->expects($this->exactly($totalCalls))
            ->method('process')
            ->with(
                $this->equalTo($inputProtocol),
                $this->equalTo($outputProtocol)
            )
            ->willReturnCallback(function () use ($processLoopResult) {
                static $iteration = 0;
                if ($iteration < count($processLoopResult)) {
                    return $processLoopResult[$iteration++];
                }
                $this->server->stop();

                return false;
            });

        $this->server->serve();
    }

    public static function serveDataProvider()
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

    /**
     * A connection whose request cannot be read ends, and the next client is
     * still served. The processor reads like generated code, so the failure
     * comes out of process() just as it would in a real server.
     */
    #[DataProvider('unreadableRequestDataProvider')]
    public function testServeContinuesAfterAConnectionFails(string $protocol, string $request): void
    {
        $protocolFactory = $protocol === 'json' ? new TJSONProtocolFactory() : new TBinaryProtocolFactory();
        $failed = new ConnectionStub($request);
        $valid = new ConnectionStub(CountingProcessor::call($protocolFactory));
        $processor = new CountingProcessor();
        $serverTransport = new QueuedServerTransport([$failed, $valid]);
        $server = new TSimpleServer(
            $processor,
            $serverTransport,
            new TTransportFactory(),
            new TTransportFactory(),
            $protocolFactory,
            $protocolFactory
        );
        $serverTransport->server = $server;

        $escaped = null;
        try {
            $server->serve();
        } catch (\Throwable $e) {
            $escaped = $e;
        }

        $this->assertSame(2, $serverTransport->accepted, 'connections accepted');
        $this->assertSame(1, $processor->served, 'calls served');
        $this->assertNotSame('', $valid->written, 'reply written to the second client');
        $this->assertSame(1, $failed->closed, 'close() calls on the failed connection');
        $this->assertSame(1, $valid->closed, 'close() calls on the served connection');
        $this->assertNull($escaped, 'serve() ended early');
    }

    public static function unreadableRequestDataProvider()
    {
        // TBinaryProtocol refuses the first word with a TProtocolException.
        yield 'binary, bad version' => [
            'protocol' => 'binary',
            'request' => "\xff\xff\xff\xff",
        ];
        // The field type name does not decode, which TJSONProtocol reports
        // with a TypeError rather than a TException.
        yield 'json, undecodable field type' => [
            'protocol' => 'json',
            'request' => '[1,"ping",1,1,{"1":{"\z"',
        ];
    }

    public function testServeContinuesWhenClosingAConnectionFails(): void
    {
        // Closing a TSocket twice throws, so a connection the processor has
        // already closed must not end serve() when the server closes it.
        $connection = new class ('') extends ConnectionStub {
            public function close(): void
            {
                parent::close();
                if ($this->closed > 1) {
                    throw new \TypeError('closed twice');
                }
            }
        };
        $serverTransport = new QueuedServerTransport([$connection]);
        $server = new TSimpleServer(
            new class {
                public function process(TProtocol $input, TProtocol $output): bool
                {
                    $input->getTransport()->close();

                    return false;
                }
            },
            $serverTransport,
            new TTransportFactory(),
            new TTransportFactory(),
            new TBinaryProtocolFactory(),
            new TBinaryProtocolFactory()
        );
        $serverTransport->server = $server;

        $server->serve();

        $this->assertSame(1, $serverTransport->accepted);
        $this->assertSame(2, $connection->closed);
    }
}
