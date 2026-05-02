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

use phpmock\phpunit\PHPMock;
use PHPUnit\Framework\TestCase;
use Test\Thrift\Unit\Lib\ReflectionHelper;
use Thrift\Exception\TException;
use Thrift\Exception\TTransportException;
use Thrift\Factory\TProtocolFactory;
use Thrift\Factory\TTransportFactoryInterface;
use Thrift\Server\TForkingServer;
use Thrift\Server\TServerTransport;
use Thrift\Transport\TTransport;

class TForkingServerTest extends TestCase
{
    use PHPMock;
    use ReflectionHelper;

    private function createServer(
        $processor = null,
        $transport = null,
        $inputTransportFactory = null,
        $outputTransportFactory = null,
        $inputProtocolFactory = null,
        $outputProtocolFactory = null
    ): TForkingServer {
        return new TForkingServer(
            $processor ?? new \stdClass(),
            $transport ?? $this->createStub(TServerTransport::class),
            $inputTransportFactory ?? $this->createStub(TTransportFactoryInterface::class),
            $outputTransportFactory ?? $this->createStub(TTransportFactoryInterface::class),
            $inputProtocolFactory ?? $this->createStub(TProtocolFactory::class),
            $outputProtocolFactory ?? $this->createStub(TProtocolFactory::class)
        );
    }

    public function testStopClosesTransportAndSetsFlag()
    {
        $transport = $this->createMock(TServerTransport::class);
        $transport->expects($this->once())->method('close');

        $server = $this->createServer(null, $transport);
        $server->stop();

        $this->assertTrue($this->getPropertyValue($server, 'stop_'));
    }

    public function testChildrenArrayInitiallyEmpty()
    {
        $server = $this->createServer();
        $this->assertEmpty($this->getPropertyValue($server, 'children_'));
    }

    public function testConstructorStoresCollaborators()
    {
        $processor = new \stdClass();
        $transport = $this->createStub(TServerTransport::class);
        $inputTransportFactory = $this->createStub(TTransportFactoryInterface::class);
        $outputTransportFactory = $this->createStub(TTransportFactoryInterface::class);
        $inputProtocolFactory = $this->createStub(TProtocolFactory::class);
        $outputProtocolFactory = $this->createStub(TProtocolFactory::class);

        $server = $this->createServer(
            $processor,
            $transport,
            $inputTransportFactory,
            $outputTransportFactory,
            $inputProtocolFactory,
            $outputProtocolFactory
        );

        $this->assertSame($processor, $this->getPropertyValue($server, 'processor_'));
        $this->assertSame($transport, $this->getPropertyValue($server, 'transport_'));
        $this->assertSame($inputTransportFactory, $this->getPropertyValue($server, 'inputTransportFactory_'));
        $this->assertSame($outputTransportFactory, $this->getPropertyValue($server, 'outputTransportFactory_'));
        $this->assertSame($inputProtocolFactory, $this->getPropertyValue($server, 'inputProtocolFactory_'));
        $this->assertSame($outputProtocolFactory, $this->getPropertyValue($server, 'outputProtocolFactory_'));
    }

    public function testServeListensAndLoopsUntilStopped()
    {
        $serverTransport = $this->createMock(TServerTransport::class);
        $serverTransport->expects($this->once())->method('listen');

        $server = $this->createServer(null, $serverTransport);

        // accept returns null (no connection), then on second call we stop
        $callCount = 0;
        $serverTransport->method('accept')->willReturnCallback(
            function () use ($server, &$callCount) {
                $callCount++;
                if ($callCount >= 2) {
                    $this->setPropertyValue($server, 'stop_', true);
                }
                return null;
            }
        );

        $this->getFunctionMock('Thrift\Server', 'pcntl_waitpid')
             ->expects($this->any())
             ->willReturn(0);

        $server->serve();

        $this->assertGreaterThanOrEqual(2, $callCount);
    }

    public function testServeForksAndHandlesParent()
    {
        $serverTransport = $this->createMock(TServerTransport::class);
        $serverTransport->expects($this->once())->method('listen');

        $clientTransport = $this->createStub(TTransport::class);
        $server = $this->createServer(null, $serverTransport);

        $callCount = 0;
        $serverTransport->method('accept')->willReturnCallback(
            function () use ($server, $clientTransport, &$callCount) {
                $callCount++;
                if ($callCount === 1) {
                    return $clientTransport;
                }
                $this->setPropertyValue($server, 'stop_', true);
                return null;
            }
        );

        $this->getFunctionMock('Thrift\Server', 'pcntl_fork')
             ->expects($this->once())
             ->willReturn(12345);

        $this->getFunctionMock('Thrift\Server', 'pcntl_waitpid')
             ->expects($this->any())
             ->willReturn(0);

        $server->serve();

        $children = $this->getPropertyValue($server, 'children_');
        $this->assertArrayHasKey(12345, $children);
        $this->assertSame($clientTransport, $children[12345]);
    }

    public function testServeThrowsTExceptionOnForkFailure()
    {
        $this->expectException(TException::class);
        $this->expectExceptionMessage('Failed to fork');

        $serverTransport = $this->createMock(TServerTransport::class);
        $clientTransport = $this->createStub(TTransport::class);

        $server = $this->createServer(null, $serverTransport);

        $serverTransport->method('accept')->willReturn($clientTransport);

        $this->getFunctionMock('Thrift\Server', 'pcntl_fork')
             ->expects($this->once())
             ->willReturn(-1);

        $this->getFunctionMock('Thrift\Server', 'pcntl_waitpid')
             ->expects($this->any())
             ->willReturn(0);

        $server->serve();
    }

    public function testServeIgnoresTTransportException()
    {
        $serverTransport = $this->createMock(TServerTransport::class);
        $serverTransport->expects($this->once())->method('listen');

        $server = $this->createServer(null, $serverTransport);

        $callCount = 0;
        $serverTransport->method('accept')->willReturnCallback(
            function () use ($server, &$callCount) {
                $callCount++;
                if ($callCount === 1) {
                    throw new TTransportException('Connection reset');
                }
                $this->setPropertyValue($server, 'stop_', true);
                return null;
            }
        );

        $this->getFunctionMock('Thrift\Server', 'pcntl_waitpid')
             ->expects($this->any())
             ->willReturn(0);

        $server->serve();

        $this->assertGreaterThanOrEqual(2, $callCount);
    }

    public function testCollectChildrenRemovesFinishedAndClosesTransport()
    {
        $server = $this->createServer();

        $transport1 = $this->createMock(TTransport::class);
        $transport1->expects($this->once())->method('close');

        $transport2 = $this->createMock(TTransport::class);
        $transport2->expects($this->never())->method('close');

        $this->setPropertyValue($server, 'children_', [
            111 => $transport1,
            222 => $transport2,
        ]);

        $this->getFunctionMock('Thrift\Server', 'pcntl_waitpid')
             ->expects($this->exactly(2))
             ->willReturnCallback(function ($pid) {
                 return ($pid === 111) ? 111 : 0;
             });

        $method = $this->getAccessibleMethod($server, 'collectChildren');
        $method->invoke($server);

        $children = $this->getPropertyValue($server, 'children_');
        $this->assertArrayNotHasKey(111, $children);
        $this->assertArrayHasKey(222, $children);
    }

    public function testCollectChildrenHandlesNullTransport()
    {
        $server = $this->createServer();

        $this->setPropertyValue($server, 'children_', [
            333 => null,
        ]);

        $this->getFunctionMock('Thrift\Server', 'pcntl_waitpid')
             ->expects($this->once())
             ->willReturn(333);

        $method = $this->getAccessibleMethod($server, 'collectChildren');
        $method->invoke($server);

        $children = $this->getPropertyValue($server, 'children_');
        $this->assertEmpty($children);
    }

    public function testHandleParentStoresChildPid()
    {
        $server = $this->createServer();
        $transport = $this->createStub(TTransport::class);

        $method = $this->getAccessibleMethod($server, 'handleParent');
        $method->invoke($server, $transport, 42);

        $children = $this->getPropertyValue($server, 'children_');
        $this->assertArrayHasKey(42, $children);
        $this->assertSame($transport, $children[42]);
    }
}
