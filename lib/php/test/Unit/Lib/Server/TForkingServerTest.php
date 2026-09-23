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

use phpmock\phpunit\PHPMock;
use PHPUnit\Framework\Attributes\RequiresFunction;
use PHPUnit\Framework\TestCase;
use ReflectionMethod;
use ReflectionProperty;
use Test\Thrift\Unit\Lib\Server\Fixture\ConnectionStub;
use Test\Thrift\Unit\Lib\Server\Fixture\CountingProcessor;
use Test\Thrift\Unit\Lib\Server\Fixture\QueuedServerTransport;
use Thrift\Exception\TException;
use Thrift\Exception\TTransportException;
use Thrift\Factory\TBinaryProtocolFactory;
use Thrift\Factory\TProtocolFactory;
use Thrift\Factory\TTransportFactory;
use Thrift\Factory\TTransportFactoryInterface;
use Thrift\Server\TForkingServer;
use Thrift\Server\TServerTransport;
use Thrift\Transport\TTransport;

class TForkingServerTest extends TestCase
{
    use PHPMock;

    private const NO_CONNECTION = 'no connection';

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

        $this->assertTrue((new ReflectionProperty($server, 'stop'))->getValue($server));
    }

    public function testChildrenArrayInitiallyEmpty()
    {
        $server = $this->createServer();
        $this->assertEmpty((new ReflectionProperty($server, 'children'))->getValue($server));
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

        $this->assertSame($processor, (new ReflectionProperty($server, 'processor'))->getValue($server));
        $this->assertSame($transport, (new ReflectionProperty($server, 'transport'))->getValue($server));
        $this->assertSame(
            $inputTransportFactory,
            (new ReflectionProperty($server, 'inputTransportFactory'))->getValue($server)
        );
        $this->assertSame(
            $outputTransportFactory,
            (new ReflectionProperty($server, 'outputTransportFactory'))->getValue($server)
        );
        $this->assertSame(
            $inputProtocolFactory,
            (new ReflectionProperty($server, 'inputProtocolFactory'))->getValue($server)
        );
        $this->assertSame(
            $outputProtocolFactory,
            (new ReflectionProperty($server, 'outputProtocolFactory'))->getValue($server)
        );
    }

    public function testServeListensAndLoopsUntilStopped()
    {
        $serverTransport = $this->createMock(TServerTransport::class);
        $serverTransport->expects($this->once())->method('listen');

        $server = $this->createServer(null, $serverTransport);

        // accept throws TTransportException (no connection), then on second call we stop
        $callCount = 0;
        $serverTransport->method('accept')->willReturnCallback(
            function () use ($server, &$callCount) {
                $callCount++;
                if ($callCount >= 2) {
                    (new ReflectionProperty($server, 'stop'))->setValue($server, true);
                }
                throw new TTransportException(self::NO_CONNECTION);
            }
        );

        $this->getFunctionMock('Thrift\Server', 'pcntl_waitpid')
             ->expects($this->never());

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
                (new ReflectionProperty($server, 'stop'))->setValue($server, true);
                throw new TTransportException(self::NO_CONNECTION);
            }
        );

        $this->getFunctionMock('Thrift\Server', 'pcntl_fork')
             ->expects($this->once())
             ->willReturn(12345);

        $this->getFunctionMock('Thrift\Server', 'pcntl_waitpid')
             ->expects($this->exactly(2))
             ->willReturn(0);

        $server->serve();

        $children = (new ReflectionProperty($server, 'children'))->getValue($server);
        $this->assertArrayHasKey(12345, $children);
        $this->assertSame($clientTransport, $children[12345]);
    }

    public function testServeThrowsTExceptionOnForkFailure()
    {
        $this->expectException(TException::class);
        $this->expectExceptionMessage('Failed to fork');

        $serverTransport = $this->createStub(TServerTransport::class);
        $clientTransport = $this->createStub(TTransport::class);

        $server = $this->createServer(null, $serverTransport);

        $serverTransport->method('accept')->willReturn($clientTransport);

        $this->getFunctionMock('Thrift\Server', 'pcntl_fork')
             ->expects($this->once())
             ->willReturn(-1);

        $this->getFunctionMock('Thrift\Server', 'pcntl_waitpid')
             ->expects($this->never());

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
                (new ReflectionProperty($server, 'stop'))->setValue($server, true);
                throw new TTransportException(self::NO_CONNECTION);
            }
        );

        $this->getFunctionMock('Thrift\Server', 'pcntl_waitpid')
             ->expects($this->never());

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

        (new ReflectionProperty($server, 'children'))->setValue($server, [
            111 => $transport1,
            222 => $transport2,
        ]);

        $this->getFunctionMock('Thrift\Server', 'pcntl_waitpid')
             ->expects($this->exactly(2))
             ->willReturnCallback(function ($pid) {
                 return ($pid === 111) ? 111 : 0;
             });

        $method = new ReflectionMethod($server, 'collectChildren');
        $method->invoke($server);

        $children = (new ReflectionProperty($server, 'children'))->getValue($server);
        $this->assertArrayNotHasKey(111, $children);
        $this->assertArrayHasKey(222, $children);
    }

    public function testCollectChildrenHandlesNullTransport()
    {
        $server = $this->createServer();

        (new ReflectionProperty($server, 'children'))->setValue($server, [
            333 => null,
        ]);

        $this->getFunctionMock('Thrift\Server', 'pcntl_waitpid')
             ->expects($this->once())
             ->willReturn(333);

        $method = new ReflectionMethod($server, 'collectChildren');
        $method->invoke($server);

        $children = (new ReflectionProperty($server, 'children'))->getValue($server);
        $this->assertEmpty($children);
    }

    public function testHandleParentStoresChildPid()
    {
        $server = $this->createServer();
        $transport = $this->createStub(TTransport::class);

        $method = new ReflectionMethod($server, 'handleParent');
        $method->invoke($server, $transport, 42);

        $children = (new ReflectionProperty($server, 'children'))->getValue($server);
        $this->assertArrayHasKey(42, $children);
        $this->assertSame($transport, $children[42]);
    }

    /**
     * The child forked for a connection ends with that connection, also when
     * the request cannot be read, and never returns into the accept loop.
     * handleChild() ends the process, so the test runs serve() in a real
     * child process and looks at how that process ended.
     */
    #[RequiresFunction('pcntl_fork')]
    public function testChildExitsWhenItsConnectionFails()
    {
        $serverTransport = new QueuedServerTransport([new ConnectionStub("\xff\xff\xff\xff")]);
        $server = $this->createServer(
            new CountingProcessor(),
            $serverTransport,
            new TTransportFactory(),
            new TTransportFactory(),
            new TBinaryProtocolFactory(),
            new TBinaryProtocolFactory()
        );
        $serverTransport->server = $server;

        // Every connection the server accepts takes the child's side of the fork.
        $this->getFunctionMock('Thrift\Server', 'pcntl_fork')
             ->expects($this->any())
             ->willReturn(0);

        $pid = \pcntl_fork();
        if ($pid === 0) {
            // Keep whatever the child prints on its way out to itself.
            ob_start(static function (): string {
                return '';
            });
            try {
                $server->serve();
            } catch (\Throwable $e) {
            }
            // Only reached when the child came back out of serve().
            exit(3);
        }

        $this->assertGreaterThan(0, $pid, 'fork failed');
        \pcntl_waitpid($pid, $status);
        $this->assertTrue(\pcntl_wifexited($status), 'the child did not exit');
        $this->assertSame(0, \pcntl_wexitstatus($status), 'exit status of the child');
    }
}
