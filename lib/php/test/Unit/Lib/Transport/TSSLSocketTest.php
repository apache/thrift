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

use phpmock\phpunit\PHPMock;
use PHPUnit\Framework\TestCase;
use PHPUnit\Framework\Attributes\DataProvider;
use Psr\Log\LoggerInterface;
use Psr\Log\LogLevel;
use Test\Thrift\Unit\Lib\UserDeprecationCapture;
use Thrift\Exception\TException;
use Thrift\Exception\TTransportException;
use Thrift\Transport\TSSLSocket;

class TSSLSocketTest extends TestCase
{
    use PHPMock;
    use UserDeprecationCapture;

    #[DataProvider('openExceptionDataProvider')]
    public function testOpenException(
        $host,
        $port,
        $context,
        $debugHandler,
        $streamSocketClientCallCount,
        $expectedException,
        $expectedMessage,
        $expectedCode
    ) {
        $this->expectException($expectedException);
        $this->expectExceptionMessage($expectedMessage);
        $this->expectExceptionCode($expectedCode);

        $this->getFunctionMock('Thrift\Transport', 'stream_socket_client')
             ->expects($this->exactly($streamSocketClientCallCount))
             ->with(
                 'ssl://' . $host . ':' . $port,
                 $this->anything(), #$errno,
                 $this->anything(), #$errstr,
                 $this->anything(), #$this->sendTimeoutSec_ + ($this->sendTimeoutUsec_ / 1000000),
                 STREAM_CLIENT_CONNECT,
                 $this->anything() #$context
             )
             ->willReturn(false);

        $socket = new TSSLSocket(
            $host,
            $port,
            $context,
            $debugHandler
        );
        $socket->open();
    }

    public static function openExceptionDataProvider()
    {
        yield 'host is empty' => [
            'host' => '',
            'port' => 9090,
            'context' => null,
            'debugHandler' => null,
            'streamSocketClientCallCount' => 0,
            'expectedException' => TTransportException::class,
            'expectedMessage' => 'Cannot open null host',
            'expectedCode' => TTransportException::NOT_OPEN,
        ];
        yield 'port is not positive' => [
            'host' => 'localhost',
            'port' => 0,
            'context' => null,
            'debugHandler' => null,
            'streamSocketClientCallCount' => 0,
            'expectedException' => TTransportException::class,
            'expectedMessage' => 'Cannot open without port',
            'expectedCode' => TTransportException::NOT_OPEN,
        ];
        yield 'connection failure' => [
            'host' => 'nonexistent-host',
            'port' => 9090,
            'context' => null,
            'debugHandler' => null,
            'streamSocketClientCallCount' => 1,
            'expectedException' => TException::class,
            'expectedMessage' => 'TSocket: Could not connect to',
            'expectedCode' => TTransportException::UNKNOWN,
        ];
    }

    public function testDoubleConnect(): void
    {
        $host = 'localhost';
        $port = 9090;
        $context = null;
        $debugHandler = null;

        $this->getFunctionMock('Thrift\Transport', 'stream_socket_client')
             ->expects($this->once())
             ->with(
                 'ssl://' . $host . ':' . $port,
                 $this->anything(), #$errno,
                 $this->anything(), #$errstr,
                 $this->anything(), #$this->sendTimeoutSec_ + ($this->sendTimeoutUsec_ / 1000000),
                 STREAM_CLIENT_CONNECT,
                 $this->anything() #$context
             )
             ->willReturn(fopen('php://memory', 'r+'));

        $transport = new TSSLSocket(
            $host,
            $port,
            $context,
            $debugHandler
        );

        $transport->open();
        $this->expectException(TTransportException::class);
        $this->expectExceptionMessage('Socket already connected');
        $this->expectExceptionCode(TTransportException::ALREADY_OPEN);
        $transport->open();
    }

    public function testDebugHandler()
    {
        $host = 'nonexistent-host';
        $port = 9090;
        $context = null;

        $debugHandler = function ($error) {
            $this->assertEquals(
                'TSocket: Could not connect to ssl://nonexistent-host:9090 (Connection refused [999])',
                $error
            );
        };

        $this->getFunctionMock('Thrift\Transport', 'stream_socket_client')
            ->expects($this->once())
            ->with(
                'ssl://' . $host . ':' . $port,
                $this->anything(), #$errno,
                $this->anything(), #$errstr,
                $this->anything(), #$this->sendTimeoutSec_ + ($this->sendTimeoutUsec_ / 1000000),
                STREAM_CLIENT_CONNECT,
                $this->anything() #$context
            )
            ->willReturnCallback(
                function ($host, &$error_code, &$error_message, $timeout, $flags, $context) {
                    $error_code = 999;
                    $error_message = 'Connection refused';

                    return false;
                }
            );

        $this->expectException(\Exception::class);
        $this->expectExceptionMessage('TSocket: Could not connect to');
        $this->expectExceptionCode(0);

        $transport = null;
        $deprecations = self::captureUserDeprecations(
            static function () use (&$transport, $host, $port, $context, $debugHandler): void {
                $transport = new TSSLSocket($host, $port, $context, $debugHandler);
                $transport->setDebug(true);
            },
        );
        $this->assertCount(2, $deprecations);
        $transport->open();
    }

    public function testOpenWithContext()
    {
        $host = 'self-signed-localhost';
        $port = 9090;
        $context = stream_context_create(
            [
                'ssl' => [
                    'verify_peer' => true,
                    'verify_peer_name' => true,
                    'allow_self_signed' => true,
                ],
            ]
        );
        $debugHandler = null;

        $this->getFunctionMock('Thrift\Transport', 'stream_socket_client')
             ->expects($this->once())
             ->with(
                 'ssl://' . $host . ':' . $port,
                 $this->anything(), #$errno,
                 $this->anything(), #$errstr,
                 $this->anything(), #$this->sendTimeoutSec_ + ($this->sendTimeoutUsec_ / 1000000),
                 STREAM_CLIENT_CONNECT,
                 $context #$context
             )
             ->willReturn(fopen('php://memory', 'r+'));

        $transport = new TSSLSocket(
            $host,
            $port,
            $context,
            $debugHandler
        );


        $transport->open();
        $this->assertTrue($transport->isOpen());
    }

    #[DataProvider('hostDataProvider')]
    public function testGetHost($host, $expected)
    {
        $port = 9090;
        $context = null;
        $debugHandler = null;
        $transport = new TSSLSocket(
            $host,
            $port,
            $context,
            $debugHandler
        );
        $this->assertEquals($expected, $transport->getHost());
    }

    public static function hostDataProvider()
    {
        yield 'localhost' => ['localhost', 'ssl://localhost'];
        yield 'ssl_localhost' => ['ssl://localhost', 'ssl://localhost'];
        yield 'http_localhost' => ['http://localhost', 'http://localhost'];
    }

    public function testDebugHandlerWithLoggerInterface(): void
    {
        $host = 'nonexistent-host';
        $port = 9090;
        $expectedMessage = 'TSocket: Could not connect to ssl://nonexistent-host:9090 (Connection refused [999])';

        $logger = $this->createMock(LoggerInterface::class);
        $logger->expects($this->once())
               ->method('log')
               ->with(LogLevel::ERROR, $expectedMessage);

        $this->getFunctionMock('Thrift\Transport', 'stream_socket_client')
            ->expects($this->once())
            ->willReturnCallback(
                function (
                    string $address,
                    &$error_code,
                    &$error_message,
                    ?float $timeout,
                    int $flags,
                    $context
                ) {
                    $error_code = 999;
                    $error_message = 'Connection refused';

                    return false;
                }
            );

        $transport = new TSSLSocket($host, $port, null, $logger);

        $this->expectException(TException::class);
        $transport->open();
    }

    public function testStringDebugHandlerTriggersDeprecation(): void
    {
        $errors = self::captureUserDeprecations(static function (): void {
            new TSSLSocket('localhost', 9090, null, 'error_log');
        });

        $this->assertCount(1, $errors);
        $this->assertSame(E_USER_DEPRECATED, $errors[0]['errno']);
        $this->assertStringContainsString(
            'Passing a callable as $debugHandler is deprecated',
            $errors[0]['errstr'],
        );
    }

    public function testClosureDebugHandlerTriggersDeprecation(): void
    {
        $errors = self::captureUserDeprecations(static function (): void {
            new TSSLSocket('localhost', 9090, null, static function (string $m): void {
            });
        });

        $this->assertCount(1, $errors);
        $this->assertSame(E_USER_DEPRECATED, $errors[0]['errno']);
        $this->assertStringContainsString(
            'Passing a callable as $debugHandler is deprecated',
            $errors[0]['errstr'],
        );
    }
}
