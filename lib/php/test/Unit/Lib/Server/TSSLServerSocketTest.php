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
use Thrift\Exception\TTransportException;
use Thrift\Server\TSSLServerSocket;
use Thrift\Transport\TSocket;

class TSSLServerSocketTest extends TestCase
{
    use PHPMock;

    public function testGetSSLHost()
    {
        $socket = new TSSLServerSocket();

        $this->assertEquals('ssl://localhost', $socket->getSSLHost('localhost'));
        $this->assertEquals('ssl://localhost', $socket->getSSLHost('ssl://localhost'));
        $this->assertEquals('tcp://localhost', $socket->getSSLHost('tcp://localhost'));
    }

    public function testListenAndClose(): void
    {
        $options = [
            'ssl' => [
                'verify_peer' => true,
                'verify_peer_name' => true,
                'allow_self_signed' => true,
            ],
        ];
        $context = stream_context_create($options);
        $socket = new TSSLServerSocket('somehost', 999, $context);

        $listener = tmpfile();
        $this->getFunctionMock('Thrift\Server', 'stream_socket_server')
             ->expects($this->once())
             ->with(
                 'ssl://somehost:999', #$address
                 $this->anything(), #&$error_code
                 $this->anything(), #&$error_string
                 STREAM_SERVER_BIND | STREAM_SERVER_LISTEN, #int $flags
                 $this->callback(function ($context) use ($options) {
                     $contextOptions = stream_context_get_options($context);

                     return is_resource($context) && $options === $contextOptions;
                 })#resource $context
             )->willReturn($listener);

        $socket->listen();

        $reflection = new \ReflectionClass($socket);
        $property = $reflection->getProperty('listener_');
        $property->setAccessible(true);

        $this->assertIsResource($property->getValue($socket));

        $this->getFunctionMock('Thrift\Server', 'fclose')
             ->expects($this->once())
             ->with($this->equalTo($listener))
             ->willReturn(true);

        $socket->close();
        $this->assertNull($property->getValue($socket));
    }

    public function testAccept()
    {
        $socket = new TSSLServerSocket('somehost', 999);
        $socket->setAcceptTimeout(1000);

        $listener = tmpfile();
        $this->getFunctionMock('Thrift\Server', 'stream_socket_server')
             ->expects($this->once())
             ->with(
                 'ssl://somehost:999', #$address
                 $this->anything(), #&$error_code
                 $this->anything(), #&$error_string
                 STREAM_SERVER_BIND | STREAM_SERVER_LISTEN, #int $flags
                 $this->callback(function ($context) {
                     $contextOptions = stream_context_get_options($context);

                     return is_resource($context) && $contextOptions === [];
                 }) #resource $context
             )->willReturn($listener);

        $transportHandle = tmpfile();
        $this->getFunctionMock('Thrift\Server', 'stream_socket_accept')
             ->expects($this->once())
             ->with(
                 $this->equalTo($listener),
                 1
             )->willReturn($transportHandle);

        $socket->listen();
        $result = $socket->accept();
        $this->assertInstanceOf(TSocket::class, $result);

        $reflection = new \ReflectionClass($result);
        $property = $reflection->getProperty('handle_');
        $property->setAccessible(true);
        $this->assertEquals($transportHandle, $property->getValue($result));
    }

    public function testAcceptFailed()
    {
        $socket = new TSSLServerSocket('somehost', 999);
        $socket->setAcceptTimeout(1000);

        $listener = tmpfile();

        $this->getFunctionMock('Thrift\Server', 'stream_socket_server')
             ->expects($this->once())
             ->with('ssl://somehost:999')
             ->willReturn($listener);

        $this->getFunctionMock('Thrift\Server', 'stream_socket_accept')
             ->expects($this->once())
             ->with(
                 $this->equalTo($listener),
                 1
             )->willReturn(null);

        $this->expectException(TTransportException::class);
        $this->expectExceptionMessage('accept() may not return NULL');

        $socket->listen();
        $socket->accept();
    }
}
