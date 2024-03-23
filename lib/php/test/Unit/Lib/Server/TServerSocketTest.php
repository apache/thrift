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
use Thrift\Server\TServerSocket;
use Thrift\Transport\TSocket;

class TServerSocketTest extends TestCase
{
    use PHPMock;

    public function testSetAcceptTimeout(): void
    {
        $socket = new TServerSocket();
        $socket->setAcceptTimeout(1000);

        $reflection = new \ReflectionClass($socket);
        $property = $reflection->getProperty('acceptTimeout_');
        $property->setAccessible(true);

        $this->assertEquals(1000, $property->getValue($socket));
    }

    public function testListenAndClose(): void
    {
        $socket = new TServerSocket('somehost', 999);

        $listener = tmpfile();
        $this->getFunctionMock('Thrift\Server', 'stream_socket_server')
             ->expects($this->once())
             ->with('tcp://somehost:999')
             ->willReturn($listener);

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
        $socket = new TServerSocket('somehost', 999);
        $socket->setAcceptTimeout(1000);

        $listener = tmpfile();

        $this->getFunctionMock('Thrift\Server', 'stream_socket_server')
             ->expects($this->once())
             ->with('tcp://somehost:999')
             ->willReturn($listener);

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
        $socket = new TServerSocket('somehost', 999);
        $socket->setAcceptTimeout(1000);

        $listener = tmpfile();

        $this->getFunctionMock('Thrift\Server', 'stream_socket_server')
             ->expects($this->once())
             ->with('tcp://somehost:999')
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
