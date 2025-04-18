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

use phpmock\phpunit\PHPMock;
use PHPUnit\Framework\TestCase;
use Thrift\Exception\TException;
use Thrift\Exception\TTransportException;
use Thrift\Transport\TSocket;

class TSocketTest extends TestCase
{
    use PHPMock;

    /**
     * @dataProvider openExceptionDataProvider
     */
    public function testOpenException(
        $host,
        $port,
        $persist,
        $debugHandler,
        $fsockopenCallCount,
        $expectedException,
        $expectedMessage,
        $expectedCode
    ) {
        $this->expectException($expectedException);
        $this->expectExceptionMessage($expectedMessage);
        $this->expectExceptionCode($expectedCode);

        $this->getFunctionMock('Thrift\Transport', 'fsockopen')
             ->expects($this->exactly($fsockopenCallCount))
             ->with(
                 $host,
                 $port,
                 $this->anything(), #$errno,
                 $this->anything(), #$errstr,
                 $this->anything() #$this->sendTimeoutSec_ + ($this->sendTimeoutUsec_ / 1000000),
             )
             ->willReturn(false);

        $socket = new TSocket(
            $host,
            $port,
            $persist,
            $debugHandler
        );
        $socket->open();
    }

    public function openExceptionDataProvider()
    {
        yield 'host is empty' => [
            'host' => '',
            'port' => 9090,
            'persist' => null,
            'debugHandler' => false,
            'fsockopenCallCount' => 0,
            'expectedException' => TTransportException::class,
            'expectedMessage' => 'Cannot open null host',
            'expectedCode' => TTransportException::NOT_OPEN,
        ];
        yield 'port is not positive' => [
            'host' => 'localhost',
            'port' => 0,
            'persist' => false,
            'debugHandler' => null,
            'fsockopenCallCount' => 0,
            'expectedException' => TTransportException::class,
            'expectedMessage' => 'Cannot open without port',
            'expectedCode' => TTransportException::NOT_OPEN,
        ];
        yield 'connection failure' => [
            'host' => 'nonexistent-host',
            'port' => 9090,
            'persist' => false,
            'debugHandler' => null,
            'fsockopenCallCount' => 1,
            'expectedException' => TException::class,
            'expectedMessage' => 'TSocket: Could not connect to',
            'expectedCode' => TTransportException::UNKNOWN,
        ];
    }

    public function testDoubleConnect(): void
    {
        $host = 'localhost';
        $port = 9090;
        $persist = false;
        $debugHandler = null;
        $handle = fopen('php://memory', 'r+');
        $this->getFunctionMock('Thrift\Transport', 'fsockopen')
             ->expects($this->once())
             ->with(
                 $host,
                 $port,
                 $this->anything(), #$errno,
                 $this->anything(), #$errstr,
                 $this->anything() #$this->sendTimeoutSec_ + ($this->sendTimeoutUsec_ / 1000000),
             )
             ->willReturn($handle);

        $this->getFunctionMock('Thrift\Transport', 'socket_import_stream')
             ->expects($this->once())
             ->with($handle)
             ->willReturn(true);

        $this->getFunctionMock('Thrift\Transport', 'socket_set_option')
             ->expects($this->once())
             ->with(
                 $this->anything(), #$socket,
                 SOL_TCP, #$level
                 TCP_NODELAY, #$option
                 1 #$value
             )
             ->willReturn(true);

        $transport = new TSocket(
            $host,
            $port,
            $persist,
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
        $false = false;

        $debugHandler = function ($error) {
            $this->assertEquals(
                'TSocket: Could not connect to nonexistent-host:9090 (Connection refused [999])',
                $error
            );
        };

        $this->getFunctionMock('Thrift\Transport', 'fsockopen')
             ->expects($this->once())
             ->with(
                 $host,
                 $port,
                 $this->anything(), #$errno,
                 $this->anything(), #$errstr,
                 $this->anything() #$this->sendTimeoutSec_ + ($this->sendTimeoutUsec_ / 1000000),
             )
             ->willReturnCallback(
                 function (
                     string $hostname,
                     int $port,
                     &$error_code,
                     &$error_message,
                     ?float $timeout
                 ) {
                     $error_code = 999;
                     $error_message = 'Connection refused';

                     return false;
                 }
             );

        $transport = new TSocket(
            $host,
            $port,
            $false,
            $debugHandler
        );
        $transport->setDebug(true);

        $this->expectException(\Exception::class);
        $this->expectExceptionMessage('TSocket: Could not connect to');
        $this->expectExceptionCode(0);
        $transport->open();
    }

    public function testOpenPersist()
    {
        $host = 'persist-localhost';
        $port = 9090;
        $persist = true;
        $debugHandler = null;

        $handle = fopen('php://memory', 'r+');

        $this->getFunctionMock('Thrift\Transport', 'pfsockopen')
             ->expects($this->once())
             ->with(
                 $host,
                 $port,
                 $this->anything(), #$errno,
                 $this->anything(), #$errstr,
                 $this->anything() #$this->sendTimeoutSec_ + ($this->sendTimeoutUsec_ / 1000000),
             )
             ->willReturn($handle);

        $this->getFunctionMock('Thrift\Transport', 'socket_import_stream')
             ->expects($this->once())
             ->with($handle)
             ->willReturn(true);

        $this->getFunctionMock('Thrift\Transport', 'socket_set_option')
             ->expects($this->once())
             ->with(
                 $this->anything(), #$socket,
                 SOL_TCP, #$level
                 TCP_NODELAY, #$option
                 1 #$value
             )
             ->willReturn(true);

        $transport = new TSocket(
            $host,
            $port,
            $persist,
            $debugHandler
        );

        $transport->open();
        $this->assertTrue($transport->isOpen());
    }

    /**
     * @dataProvider open_THRIFT_5132_DataProvider
     */
    public function testOpen_THRIFT_5132(
        $socketImportResult
    ) {
        $host = 'localhost';
        $port = 9090;
        $persist = false;
        $debugHandler = null;

        $this->getFunctionMock('Thrift\Transport', 'fsockopen')
             ->expects($this->once())
             ->with(
                 $host,
                 $port,
                 $this->anything(), #$errno,
                 $this->anything(), #$errstr,
                 $this->anything() #$this->sendTimeoutSec_ + ($this->sendTimeoutUsec_ / 1000000),
             )
             ->willReturn(fopen('php://input', 'r+'));

        $this->getFunctionMock('Thrift\Transport', 'socket_import_stream')
             ->expects($this->once())
             ->willReturn($socketImportResult);

        $this->getFunctionMock('Thrift\Transport', 'socket_set_option')
             ->expects($socketImportResult ? $this->once() : $this->never())
             ->with(
                 $this->anything(), #$socket,
                 SOL_TCP, #$level
                 TCP_NODELAY, #$option
                 1 #$value
             )
             ->willReturn(true);

        $transport = new TSocket(
            $host,
            $port,
            $persist,
            $debugHandler
        );

        $transport->open();
        $this->assertTrue($transport->isOpen());
    }

    public function open_THRIFT_5132_DataProvider()
    {
        yield 'socket_import_stream success' => [
            'socketImportResult' => true,
        ];
        yield 'socket_import_stream fail' => [
            'socketImportResult' => false,
        ];
    }

    public function testSetHandle()
    {
        $host = 'localhost';
        $port = 9090;
        $persist = false;
        $debugHandler = null;
        $transport = new TSocket(
            $host,
            $port,
            $persist,
            $debugHandler
        );

        $this->assertFalse($transport->isOpen());
        $transport->setHandle(fopen('php://memory', 'r+'));
        $this->assertTrue($transport->isOpen());
    }

    public function testSetSendTimeout()
    {
        $host = 'localhost';
        $port = 9090;
        $persist = false;
        $debugHandler = null;
        $transport = new TSocket(
            $host,
            $port,
            $persist,
            $debugHandler
        );

        $transport->setSendTimeout(9999);
        $reflector = new \ReflectionClass($transport);
        $property = $reflector->getProperty('sendTimeoutSec_');
        $property->setAccessible(true);
        $this->assertEquals(9.0, $property->getValue($transport));
        $property = $reflector->getProperty('sendTimeoutUsec_');
        $property->setAccessible(true);
        $this->assertEquals(999000, $property->getValue($transport));
    }

    public function testSetRecvTimeout()
    {
        $host = 'localhost';
        $port = 9090;
        $persist = false;
        $debugHandler = null;
        $transport = new TSocket(
            $host,
            $port,
            $persist,
            $debugHandler
        );

        $transport->setRecvTimeout(9999);
        $reflector = new \ReflectionClass($transport);
        $property = $reflector->getProperty('recvTimeoutSec_');
        $property->setAccessible(true);
        $this->assertEquals(9.0, $property->getValue($transport));
        $property = $reflector->getProperty('recvTimeoutUsec_');
        $property->setAccessible(true);
        $this->assertEquals(999000, $property->getValue($transport));
    }

    /**
     * @dataProvider hostDataProvider
     */
    public function testGetHost($host, $expected)
    {
        $port = 9090;
        $persist = false;
        $debugHandler = null;
        $transport = new TSocket(
            $host,
            $port,
            $persist,
            $debugHandler
        );
        $this->assertEquals($expected, $transport->getHost());
    }

    public function hostDataProvider()
    {
        yield 'localhost' => ['localhost', 'localhost'];
        yield 'ssl_localhost' => ['ssl://localhost', 'ssl://localhost'];
        yield 'http_localhost' => ['http://localhost', 'http://localhost'];
    }

    public function testGetPort()
    {
        $host = 'localhost';
        $port = 9090;
        $persist = false;
        $debugHandler = null;
        $transport = new TSocket(
            $host,
            $port,
            $persist,
            $debugHandler
        );
        $this->assertEquals($port, $transport->getPort());
    }

    public function testClose()
    {
        $host = 'localhost';
        $port = 9090;
        $persist = false;
        $debugHandler = null;
        $transport = new TSocket(
            $host,
            $port,
            $persist,
            $debugHandler
        );
        $transport->setHandle(fopen('php://memory', 'r+'));
        $reflector = new \ReflectionClass($transport);
        $property = $reflector->getProperty('handle_');
        $property->setAccessible(true);
        $this->assertNotNull($property->getValue($transport));

        $transport->close();
        $reflector = new \ReflectionClass($transport);
        $property = $reflector->getProperty('handle_');
        $property->setAccessible(true);
        $this->assertNull($property->getValue($transport));
    }

    /**
     * @dataProvider writeFailDataProvider
     */
    public function testWriteFail(
        $streamSelectResult,
        $fwriteCallCount,
        $expectedException,
        $expectedMessage,
        $expectedCode
    ) {
        $host = 'localhost';
        $port = 9090;
        $persist = false;
        $debugHandler = null;
        $handle = fopen('php://memory', 'r+');

        $this->getFunctionMock('Thrift\Transport', 'stream_select')
             ->expects($this->once())
             ->with(
                 $this->anything(), #$null,
                 [$handle],
                 $this->anything(), #$null,
                 $this->anything(), #$this->sendTimeoutSec_,
                 $this->anything() #$this->sendTimeoutUsec_
             )
             ->willReturn($streamSelectResult);

        $this->getFunctionMock('Thrift\Transport', 'fwrite')
             ->expects($this->exactly($fwriteCallCount))
             ->with(
                 $handle,
                 'test1234456789132456798'
             )
             ->willReturn(false);

        $this->expectException($expectedException);
        $this->expectExceptionMessage($expectedMessage);
        $this->expectExceptionCode($expectedCode);

        $transport = new TSocket(
            $host,
            $port,
            $persist,
            $debugHandler
        );
        $transport->setHandle($handle);

        $transport->write('test1234456789132456798');
    }

    public function testWrite()
    {
        $host = 'localhost';
        $port = 9090;
        $persist = false;
        $debugHandler = null;
        $transport = new TSocket(
            $host,
            $port,
            $persist,
            $debugHandler
        );
        $fileName = sys_get_temp_dir() . '/' . md5(mt_rand(0, time()) . time());
        touch($fileName);
        $handle = fopen($fileName, 'r+');
        $transport->setHandle($handle);
        $transport->write('test1234456789132456798');
        $this->assertEquals('test1234456789132456798', file_get_contents($fileName));

        register_shutdown_function(function () use ($fileName) {
            is_file($fileName) && unlink($fileName);
        });
    }

    public function writeFailDataProvider()
    {
        yield 'stream_select timeout' => [
            'streamSelectResult' => 0,
            'fwriteCallCount' => 0,
            'expectedException' => TTransportException::class,
            'expectedMessage' => 'TSocket: timed out writing 23 bytes from localhost:9090',
            'expectedCode' => 0,
        ];
        yield 'stream_select fail write' => [
            'streamSelectResult' => 1,
            'fwriteCallCount' => 1,
            'expectedException' => TTransportException::class,
            'expectedMessage' => 'TSocket: Could not write 23 bytes localhost:9090',
            'expectedCode' => 0,
        ];
        yield 'stream_select fail' => [
            'streamSelectResult' => false,
            'fwriteCallCount' => 0,
            'expectedException' => TTransportException::class,
            'expectedMessage' => 'TSocket: Could not write 23 bytes localhost:9090',
            'expectedCode' => 0,
        ];
    }

    public function testRead()
    {
        $host = 'localhost';
        $port = 9090;
        $persist = false;
        $debugHandler = null;
        $transport = new TSocket(
            $host,
            $port,
            $persist,
            $debugHandler
        );
        $fileName = sys_get_temp_dir() . '/' . md5(mt_rand(0, time()) . time());
        file_put_contents($fileName, '12345678901234567890');
        $handle = fopen($fileName, 'r+');
        $transport->setHandle($handle);
        $this->assertEquals('12345', $transport->read(5));

        register_shutdown_function(function () use ($fileName) {
            is_file($fileName) && unlink($fileName);
        });
    }

    /**
     * @dataProvider readFailDataProvider
     */
    public function testReadFail(
        $streamSelectResult,
        $freadResult,
        $feofResult,
        $expectedException,
        $expectedMessage,
        $expectedCode
    ) {
        $host = 'localhost';
        $port = 9090;
        $persist = false;
        $debugHandler = null;
        $handle = fopen('php://memory', 'r+');

        $this->getFunctionMock('Thrift\Transport', 'stream_select')
             ->expects($this->once())
             ->with(
                 [$handle],
                 $this->anything(), #$null,
                 $this->anything(), #$null,
                 $this->anything(), #$this->recvTimeoutSec_,
                 $this->anything() #$this->recvTimeoutUsec_
             )
             ->willReturn($streamSelectResult);

        $this->getFunctionMock('Thrift\Transport', 'fread')
             ->expects($this->exactly($streamSelectResult ? 1 : 0))
             ->with(
                 $handle,
                 5
             )
             ->willReturn($freadResult);
        $this->getFunctionMock('Thrift\Transport', 'feof')
             ->expects($this->exactly($feofResult ? 1 : 0))
             ->with($handle)
             ->willReturn($feofResult);

        $this->expectException($expectedException);
        $this->expectExceptionMessage($expectedMessage);
        $this->expectExceptionCode($expectedCode);

        $transport = new TSocket(
            $host,
            $port,
            $persist,
            $debugHandler
        );
        $transport->setHandle($handle);

        $transport->read(5);
    }

    public function readFailDataProvider()
    {
        yield 'stream_select timeout' => [
            'streamSelectResult' => 0,
            'freadResult' => '',
            'feofResult' => false,
            'expectedException' => TTransportException::class,
            'expectedMessage' => 'TSocket: timed out reading 5 bytes from localhost:9090',
            'expectedCode' => 0,
        ];
        yield 'stream_select fail read' => [
            'streamSelectResult' => 1,
            'freadResult' => '',
            'feofResult' => true,
            'expectedException' => TTransportException::class,
            'expectedMessage' => 'TSocket read 0 bytes',
            'expectedCode' => 0,
        ];
        yield 'stream_select fail' => [
            'streamSelectResult' => false,
            'freadResult' => '',
            'feofResult' => false,
            'expectedException' => TTransportException::class,
            'expectedMessage' => 'TSocket: Could not read 5 bytes from localhost:9090',
            'expectedCode' => 0,
        ];
        yield 'fread false' => [
            'streamSelectResult' => 1,
            'freadResult' => false,
            'feofResult' => false,
            'expectedException' => TTransportException::class,
            'expectedMessage' => 'TSocket: Could not read 5 bytes from localhost:9090',
            'expectedCode' => 0,
        ];
        yield 'fread empty' => [
            'streamSelectResult' => 1,
            'freadResult' => '',
            'feofResult' => true,
            'expectedException' => TTransportException::class,
            'expectedMessage' => 'TSocket read 0 bytes',
            'expectedCode' => 0,
        ];
    }

    public function testFlush()
    {
        $host = 'localhost';
        $port = 9090;
        $persist = false;
        $debugHandler = null;
        $transport = new TSocket(
            $host,
            $port,
            $persist,
            $debugHandler
        );
        $this->assertNUll($transport->flush());
    }
}
