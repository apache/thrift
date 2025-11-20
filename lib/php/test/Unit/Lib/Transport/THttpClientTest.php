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
use Thrift\Exception\TTransportException;
use Thrift\Transport\THttpClient;

class THttpClientTest extends TestCase
{
    use PHPMock;

    public function testSetTimeoutSecs()
    {
        $host = 'localhost';
        $transport = new THttpClient($host);
        $transport->setTimeoutSecs(1000);

        $ref = new \ReflectionClass($transport);
        $prop = $ref->getProperty('timeout_');
        $prop->setAccessible(true);
        $this->assertEquals(1000, $prop->getValue($transport));
    }

    public function testIsOpen()
    {
        $host = 'localhost';
        $transport = new THttpClient($host);
        $this->assertTrue($transport->isOpen());
    }

    public function testOpen()
    {
        $host = 'localhost';
        $transport = new THttpClient($host);
        $this->assertNull($transport->open());
    }

    public function testClose()
    {
        $handle = fopen('php://temp', 'r+');
        $this->getFunctionMock('Thrift\\Transport', 'fclose')
             ->expects($this->once())
             ->with($handle)
             ->willReturn(true);

        $host = 'localhost';
        $transport = new THttpClient($host);

        $ref = new \ReflectionClass($transport);
        $propRequest = $ref->getProperty('handle_');
        $propRequest->setAccessible(true);
        $propRequest->setValue($transport, $handle);

        $this->assertNull($transport->close());
        $this->assertNull($propRequest->getValue($transport));
    }

    /**
     * @dataProvider readDataProvider
     */
    public function testRead(
        $readLen,
        $freadResult,
        $streamGetMetaDataResult,
        $expectedResult,
        $expectedException,
        $expectedExceptionMessage,
        $expectedExceptionCode
    ) {
        $handle = fopen('php://temp', 'r+');
        $this->getFunctionMock('Thrift\\Transport', 'fread')
             ->expects($this->once())
             ->with($handle, $readLen)
             ->willReturn($freadResult);

        $this->getFunctionMock('Thrift\\Transport', 'stream_get_meta_data')
             ->expects(!empty($streamGetMetaDataResult) ? $this->once() : $this->never())
             ->with($handle)
             ->willReturn($streamGetMetaDataResult);

        if ($expectedException) {
            $this->expectException($expectedException);
            $this->expectExceptionMessage($expectedExceptionMessage);
            $this->expectExceptionCode($expectedExceptionCode);
        }

        $host = 'localhost';
        $transport = new THttpClient($host);

        $ref = new \ReflectionClass($transport);
        $propRequest = $ref->getProperty('handle_');
        $propRequest->setAccessible(true);
        $propRequest->setValue($transport, $handle);

        $this->assertEquals($expectedResult, $transport->read($readLen));
    }

    public function readDataProvider()
    {
        yield 'read success' => [
            'readLen' => 10,
            'freadResult' => '1234567890',
            'streamGetMetaDataResult' => [],
            'expectedResult' => '1234567890',
            'expectedException' => null,
            'expectedExceptionMessage' => null,
            'expectedExceptionCode' => null,
        ];
        yield 'read failed' => [
            'readLen' => 10,
            'freadResult' => false,
            'streamGetMetaDataResult' => [
                'timed_out' => false,
            ],
            'expectedResult' => '',
            'expectedException' => TTransportException::class,
            'expectedExceptionMessage' => 'THttpClient: Could not read 10 bytes from localhost:80',
            'expectedExceptionCode' => TTransportException::UNKNOWN,
        ];
        yield 'read timeout' => [
            'readLen' => 10,
            'freadResult' => '',
            'streamGetMetaDataResult' => [
                'timed_out' => true,
            ],
            'expectedResult' => '',
            'expectedException' => TTransportException::class,
            'expectedExceptionMessage' => 'THttpClient: timed out reading 10 bytes from localhost:80',
            'expectedExceptionCode' => TTransportException::TIMED_OUT,
        ];
    }

    public function testWrite()
    {
        $host = 'localhost';
        $transport = new THttpClient($host);

        $ref = new \ReflectionClass($transport);
        $prop = $ref->getProperty('buf_');
        $prop->setAccessible(true);

        $transport->write('1234567890');

        $this->assertEquals('1234567890', $prop->getValue($transport));
    }

    /**
     * @dataProvider flushDataProvider
     */
    public function testFlush(
        $host,
        $port,
        $uri,
        $scheme,
        $context,
        $headers,
        $timeout,
        $streamContextOptions,
        $streamContext,
        $fopenResult,
        $expectedHost,
        $expectedUri,
        $expectedException,
        $expectedExceptionMessage,
        $expectedExceptionCode
    ) {
        $this->getFunctionMock('Thrift\\Transport', 'stream_context_create')
             ->expects($this->once())
             ->with($streamContextOptions)
             ->willReturn($streamContext);

        $this->getFunctionMock('Thrift\\Transport', 'fopen')
             ->expects($this->once())
             ->with(
                 $scheme . '://' . $expectedHost . $expectedUri,
                 'r',
                 false,
                 $streamContext
             )->willReturn($fopenResult);

        if ($expectedException) {
            $this->expectException($expectedException);
            $this->expectExceptionMessage($expectedExceptionMessage);
            $this->expectExceptionCode($expectedExceptionCode);
        }

        $transport = new THttpClient($host, $port, $uri, $scheme, $context);
        if (!empty($headers)) {
            $transport->addHeaders($headers);
        }
        if (!empty($timeout)) {
            $transport->setTimeoutSecs($timeout);
        }

        $this->assertNull($transport->flush());
    }

    public function flushDataProvider()
    {
        $default = [
            'host' => 'localhost',
            'port' => '80',
            'uri' => '',
            'scheme' => 'http',
            'context' => [],
            'headers' => [],
            'timeout' => null,
            'streamContextOptions' => [
                'http' => [
                    'method' => 'POST',
                    'header' => "Host: localhost\r\n" .
                        "Accept: application/x-thrift\r\n" .
                        "User-Agent: PHP/THttpClient\r\n" .
                        "Content-Type: application/x-thrift\r\n" .
                        "Content-Length: 0",
                    'content' => '',
                    'max_redirects' => 1,
                ],
            ],
            'streamContext' => fopen('php://temp', 'r+'),
            'fopenResult' => fopen('php://memory', 'r+'),
            'expectedHost' => 'localhost',
            'expectedUri' => '',
            'expectedException' => '',
            'expectedExceptionMessage' => '',
            'expectedExceptionCode' => '',
        ];

        yield 'success' => $default;
        yield 'additionalHeaders' => array_merge(
            $default,
            [
                'headers' => [
                    'X-Test-Header' => 'test',
                ],
                'streamContextOptions' => [
                    'http' => [
                        'method' => 'POST',
                        'header' => "Host: localhost\r\n" .
                            "Accept: application/x-thrift\r\n" .
                            "User-Agent: PHP/THttpClient\r\n" .
                            "Content-Type: application/x-thrift\r\n" .
                            "Content-Length: 0\r\n" .
                            "X-Test-Header: test",
                        'content' => '',
                        'max_redirects' => 1,
                    ],
                ],
            ]
        );
        yield 'timeout' => array_merge(
            $default,
            [
                'timeout' => 1000,
                'streamContextOptions' => [
                    'http' => [
                        'method' => 'POST',
                        'header' => "Host: localhost\r\n" .
                            "Accept: application/x-thrift\r\n" .
                            "User-Agent: PHP/THttpClient\r\n" .
                            "Content-Type: application/x-thrift\r\n" .
                            "Content-Length: 0",
                        'content' => '',
                        'max_redirects' => 1,
                        'timeout' => 1000,
                    ],
                ],
            ]
        );
        yield 'fopenFailed' => array_merge(
            $default,
            [
                'host' => 'localhost',
                'port' => 8080,
                'uri' => 'test',
                'expectedHost' => 'localhost:8080',
                'expectedUri' => '/test',
                'streamContextOptions' => [
                    'http' => [
                        'method' => 'POST',
                        'header' => "Host: localhost:8080\r\n" .
                            "Accept: application/x-thrift\r\n" .
                            "User-Agent: PHP/THttpClient\r\n" .
                            "Content-Type: application/x-thrift\r\n" .
                            "Content-Length: 0",
                        'content' => '',
                        'max_redirects' => 1,
                    ],
                ],
                'fopenResult' => false,
                'expectedException' => TTransportException::class,
                'expectedExceptionMessage' => 'THttpClient: Could not connect to localhost:8080/test',
                'expectedExceptionCode' => TTransportException::NOT_OPEN,
            ]
        );
    }

    public function testAddHeaders()
    {
        $host = 'localhost';
        $transport = new THttpClient($host);

        $ref = new \ReflectionClass($transport);
        $propRequest = $ref->getProperty('headers_');
        $propRequest->setAccessible(true);
        $propRequest->setValue($transport, ['test' => '1234567890']);

        $transport->addHeaders(['test2' => '12345']);
        $this->assertEquals(['test' => '1234567890', 'test2' => '12345'], $propRequest->getValue($transport));
    }
}
