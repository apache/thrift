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
use PHPUnit\Framework\Constraint\Constraint;
use PHPUnit\Framework\Assert;
use PHPUnit\Framework\Attributes\DataProvider;
use Test\Thrift\Unit\Lib\ReflectionHelper;
use Thrift\Exception\TTransportException;
use Thrift\Transport\TCurlClient;

class TCurlClientTest extends TestCase
{
    use PHPMock;
    use ReflectionHelper;

    public function testSetTimeoutSecs()
    {
        $host = 'localhost';
        $transport = new TCurlClient($host);
        $transport->setTimeoutSecs(1000);

        $this->assertEquals(1000, $this->getPropertyValue($transport, 'timeout_'));
    }

    public function testSetConnectionTimeoutSecs()
    {
        $host = 'localhost';
        $transport = new TCurlClient($host);
        $transport->setConnectionTimeoutSecs(1000);

        $this->assertEquals(1000, $this->getPropertyValue($transport, 'connectionTimeout_'));
    }

    public function testIsOpen()
    {
        $host = 'localhost';
        $transport = new TCurlClient($host);
        $this->assertTrue($transport->isOpen());
    }

    public function testOpen()
    {
        $host = 'localhost';
        $transport = new TCurlClient($host);
        $this->assertNull($transport->open());
    }

    public function testClose()
    {
        $host = 'localhost';
        $transport = new TCurlClient($host);

        $this->setPropertyValue($transport, 'request_', 'testRequest');
        $this->setPropertyValue($transport, 'response_', 'testResponse');

        $this->assertNull($transport->close());
        $this->assertEmpty($this->getPropertyValue($transport, 'request_'));
        $this->assertEmpty($this->getPropertyValue($transport, 'response_'));
    }

    public function testRead()
    {
        $host = 'localhost';
        $transport = new TCurlClient($host);

        $this->setPropertyValue($transport, 'response_', '1234567890');

        $response = $transport->read(5);
        $this->assertEquals('12345', $response);
        $this->assertEquals('67890', $this->getPropertyValue($transport, 'response_'));

        $response = $transport->read(5);
        $this->assertEquals('67890', $response);
        # The response does not cleaned after reading full answer, maybe it should be fixed
        $this->assertEquals('67890', $this->getPropertyValue($transport, 'response_'));
    }

    public function testReadAll()
    {
        $host = 'localhost';
        $transport = new TCurlClient($host);

        $this->setPropertyValue($transport, 'response_', '1234567890');

        $response = $transport->readAll(5);
        $this->assertEquals('12345', $response);
        $this->assertEquals('67890', $this->getPropertyValue($transport, 'response_'));
    }

    public function testReadAllThrift4656()
    {
        $host = 'localhost';
        $transport = new TCurlClient($host);

        $this->setPropertyValue($transport, 'response_', '');

        $this->expectException(TTransportException::class);
        $this->expectExceptionMessage('TCurlClient could not read 5 bytes');
        $this->expectExceptionCode(TTransportException::UNKNOWN);

        $transport->readAll(5);
    }

    public function testWrite()
    {
        $host = 'localhost';
        $transport = new TCurlClient($host);

        $this->setPropertyValue($transport, 'request_', '1234567890');

        $transport->write('12345');
        $this->assertEquals('123456789012345', $this->getPropertyValue($transport, 'request_'));
    }

    public function testAddHeaders()
    {
        $host = 'localhost';
        $transport = new TCurlClient($host);

        $this->setPropertyValue($transport, 'headers_', ['test' => '1234567890']);

        $transport->addHeaders(['test2' => '12345']);
        $this->assertEquals(['test' => '1234567890', 'test2' => '12345'], $this->getPropertyValue($transport, 'headers_'));
    }

    #[DataProvider('flushDataProvider')]
    public function testFlush(
        $host,
        $port,
        $uri,
        $scheme,
        $headers,
        $request,
        $timeout,
        $connectionTimeout,
        $curlSetOptCalls,
        $response,
        $responseError,
        $responseCode,
        $expectedException = null,
        $expectedMessage = null,
        $expectedCode = null
    ) {
        $this->getFunctionMock('Thrift\\Transport', 'register_shutdown_function')
            ->expects($this->once())
            ->with(
                $this->callback(
                    function ($arg) {
                        return is_array($arg)
                            && $arg[0] === 'Thrift\\Transport\\TCurlClient'
                            && $arg[1] === 'closeCurlHandle';
                    }
                )
            );
        $this->getFunctionMock('Thrift\\Transport', 'curl_init')
             ->expects($this->once());

        $this->getFunctionMock('Thrift\\Transport', 'curl_setopt')
             ->expects($this->any())
             ->willReturnCallback(function (...$args) use ($curlSetOptCalls) {
                 static $iteration = 0;
                 $expected = $curlSetOptCalls[$iteration++];
                foreach ($expected as $i => $exp) {
                    if ($exp instanceof Constraint) {
                        $this->assertThat($args[$i], $exp);
                    } else {
                        $this->assertSame($exp, $args[$i]);
                    }
                }

                 return true;
             });

        $this->getFunctionMock('Thrift\\Transport', 'curl_exec')
             ->expects($this->once())
             ->with(Assert::anything())
             ->willReturn($response);

        $this->getFunctionMock('Thrift\\Transport', 'curl_error')
             ->expects($this->once())
             ->with(Assert::anything())
             ->willReturn($responseError);

        $this->getFunctionMock('Thrift\\Transport', 'curl_getinfo')
             ->expects($this->once())
             ->with(Assert::anything(), CURLINFO_HTTP_CODE)
             ->willReturn($responseCode);

        if (!is_null($expectedException)) {
            $this->expectException($expectedException);
            $this->expectExceptionMessage($expectedMessage);
            $this->expectExceptionCode($expectedCode);

            $this->getFunctionMock('Thrift\\Transport', 'curl_close')
                 ->expects($this->once())
                 ->with(Assert::anything());
        }

        $transport = new TCurlClient($host, $port, $uri, $scheme);
        if (!empty($headers)) {
            $transport->addHeaders($headers);
        }
        $transport->write($request);
        if (!empty($timeout)) {
            $transport->setTimeoutSecs($timeout);
        }
        if (!empty($connectionTimeout)) {
            $transport->setConnectionTimeoutSecs($connectionTimeout);
        }

        $transport->flush();
    }

    public static function flushDataProvider()
    {
        $request = 'request';

        $default = [
            'host' => 'localhost',
            'port' => 80,
            'uri' => '',
            'scheme' => 'http',
            'headers' => [],
            'request' => $request,
            'timeout' => null,
            'connectionTimeout' => null,
            'curlSetOptCalls' => [
                [Assert::anything(), CURLOPT_RETURNTRANSFER, true],
                [Assert::anything(), CURLOPT_USERAGENT, 'PHP/TCurlClient'],
                [Assert::anything(), CURLOPT_CUSTOMREQUEST, 'POST'],
                [Assert::anything(), CURLOPT_FOLLOWLOCATION, true],
                [Assert::anything(), CURLOPT_MAXREDIRS, 1],
                [
                    Assert::anything(),
                    CURLOPT_HTTPHEADER,
                    [
                        'Accept: application/x-thrift',
                        'Content-Type: application/x-thrift',
                        'Content-Length: ' . strlen($request),
                    ],
                ],
                [Assert::anything(), CURLOPT_POSTFIELDS, $request],
                [Assert::anything(), CURLOPT_URL, 'http://localhost'],
            ],
            'response' => 'response',
            'responseError' => '',
            'responseCode' => 200,
        ];

        yield 'default' => $default;
        yield 'additionalHeaders' => array_merge(
            $default,
            [
                'headers' => ['test' => '1234567890'],
                'curlSetOptCalls' => [
                    [Assert::anything(), CURLOPT_RETURNTRANSFER, true],
                    [Assert::anything(), CURLOPT_USERAGENT, 'PHP/TCurlClient'],
                    [Assert::anything(), CURLOPT_CUSTOMREQUEST, 'POST'],
                    [Assert::anything(), CURLOPT_FOLLOWLOCATION, true],
                    [Assert::anything(), CURLOPT_MAXREDIRS, 1],
                    [
                        Assert::anything(),
                        CURLOPT_HTTPHEADER,
                        [
                            'Accept: application/x-thrift',
                            'Content-Type: application/x-thrift',
                            'Content-Length: ' . strlen($request),
                            'test: 1234567890',
                        ],
                    ],
                    [Assert::anything(), CURLOPT_POSTFIELDS, $request],
                    [Assert::anything(), CURLOPT_URL, 'http://localhost'],
                ],
            ]
        );
        yield 'uri' => array_merge(
            $default,
            [
                'uri' => 'test1234567890',
                'curlSetOptCalls' => [
                    [Assert::anything(), CURLOPT_RETURNTRANSFER, true],
                    [Assert::anything(), CURLOPT_USERAGENT, 'PHP/TCurlClient'],
                    [Assert::anything(), CURLOPT_CUSTOMREQUEST, 'POST'],
                    [Assert::anything(), CURLOPT_FOLLOWLOCATION, true],
                    [Assert::anything(), CURLOPT_MAXREDIRS, 1],
                    [
                        Assert::anything(),
                        CURLOPT_HTTPHEADER,
                        [
                            'Accept: application/x-thrift',
                            'Content-Type: application/x-thrift',
                            'Content-Length: ' . strlen($request),
                        ],
                    ],
                    [Assert::anything(), CURLOPT_POSTFIELDS, $request],
                    [Assert::anything(), CURLOPT_URL, 'http://localhost/test1234567890'],
                ],
            ]
        );
        yield 'timeout' => array_merge(
            $default,
            [
                'timeout' => 10,
                'connectionTimeout' => 10,
                'curlSetOptCalls' => [
                    [Assert::anything(), CURLOPT_RETURNTRANSFER, true],
                    [Assert::anything(), CURLOPT_USERAGENT, 'PHP/TCurlClient'],
                    [Assert::anything(), CURLOPT_CUSTOMREQUEST, 'POST'],
                    [Assert::anything(), CURLOPT_FOLLOWLOCATION, true],
                    [Assert::anything(), CURLOPT_MAXREDIRS, 1],
                    [
                        Assert::anything(),
                        CURLOPT_HTTPHEADER,
                        [
                            'Accept: application/x-thrift',
                            'Content-Type: application/x-thrift',
                            'Content-Length: ' . strlen($request),
                        ],
                    ],
                    [Assert::anything(), CURLOPT_TIMEOUT, 10],
                    [Assert::anything(), CURLOPT_CONNECTTIMEOUT, 10],
                    [Assert::anything(), CURLOPT_POSTFIELDS, $request],
                    [Assert::anything(), CURLOPT_URL, 'http://localhost'],
                ],
            ]
        );
        yield 'timeout msec' => array_merge(
            $default,
            [
                'timeout' => 0.1,
                'connectionTimeout' => 0.1,
                'curlSetOptCalls' => [
                    [Assert::anything(), CURLOPT_RETURNTRANSFER, true],
                    [Assert::anything(), CURLOPT_USERAGENT, 'PHP/TCurlClient'],
                    [Assert::anything(), CURLOPT_CUSTOMREQUEST, 'POST'],
                    [Assert::anything(), CURLOPT_FOLLOWLOCATION, true],
                    [Assert::anything(), CURLOPT_MAXREDIRS, 1],
                    [
                        Assert::anything(),
                        CURLOPT_HTTPHEADER,
                        [
                            'Accept: application/x-thrift',
                            'Content-Type: application/x-thrift',
                            'Content-Length: ' . strlen($request),
                        ],
                    ],
                    [Assert::anything(), CURLOPT_TIMEOUT_MS, 100.0],
                    [Assert::anything(), CURLOPT_CONNECTTIMEOUT_MS, 100.0],
                    [Assert::anything(), CURLOPT_POSTFIELDS, $request],
                    [Assert::anything(), CURLOPT_URL, 'http://localhost'],
                ],
            ]
        );
        yield 'curl_exec return false' => array_merge(
            $default,
            [
                'response' => false,
                'expectedException' => TTransportException::class,
                'expectedMessage' => 'TCurlClient: Could not connect to http://localhost',
                'expectedCode' => TTransportException::UNKNOWN,
            ]
        );
        yield 'curl_exec return response code 403' => array_merge(
            $default,
            [
                'responseError' => 'Access denied',
                'responseCode' => 403,
                'expectedException' => TTransportException::class,
                'expectedMessage' => 'TCurlClient: Could not connect to http://localhost, Access denied, HTTP status code: 403',
                'expectedCode' => TTransportException::UNKNOWN,
            ]
        );
    }

    public function testCloseCurlHandle()
    {
        $this->getFunctionMock('Thrift\\Transport', 'curl_close')
             ->expects($this->once())
             ->with('testHandle');

        $transport = new TCurlClient('localhost');
        $this->setPropertyValue($transport, 'curlHandle', 'testHandle');

        $transport::closeCurlHandle();
    }
}
