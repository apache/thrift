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
use Thrift\Transport\TCurlClient;

class TCurlClientTest extends TestCase
{
    use PHPMock;

    public function testSetTimeoutSecs()
    {
        $host = 'localhost';
        $transport = new TCurlClient($host);
        $transport->setTimeoutSecs(1000);

        $ref = new \ReflectionClass($transport);
        $prop = $ref->getProperty('timeout_');
        $prop->setAccessible(true);
        $this->assertEquals(1000, $prop->getValue($transport));
    }

    public function testSetConnectionTimeoutSecs()
    {
        $host = 'localhost';
        $transport = new TCurlClient($host);
        $transport->setConnectionTimeoutSecs(1000);

        $ref = new \ReflectionClass($transport);
        $prop = $ref->getProperty('connectionTimeout_');
        $prop->setAccessible(true);
        $this->assertEquals(1000, $prop->getValue($transport));
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

        $ref = new \ReflectionClass($transport);
        $propRequest = $ref->getProperty('request_');
        $propRequest->setAccessible(true);
        $propRequest->setValue($transport, 'testRequest');
        $propResponse = $ref->getProperty('response_');
        $propResponse->setAccessible(true);
        $propResponse->setValue($transport, 'testResponse');

        $this->assertNull($transport->close());
        $this->assertEmpty($propRequest->getValue($transport));
        $this->assertEmpty($propResponse->getValue($transport));
    }

    public function testRead()
    {
        $host = 'localhost';
        $transport = new TCurlClient($host);

        $ref = new \ReflectionClass($transport);
        $propResponse = $ref->getProperty('response_');
        $propResponse->setAccessible(true);
        $propResponse->setValue($transport, '1234567890');

        $response = $transport->read(5);
        $this->assertEquals('12345', $response);
        $this->assertEquals('67890', $propResponse->getValue($transport));

        $response = $transport->read(5);
        $this->assertEquals('67890', $response);
        # The response does not cleaned after reading full answer, maybe it should be fixed
        $this->assertEquals('67890', $propResponse->getValue($transport));
    }

    public function testReadAll()
    {
        $host = 'localhost';
        $transport = new TCurlClient($host);

        $ref = new \ReflectionClass($transport);
        $propResponse = $ref->getProperty('response_');
        $propResponse->setAccessible(true);
        $propResponse->setValue($transport, '1234567890');

        $response = $transport->readAll(5);
        $this->assertEquals('12345', $response);
        $this->assertEquals('67890', $propResponse->getValue($transport));
    }

    public function testReadAll_THRIFT_4656()
    {
        $host = 'localhost';
        $transport = new TCurlClient($host);

        $ref = new \ReflectionClass($transport);
        $propResponse = $ref->getProperty('response_');
        $propResponse->setAccessible(true);
        $propResponse->setValue($transport, '');

        $this->expectException(TTransportException::class);
        $this->expectExceptionMessage('TCurlClient could not read 5 bytes');
        $this->expectExceptionCode(TTransportException::UNKNOWN);

        $transport->readAll(5);
    }

    public function testWrite()
    {
        $host = 'localhost';
        $transport = new TCurlClient($host);

        $ref = new \ReflectionClass($transport);
        $propRequest = $ref->getProperty('request_');
        $propRequest->setAccessible(true);
        $propRequest->setValue($transport, '1234567890');

        $transport->write('12345');
        $this->assertEquals('123456789012345', $propRequest->getValue($transport));
    }

    public function testAddHeaders()
    {
        $host = 'localhost';
        $transport = new TCurlClient($host);

        $ref = new \ReflectionClass($transport);
        $propRequest = $ref->getProperty('headers_');
        $propRequest->setAccessible(true);
        $propRequest->setValue($transport, ['test' => '1234567890']);

        $transport->addHeaders(['test2' => '12345']);
        $this->assertEquals(['test' => '1234567890', 'test2' => '12345'], $propRequest->getValue($transport));
    }

    /**
     * @dataProvider flushDataProvider
     */
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
                         return is_array(
                                 $arg
                             ) && $arg[0] === 'Thrift\\Transport\\TCurlClient' && $arg[1] === 'closeCurlHandle';
                     }
                 )
             );
        $this->getFunctionMock('Thrift\\Transport', 'curl_init')
             ->expects($this->once());

        $this->getFunctionMock('Thrift\\Transport', 'curl_setopt')
             ->expects($this->any())
             ->withConsecutive(...$curlSetOptCalls)
             ->willReturn(true);

        $this->getFunctionMock('Thrift\\Transport', 'curl_exec')
             ->expects($this->once())
             ->with($this->anything())
             ->willReturn($response);

        $this->getFunctionMock('Thrift\\Transport', 'curl_error')
             ->expects($this->once())
             ->with($this->anything())
             ->willReturn($responseError);

        $this->getFunctionMock('Thrift\\Transport', 'curl_getinfo')
             ->expects($this->once())
             ->with($this->anything(), CURLINFO_HTTP_CODE)
             ->willReturn($responseCode);

        if (!is_null($expectedException)) {
            $this->expectException($expectedException);
            $this->expectExceptionMessage($expectedMessage);
            $this->expectExceptionCode($expectedCode);

            $this->getFunctionMock('Thrift\\Transport', 'curl_close')
                 ->expects($this->once())
                 ->with($this->anything());
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

    public function flushDataProvider()
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
                [$this->anything(), CURLOPT_RETURNTRANSFER, true],
                [$this->anything(), CURLOPT_USERAGENT, 'PHP/TCurlClient'],
                [$this->anything(), CURLOPT_CUSTOMREQUEST, 'POST'],
                [$this->anything(), CURLOPT_FOLLOWLOCATION, true],
                [$this->anything(), CURLOPT_MAXREDIRS, 1],
                [
                    $this->anything(),
                    CURLOPT_HTTPHEADER,
                    [
                        'Accept: application/x-thrift',
                        'Content-Type: application/x-thrift',
                        'Content-Length: ' . strlen($request),
                    ],
                ],
                [$this->anything(), CURLOPT_POSTFIELDS, $request],
                [$this->anything(), CURLOPT_URL, 'http://localhost'],
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
                    [$this->anything(), CURLOPT_RETURNTRANSFER, true],
                    [$this->anything(), CURLOPT_USERAGENT, 'PHP/TCurlClient'],
                    [$this->anything(), CURLOPT_CUSTOMREQUEST, 'POST'],
                    [$this->anything(), CURLOPT_FOLLOWLOCATION, true],
                    [$this->anything(), CURLOPT_MAXREDIRS, 1],
                    [
                        $this->anything(),
                        CURLOPT_HTTPHEADER,
                        [
                            'Accept: application/x-thrift',
                            'Content-Type: application/x-thrift',
                            'Content-Length: ' . strlen($request),
                            'test: 1234567890',
                        ],
                    ],
                    [$this->anything(), CURLOPT_POSTFIELDS, $request],
                    [$this->anything(), CURLOPT_URL, 'http://localhost'],
                ],
            ]
        );
        yield 'uri' => array_merge(
            $default,
            [
                'uri' => 'test1234567890',
                'curlSetOptCalls' => [
                    [$this->anything(), CURLOPT_RETURNTRANSFER, true],
                    [$this->anything(), CURLOPT_USERAGENT, 'PHP/TCurlClient'],
                    [$this->anything(), CURLOPT_CUSTOMREQUEST, 'POST'],
                    [$this->anything(), CURLOPT_FOLLOWLOCATION, true],
                    [$this->anything(), CURLOPT_MAXREDIRS, 1],
                    [
                        $this->anything(),
                        CURLOPT_HTTPHEADER,
                        [
                            'Accept: application/x-thrift',
                            'Content-Type: application/x-thrift',
                            'Content-Length: ' . strlen($request),
                        ],
                    ],
                    [$this->anything(), CURLOPT_POSTFIELDS, $request],
                    [$this->anything(), CURLOPT_URL, 'http://localhost/test1234567890'],
                ],
            ]
        );
        yield 'timeout' => array_merge(
            $default,
            [
                'timeout' => 10,
                'connectionTimeout' => 10,
                'curlSetOptCalls' => [
                    [$this->anything(), CURLOPT_RETURNTRANSFER, true],
                    [$this->anything(), CURLOPT_USERAGENT, 'PHP/TCurlClient'],
                    [$this->anything(), CURLOPT_CUSTOMREQUEST, 'POST'],
                    [$this->anything(), CURLOPT_FOLLOWLOCATION, true],
                    [$this->anything(), CURLOPT_MAXREDIRS, 1],
                    [
                        $this->anything(),
                        CURLOPT_HTTPHEADER,
                        [
                            'Accept: application/x-thrift',
                            'Content-Type: application/x-thrift',
                            'Content-Length: ' . strlen($request),
                        ],
                    ],
                    [$this->anything(), CURLOPT_TIMEOUT, 10],
                    [$this->anything(), CURLOPT_CONNECTTIMEOUT, 10],
                    [$this->anything(), CURLOPT_POSTFIELDS, $request],
                    [$this->anything(), CURLOPT_URL, 'http://localhost'],
                ],
            ]
        );
        yield 'timeout msec' => array_merge(
            $default,
            [
                'timeout' => 0.1,
                'connectionTimeout' => 0.1,
                'curlSetOptCalls' => [
                    [$this->anything(), CURLOPT_RETURNTRANSFER, true],
                    [$this->anything(), CURLOPT_USERAGENT, 'PHP/TCurlClient'],
                    [$this->anything(), CURLOPT_CUSTOMREQUEST, 'POST'],
                    [$this->anything(), CURLOPT_FOLLOWLOCATION, true],
                    [$this->anything(), CURLOPT_MAXREDIRS, 1],
                    [
                        $this->anything(),
                        CURLOPT_HTTPHEADER,
                        [
                            'Accept: application/x-thrift',
                            'Content-Type: application/x-thrift',
                            'Content-Length: ' . strlen($request),
                        ],
                    ],
                    [$this->anything(), CURLOPT_TIMEOUT_MS, 100],
                    [$this->anything(), CURLOPT_CONNECTTIMEOUT_MS, 100],
                    [$this->anything(), CURLOPT_POSTFIELDS, $request],
                    [$this->anything(), CURLOPT_URL, 'http://localhost'],
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
        $ref = new \ReflectionClass($transport);
        $prop = $ref->getProperty('curlHandle');
        $prop->setAccessible(true);
        $prop->setValue($transport, 'testHandle');

        $transport::closeCurlHandle();
    }
}
