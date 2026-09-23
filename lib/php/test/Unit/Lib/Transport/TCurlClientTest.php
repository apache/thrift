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
use PHPUnit\Framework\Constraint\Constraint;
use PHPUnit\Framework\Assert;
use PHPUnit\Framework\Attributes\DataProvider;
use ReflectionProperty;
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

        $this->assertEquals(1000, (new ReflectionProperty($transport, 'timeout'))->getValue($transport));
    }

    public function testSetConnectionTimeoutSecs()
    {
        $host = 'localhost';
        $transport = new TCurlClient($host);
        $transport->setConnectionTimeoutSecs(1000);

        $this->assertEquals(1000, (new ReflectionProperty($transport, 'connectionTimeout'))->getValue($transport));
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

        (new ReflectionProperty($transport, 'request'))->setValue($transport, 'testRequest');
        (new ReflectionProperty($transport, 'response'))->setValue($transport, 'testResponse');

        $this->assertNull($transport->close());
        $this->assertEmpty((new ReflectionProperty($transport, 'request'))->getValue($transport));
        $this->assertEmpty((new ReflectionProperty($transport, 'response'))->getValue($transport));
    }

    public function testRead()
    {
        $host = 'localhost';
        $transport = new TCurlClient($host);

        (new ReflectionProperty($transport, 'response'))->setValue($transport, '1234567890');

        $response = $transport->read(5);
        $this->assertEquals('12345', $response);

        $response = $transport->read(5);
        $this->assertEquals('67890', $response);
        # The response does not cleaned after reading full answer, maybe it should be fixed
        $this->assertEquals('67890', $transport->read(5));
    }

    public function testReadAll()
    {
        $host = 'localhost';
        $transport = new TCurlClient($host);

        (new ReflectionProperty($transport, 'response'))->setValue($transport, '1234567890');

        $response = $transport->readAll(5);
        $this->assertEquals('12345', $response);
        $this->assertEquals('67890', $transport->readAll(5));
    }

    public function testReadAfterFlushStartsAtTheNewResponse()
    {
        $this->getFunctionMock('Thrift\\Transport', 'register_shutdown_function');
        $this->getFunctionMock('Thrift\\Transport', 'curl_init')->expects($this->any())->willReturn(true);
        $this->getFunctionMock('Thrift\\Transport', 'curl_setopt')->expects($this->any())->willReturn(true);
        $this->getFunctionMock('Thrift\\Transport', 'curl_exec')
             ->expects($this->exactly(2))
             ->willReturnOnConsecutiveCalls('abcdef', '123456');
        $this->getFunctionMock('Thrift\\Transport', 'curl_error')->expects($this->any())->willReturn('');
        $this->getFunctionMock('Thrift\\Transport', 'curl_getinfo')->expects($this->any())->willReturn(200);

        $transport = new TCurlClient('localhost');
        $transport->flush();
        $this->assertEquals('ab', $transport->read(2));

        $transport->flush();
        $this->assertEquals('123', $transport->read(3));
        $this->assertEquals('456', $transport->readAll(3));
    }

    public function testReadAllThrift4656()
    {
        $host = 'localhost';
        $transport = new TCurlClient($host);

        (new ReflectionProperty($transport, 'response'))->setValue($transport, '');

        $this->expectException(TTransportException::class);
        $this->expectExceptionMessage('TCurlClient could not read 5 bytes');
        $this->expectExceptionCode(TTransportException::UNKNOWN);

        $transport->readAll(5);
    }

    public function testClientsKeepIndependentTimeoutsAndReuseTheirOwnHandles(): void
    {
        $this->getFunctionMock('Thrift\\Transport', 'register_shutdown_function');
        $firstHandle = new \stdClass();
        $secondHandle = new \stdClass();
        $this->getFunctionMock('Thrift\\Transport', 'curl_init')
            ->expects($this->exactly(2))
            ->willReturnOnConsecutiveCalls($firstHandle, $secondHandle);

        $options = new \SplObjectStorage();
        $this->getFunctionMock('Thrift\\Transport', 'curl_setopt')
            ->expects($this->any())
            ->willReturnCallback(function ($handle, $option, $value) use ($options) {
                $settings = $options->contains($handle) ? $options[$handle] : [];
                $settings[$option] = $value;
                $options[$handle] = $settings;

                return true;
            });
        $requests = [];
        $this->getFunctionMock('Thrift\\Transport', 'curl_exec')
            ->expects($this->exactly(3))
            ->willReturnCallback(function ($handle) use ($options, &$requests) {
                $settings = $options[$handle];
                $requests[] = [
                    $handle,
                    $settings[CURLOPT_TIMEOUT_MS] ?? null,
                    $settings[CURLOPT_CONNECTTIMEOUT_MS] ?? null,
                ];

                return 'reply';
            });
        $this->getFunctionMock('Thrift\\Transport', 'curl_getinfo')->expects($this->any())->willReturn(200);
        $this->getFunctionMock('Thrift\\Transport', 'curl_error')->expects($this->any())->willReturn('');

        $first = new TCurlClient('localhost');
        $first->setTimeoutSecs(0.1);
        $first->setConnectionTimeoutSecs(0.2);
        $first->flush();
        $second = new TCurlClient('localhost');
        $second->flush();
        $first->flush();

        $this->assertSame([
            [$firstHandle, 100.0, 200.0],
            [$secondHandle, null, null],
            [$firstHandle, 100.0, 200.0],
        ], $requests);
    }

    #[DataProvider('releaseHandleDataProvider')]
    public function testReleasingOneClientLeavesTheOtherHandleUsable(string $release): void
    {
        $this->getFunctionMock('Thrift\\Transport', 'register_shutdown_function')->expects($this->never());
        $firstHandle = new \stdClass();
        $secondHandle = new \stdClass();
        $replacementHandle = new \stdClass();
        $this->getFunctionMock('Thrift\\Transport', 'curl_init')
            ->expects($this->exactly(3))
            ->willReturnOnConsecutiveCalls($firstHandle, $secondHandle, $replacementHandle);
        $this->getFunctionMock('Thrift\\Transport', 'curl_setopt')->expects($this->any())->willReturn(true);
        $this->getFunctionMock('Thrift\\Transport', 'curl_getinfo')->expects($this->any())->willReturn(200);
        $this->getFunctionMock('Thrift\\Transport', 'curl_error')->expects($this->any())->willReturn('');
        $handles = [];
        $this->getFunctionMock('Thrift\\Transport', 'curl_exec')
            ->expects($this->exactly($release === 'failure' ? 5 : 4))
            ->willReturnCallback(function ($handle) use (&$handles, $release) {
                $handles[] = $handle;

                return $release === 'failure' && count($handles) === 3 ? false : 'reply';
            });

        $first = new TCurlClient('localhost');
        $second = new TCurlClient('localhost');
        $first->flush();
        $second->flush();
        if ($release === 'failure') {
            try {
                $first->flush();
                $this->fail('Expected the failed transfer to throw');
            } catch (TTransportException $e) {
                $this->assertSame(TTransportException::UNKNOWN, $e->getCode());
            }
        } else {
            $first->$release();
        }
        $second->flush();
        $first->flush();

        $expected = [$firstHandle, $secondHandle];
        if ($release === 'failure') {
            $expected[] = $firstHandle;
        }
        $expected[] = $secondHandle;
        $expected[] = $replacementHandle;
        $this->assertSame($expected, $handles);
        $this->assertSame('reply', $first->readAll(5));
        $this->assertSame('reply', $second->readAll(5));
    }

    public static function releaseHandleDataProvider(): array
    {
        return [
            'close transport' => ['close'],
            'close handle' => ['closeCurlHandle'],
            'failed transfer' => ['failure'],
        ];
    }

    public function testWrite()
    {
        $host = 'localhost';
        $transport = new TCurlClient($host);

        (new ReflectionProperty($transport, 'request'))->setValue($transport, '1234567890');

        $transport->write('12345');
        $this->assertEquals('123456789012345', (new ReflectionProperty($transport, 'request'))->getValue($transport));
    }

    public function testAddHeaders()
    {
        $host = 'localhost';
        $transport = new TCurlClient($host);

        (new ReflectionProperty($transport, 'headers'))->setValue($transport, ['test' => '1234567890']);

        $transport->addHeaders(['test2' => '12345']);
        $this->assertEquals(
            ['test' => '1234567890', 'test2' => '12345'],
            (new ReflectionProperty($transport, 'headers'))->getValue($transport)
        );
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
            ->expects($this->never());
        $this->getFunctionMock('Thrift\\Transport', 'curl_init')
             ->expects($this->once());

        $this->getFunctionMock('Thrift\\Transport', 'curl_setopt')
             ->expects($this->exactly(count($curlSetOptCalls)))
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
                 ->expects($this->never());
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
                [Assert::anything(), CURLOPT_FOLLOWLOCATION, false],
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
                    [Assert::anything(), CURLOPT_FOLLOWLOCATION, false],
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
                    [Assert::anything(), CURLOPT_FOLLOWLOCATION, false],
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
                'timeout' => 10.0,
                'connectionTimeout' => 10.0,
                'curlSetOptCalls' => [
                    [Assert::anything(), CURLOPT_RETURNTRANSFER, true],
                    [Assert::anything(), CURLOPT_USERAGENT, 'PHP/TCurlClient'],
                    [Assert::anything(), CURLOPT_CUSTOMREQUEST, 'POST'],
                    [Assert::anything(), CURLOPT_FOLLOWLOCATION, false],
                    [
                        Assert::anything(),
                        CURLOPT_HTTPHEADER,
                        [
                            'Accept: application/x-thrift',
                            'Content-Type: application/x-thrift',
                            'Content-Length: ' . strlen($request),
                        ],
                    ],
                    [Assert::anything(), CURLOPT_TIMEOUT, 10.0],
                    [Assert::anything(), CURLOPT_CONNECTTIMEOUT, 10.0],
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
                    [Assert::anything(), CURLOPT_FOLLOWLOCATION, false],
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

    /**
     * flush() follows one redirect, and only within the origin of the URL it
     * requested: the same scheme, host and port. The request is sent again to
     * the path and query of the redirect target, under the scheme, host and
     * port of that URL. Any other redirect fails the request.
     */
    #[DataProvider('redirectDataProvider')]
    public function testFlushFollowsOneRedirectWithinTheOrigin(
        $scheme,
        $port,
        $exchanges,
        $expectedUrls,
        $expectedResponse
    ) {
        $this->getFunctionMock('Thrift\\Transport', 'register_shutdown_function');
        $this->getFunctionMock('Thrift\\Transport', 'curl_init')->expects($this->once())->willReturn(true);
        $this->getFunctionMock('Thrift\\Transport', 'curl_error')->expects($this->once())->willReturn('');

        $urls = [];
        $this->getFunctionMock('Thrift\\Transport', 'curl_setopt')
             ->expects($this->atLeastOnce())
             ->willReturnCallback(function ($handle, $option, $value) use (&$urls) {
                if ($option === CURLOPT_URL) {
                    $urls[] = $value;
                }

                 return true;
             });

        $exchange = -1;
        $this->getFunctionMock('Thrift\\Transport', 'curl_exec')
             ->expects($this->exactly(count($expectedUrls)))
             ->willReturnCallback(function () use (&$exchange) {
                 $exchange++;

                 return 'response ' . $exchange;
             });
        $this->getFunctionMock('Thrift\\Transport', 'curl_getinfo')
             ->expects($this->atLeastOnce())
             ->willReturnCallback(function ($handle, $option) use (&$exchange, $exchanges) {
                 [$code, $location] = $exchanges[$exchange];
                if ($option === CURLINFO_HTTP_CODE) {
                    return $code;
                }
                 $this->assertSame(CURLINFO_REDIRECT_URL, $option);

                 return $location;
             });

        $transport = new TCurlClient('localhost', $port, '/rpc', $scheme);
        $transport->write('request');
        try {
            $transport->flush();
            $response = $transport->read(64);
        } catch (TTransportException $e) {
            $response = null;
        }

        $this->assertSame($expectedUrls, $urls);
        $this->assertSame($expectedResponse, $response);
    }

    public static function redirectDataProvider()
    {
        yield 'same origin' => [
            'http', 80,
            [[302, 'http://localhost/moved'], [200, false]],
            ['http://localhost/rpc', 'http://localhost/moved'],
            'response 1',
        ];
        yield 'the query is kept' => [
            'http', 80,
            [[307, 'http://localhost/moved?a=1&b=2'], [200, false]],
            ['http://localhost/rpc', 'http://localhost/moved?a=1&b=2'],
            'response 1',
        ];
        yield 'host in another case' => [
            'http', 80,
            [[301, 'HTTP://LocalHost/moved'], [200, false]],
            ['http://localhost/rpc', 'http://localhost/moved'],
            'response 1',
        ];
        yield 'default port named' => [
            'http', 80,
            [[308, 'http://localhost:80/moved'], [200, false]],
            ['http://localhost/rpc', 'http://localhost/moved'],
            'response 1',
        ];
        yield 'https on port 443' => [
            'https', 443,
            [[307, 'https://localhost/moved'], [200, false]],
            ['https://localhost:443/rpc', 'https://localhost:443/moved'],
            'response 1',
        ];
        yield 'https with the port left at 80, which the URL leaves out' => [
            'https', 80,
            [[302, 'https://localhost:443/moved'], [200, false]],
            ['https://localhost/rpc', 'https://localhost/moved'],
            'response 1',
        ];
        yield 'another host' => [
            'http', 80,
            [[302, 'http://example.com/moved']],
            ['http://localhost/rpc'],
            null,
        ];
        yield 'another port' => [
            'http', 80,
            [[307, 'http://localhost:8080/moved']],
            ['http://localhost/rpc'],
            null,
        ];
        yield 'another scheme' => [
            'http', 80,
            [[301, 'https://localhost/moved']],
            ['http://localhost/rpc'],
            null,
        ];
        yield 'user info in front of another host' => [
            'http', 80,
            [[302, 'http://localhost@example.com/moved']],
            ['http://localhost/rpc'],
            null,
        ];
        yield 'no location' => [
            'http', 80,
            [[302, false]],
            ['http://localhost/rpc'],
            null,
        ];
        yield 'a second redirect' => [
            'http', 80,
            [[302, 'http://localhost/moved'], [302, 'http://localhost/again']],
            ['http://localhost/rpc', 'http://localhost/moved'],
            null,
        ];
    }

    public function testCloseCurlHandle()
    {
        $this->getFunctionMock('Thrift\\Transport', 'curl_close')
             ->expects($this->never());

        $transport = new TCurlClient('localhost');
        $curlHandle = new ReflectionProperty($transport, 'curlHandle');
        $curlHandle->setValue($transport, 'testHandle');

        $transport->closeCurlHandle();

        $this->assertNull($curlHandle->getValue($transport));
    }
}
