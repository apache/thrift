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

use Nyholm\Psr7\Factory\Psr17Factory;
use PHPUnit\Framework\Attributes\DataProvider;
use PHPUnit\Framework\TestCase;
use Psr\Http\Client\ClientExceptionInterface;
use Psr\Http\Client\ClientInterface;
use Psr\Http\Client\NetworkExceptionInterface;
use Psr\Http\Message\RequestInterface;
use Psr\Http\Message\ResponseInterface;
use Test\Thrift\Unit\Lib\ReflectionHelper;
use Thrift\Exception\TTransportException;
use Thrift\Transport\TPsrHttpClient;

class TPsrHttpClientTest extends TestCase
{
    use ReflectionHelper;

    private Psr17Factory $psr17;

    protected function setUp(): void
    {
        $this->psr17 = new Psr17Factory();
    }

    private function makeClient(ResponseInterface|\Throwable $behavior): ClientInterface
    {
        return new class ($behavior) implements ClientInterface {
            public ?RequestInterface $lastRequest = null;

            public function __construct(private ResponseInterface|\Throwable $behavior)
            {
            }

            public function sendRequest(RequestInterface $request): ResponseInterface
            {
                $this->lastRequest = $request;
                if ($this->behavior instanceof \Throwable) {
                    throw $this->behavior;
                }
                return $this->behavior;
            }
        };
    }

    private function makeTransport(
        ClientInterface $client,
        string $url = 'http://localhost',
    ): TPsrHttpClient {
        return new TPsrHttpClient(
            $url,
            $client,
            $this->psr17,
            $this->psr17,
        );
    }

    public function testIsOpenAlwaysTrue(): void
    {
        $transport = $this->makeTransport($this->makeClient($this->psr17->createResponse(200)));
        $this->assertTrue($transport->isOpen());
    }

    public function testOpenIsNoop(): void
    {
        $transport = $this->makeTransport($this->makeClient($this->psr17->createResponse(200)));
        $this->assertNull($transport->open());
    }

    public function testCloseClearsBuffers(): void
    {
        $transport = $this->makeTransport($this->makeClient($this->psr17->createResponse(200)));
        $this->setPropertyValue($transport, 'request', 'pending');
        $this->setPropertyValue($transport, 'response', 'leftover');

        $transport->close();

        $this->assertSame('', $this->getPropertyValue($transport, 'request'));
        $this->assertSame('', $this->getPropertyValue($transport, 'response'));
    }

    public function testWriteAccumulatesBuffer(): void
    {
        $transport = $this->makeTransport($this->makeClient($this->psr17->createResponse(200)));

        $transport->write('foo');
        $transport->write('bar');

        $this->assertSame('foobar', $this->getPropertyValue($transport, 'request'));
    }

    public function testReadConsumesResponseBuffer(): void
    {
        $transport = $this->makeTransport($this->makeClient($this->psr17->createResponse(200)));
        $this->setPropertyValue($transport, 'response', '1234567890');

        $this->assertSame('12345', $transport->read(5));
        $this->assertSame('67890', $this->getPropertyValue($transport, 'response'));
        $this->assertSame('67890', $transport->read(99));
        $this->assertSame('', $this->getPropertyValue($transport, 'response'));
    }

    public function testReadAllThrowsWhenShort(): void
    {
        $transport = $this->makeTransport($this->makeClient($this->psr17->createResponse(200)));
        $this->setPropertyValue($transport, 'response', 'abc');

        $this->expectException(TTransportException::class);
        $this->expectExceptionMessage('TPsrHttpClient could not read 10 bytes');

        $transport->readAll(10);
    }

    public function testAddHeadersMergesWithExisting(): void
    {
        $transport = $this->makeTransport($this->makeClient($this->psr17->createResponse(200)));
        $this->setPropertyValue($transport, 'headers', ['X-Existing' => 'old']);

        $transport->addHeaders(['X-New' => 'new', 'X-Existing' => 'replaced']);

        $this->assertSame(
            ['X-Existing' => 'replaced', 'X-New' => 'new'],
            $this->getPropertyValue($transport, 'headers'),
        );
    }

    public function testFlushSendsPostWithDefaultHeaders(): void
    {
        $client = $this->makeClient($this->psr17->createResponse(200));
        $transport = $this->makeTransport($client);

        $transport->write('payload-bytes');
        $transport->flush();

        $request = $client->lastRequest;
        $this->assertNotNull($request);
        $this->assertSame('POST', $request->getMethod());
        $this->assertSame('http://localhost', (string) $request->getUri());
        $this->assertSame('payload-bytes', (string) $request->getBody());
        $this->assertSame('application/x-thrift', $request->getHeaderLine('Accept'));
        $this->assertSame('application/x-thrift', $request->getHeaderLine('Content-Type'));
        $this->assertSame((string) strlen('payload-bytes'), $request->getHeaderLine('Content-Length'));
        $this->assertSame('PHP/TPsrHttpClient', $request->getHeaderLine('User-Agent'));
    }

    public function testFlushWithEmptyBody(): void
    {
        $client = $this->makeClient($this->psr17->createResponse(200));
        $transport = $this->makeTransport($client);

        $transport->flush();

        $request = $client->lastRequest;
        $this->assertNotNull($request);
        $this->assertSame('', (string) $request->getBody());
        $this->assertSame('0', $request->getHeaderLine('Content-Length'));
    }

    public function testFlushClearsRequestBufferAfterSend(): void
    {
        $client = $this->makeClient($this->psr17->createResponse(200));
        $transport = $this->makeTransport($client);
        $transport->write('payload');

        $transport->flush();

        $this->assertSame('', $this->getPropertyValue($transport, 'request'));
    }

    #[DataProvider('urlProvider')]
    public function testFlushPassesUrlThroughToRequest(string $url): void
    {
        $client = $this->makeClient($this->psr17->createResponse(200));
        $transport = $this->makeTransport($client, $url);

        $transport->flush();

        $this->assertSame($url, (string) $client->lastRequest->getUri());
    }

    public static function urlProvider(): iterable
    {
        yield 'http no path' => ['http://localhost'];
        yield 'http with path' => ['http://example.com/rpc'];
        yield 'http non-default port' => ['http://example.com:8080/rpc'];
        yield 'https with path' => ['https://example.com/rpc'];
        yield 'https with port' => ['https://example.com:8443/rpc'];
        yield 'nested path' => ['http://example.com/api/v1/thrift'];
    }

    public function testFlushAppliesCustomHeaders(): void
    {
        $client = $this->makeClient($this->psr17->createResponse(200));
        $transport = $this->makeTransport($client);
        $transport->addHeaders(['X-Test-Header' => 'value-1', 'Authorization' => 'Bearer token']);

        $transport->flush();

        $request = $client->lastRequest;
        $this->assertSame('value-1', $request->getHeaderLine('X-Test-Header'));
        $this->assertSame('Bearer token', $request->getHeaderLine('Authorization'));
    }

    public function testFlushAllowsCustomHeadersToOverrideDefaults(): void
    {
        $client = $this->makeClient($this->psr17->createResponse(200));
        $transport = $this->makeTransport($client);
        $transport->addHeaders(['User-Agent' => 'custom-agent']);

        $transport->flush();

        $this->assertSame('custom-agent', $client->lastRequest->getHeaderLine('User-Agent'));
    }

    public function testFlushPopulatesResponseBufferFromBody(): void
    {
        $response = $this->psr17->createResponse(200)
            ->withBody($this->psr17->createStream('response-bytes'));
        $client = $this->makeClient($response);
        $transport = $this->makeTransport($client);

        $transport->flush();

        $this->assertSame('response-bytes', $transport->readAll(strlen('response-bytes')));
    }

    public function testFlushWrapsInvalidUrlAsNotOpen(): void
    {
        $client = $this->makeClient($this->psr17->createResponse(200));
        $transport = $this->makeTransport($client, 'http://example.com:99999/rpc');

        $this->expectException(TTransportException::class);
        $this->expectExceptionCode(TTransportException::NOT_OPEN);
        $this->expectExceptionMessage('TPsrHttpClient: invalid request for http://example.com:99999/rpc');

        $transport->flush();
    }

    public function testFlushThrowsOnNon200(): void
    {
        $client = $this->makeClient($this->psr17->createResponse(500));
        $transport = $this->makeTransport($client, 'http://example.com:8080/rpc');

        $this->expectException(TTransportException::class);
        $this->expectExceptionMessage('TPsrHttpClient: Could not connect to http://example.com:8080/rpc, HTTP status code: 500');
        $this->expectExceptionCode(TTransportException::UNKNOWN);

        $transport->flush();
    }

    public function testFlushWrapsNetworkExceptionAsNotOpen(): void
    {
        $networkException = new class ('boom') extends \RuntimeException implements NetworkExceptionInterface {
            public function getRequest(): RequestInterface
            {
                throw new \LogicException('not used');
            }
        };
        $transport = $this->makeTransport($this->makeClient($networkException));

        $this->expectException(TTransportException::class);
        $this->expectExceptionCode(TTransportException::NOT_OPEN);
        $this->expectExceptionMessage('TPsrHttpClient: Could not connect to http://localhost: boom');

        $transport->flush();
    }

    public function testFlushWrapsGenericClientExceptionAsUnknown(): void
    {
        $clientException = new class ('client error') extends \RuntimeException implements ClientExceptionInterface {
        };
        $transport = $this->makeTransport($this->makeClient($clientException));

        $this->expectException(TTransportException::class);
        $this->expectExceptionCode(TTransportException::UNKNOWN);
        $this->expectExceptionMessage('TPsrHttpClient: Request to http://localhost failed: client error');

        $transport->flush();
    }
}
