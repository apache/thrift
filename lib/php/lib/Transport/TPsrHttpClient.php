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
 *
 * @package thrift.transport
 */

declare(strict_types=1);

namespace Thrift\Transport;

use Http\Discovery\Exception\NotFoundException;
use Http\Discovery\Psr17FactoryDiscovery;
use Http\Discovery\Psr18ClientDiscovery;
use Psr\Http\Client\ClientExceptionInterface;
use Psr\Http\Client\ClientInterface;
use Psr\Http\Client\NetworkExceptionInterface;
use Psr\Http\Message\RequestFactoryInterface;
use Psr\Http\Message\StreamFactoryInterface;
use Thrift\Exception\TTransportException;

/**
 * HTTP client for Thrift backed by any PSR-18 ClientInterface.
 *
 * If no client/factories are provided, they are auto-discovered via
 * php-http/discovery. Install one of the suggested PSR-18 implementations
 * (e.g. guzzlehttp/guzzle, symfony/http-client, php-http/curl-client) and a
 * PSR-7 implementation (e.g. nyholm/psr7) at runtime.
 *
 * @package thrift.transport
 */
class TPsrHttpClient extends TTransport
{
    protected string $request = '';

    protected string $response = '';

    /**
     * @var array<string, string|int>
     */
    protected array $headers = [];

    protected ClientInterface $client;
    protected RequestFactoryInterface $requestFactory;
    protected StreamFactoryInterface $streamFactory;

    /**
     * @throws TTransportException when a dependency must be auto-discovered but
     *         no PSR-18 client or PSR-17 factory is installed.
     */
    public function __construct(
        protected string $url,
        ?ClientInterface $client = null,
        ?RequestFactoryInterface $requestFactory = null,
        ?StreamFactoryInterface $streamFactory = null,
    ) {
        $this->client = $client ?? self::discover(
            Psr18ClientDiscovery::find(...),
            'PSR-18 client',
            'guzzlehttp/guzzle, symfony/http-client, php-http/curl-client',
        );
        $this->requestFactory = $requestFactory ?? self::discover(
            Psr17FactoryDiscovery::findRequestFactory(...),
            'PSR-17 request factory',
            'nyholm/psr7 or guzzlehttp/psr7',
        );
        $this->streamFactory = $streamFactory ?? self::discover(
            Psr17FactoryDiscovery::findStreamFactory(...),
            'PSR-17 stream factory',
            'nyholm/psr7 or guzzlehttp/psr7',
        );
    }

    public function isOpen(): bool
    {
        return true;
    }

    public function open(): void
    {
    }

    public function close(): void
    {
        $this->request = '';
        $this->response = '';
    }

    public function read(int $len): string
    {
        if ($len >= strlen($this->response)) {
            $ret = $this->response;
            $this->response = '';

            return $ret;
        }

        $ret = substr($this->response, 0, $len);
        $this->response = substr($this->response, $len);

        return $ret;
    }

    /**
     * Guarantees that the full amount of data is read. Since the entire HTTP
     * response is buffered up-front in {@see self::flush()}, the default
     * loop-based readAll cannot be used.
     *
     * @throws TTransportException if cannot read data
     */
    public function readAll(int $len): string
    {
        $data = $this->read($len);

        if (strlen($data) !== $len) {
            throw new TTransportException('TPsrHttpClient could not read ' . $len . ' bytes');
        }

        return $data;
    }

    public function write(string $buf): void
    {
        $this->request .= $buf;
    }

    /**
     * Sends the buffered request over HTTP using the injected PSR-18 client.
     *
     * On failure the request buffer is consumed; the caller cannot retry the
     * same payload without rewriting it via {@see self::write()}.
     *
     * @throws TTransportException if the URL or headers are invalid, the
     *         request fails, or the response is non-200
     */
    public function flush(): void
    {
        try {
            $body = $this->streamFactory->createStream($this->request);
            $defaultHeaders = [
                'Accept' => 'application/x-thrift',
                'Content-Type' => 'application/x-thrift',
                'Content-Length' => (string) ($body->getSize() ?? strlen($this->request)),
                'User-Agent' => 'PHP/TPsrHttpClient',
            ];
            $psrRequest = $this->requestFactory->createRequest('POST', $this->url)
                ->withBody($body);
            foreach (array_merge($defaultHeaders, $this->headers) as $name => $value) {
                $psrRequest = $psrRequest->withHeader($name, (string) $value);
            }
        } catch (\InvalidArgumentException | \RuntimeException $e) {
            throw new TTransportException(
                'TPsrHttpClient: invalid request for ' . $this->url . ': ' . $e->getMessage(),
                TTransportException::NOT_OPEN,
            );
        }

        $this->request = '';

        try {
            $response = $this->client->sendRequest($psrRequest);
        } catch (NetworkExceptionInterface $e) {
            throw new TTransportException(
                'TPsrHttpClient: Could not connect to ' . $this->url . ': ' . $e->getMessage(),
                TTransportException::NOT_OPEN,
            );
        } catch (ClientExceptionInterface $e) {
            throw new TTransportException(
                'TPsrHttpClient: Request to ' . $this->url . ' failed: ' . $e->getMessage(),
                TTransportException::UNKNOWN,
            );
        }

        $code = $response->getStatusCode();
        if ($code !== 200) {
            throw new TTransportException(
                'TPsrHttpClient: Could not connect to ' . $this->url . ', HTTP status code: ' . $code,
                TTransportException::UNKNOWN,
            );
        }

        $this->response = (string) $response->getBody();
    }

    /**
     * @param array<string, string|int> $headers
     */
    public function addHeaders(array $headers): void
    {
        $this->headers = array_merge($this->headers, $headers);
    }

    /**
     * @template T
     * @param callable(): T $find
     * @return T
     *
     * @throws TTransportException when discovery cannot locate the dependency
     */
    private static function discover(callable $find, string $name, string $suggested): mixed
    {
        try {
            return $find();
        } catch (NotFoundException) {
            throw new TTransportException(
                "TPsrHttpClient: no $name found. Install $suggested.",
                TTransportException::NOT_OPEN,
            );
        }
    }
}
