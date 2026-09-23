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

use Thrift\Exception\TTransportException;

/**
 * HTTP client for Thrift
 *
 * @package thrift.transport
 */
class TCurlClient extends TTransport
{
    /**
     * The port a URL without one refers to, by scheme.
     */
    private const DEFAULT_PORTS = ['http' => 80, 'https' => 443];

    /** @var \CurlHandle|null */
    private $curlHandle;

    /**
     * The URI to request
     */
    protected string $uri;

    /**
     * Buffer for the HTTP request data
     */
    protected string $request = '';

    /**
     * Buffer for the HTTP response data. `false` reflects a failed curl_exec.
     */
    protected string|false|null $response = null;

    /**
     * Offset of the first unread byte in $response.
     */
    private int $responsePos = 0;

    /**
     * Read timeout in seconds.
     */
    protected ?float $timeout = null;

    /**
     * Connection timeout in seconds.
     */
    protected ?float $connectionTimeout = null;

    /**
     * http headers
     *
     * @var array<string, string|int>
     */
    protected array $headers = [];

    /**
     * Make a new HTTP client.
     */
    public function __construct(
        protected string $host,
        protected int $port = 80,
        string $uri = '',
        protected string $scheme = 'http',
    ) {
        $this->uri = ($uri === '' || str_starts_with($uri, '/')) ? $uri : '/' . $uri;
    }

    public function setTimeoutSecs(?float $timeout): void
    {
        $this->timeout = $timeout;
    }

    public function setConnectionTimeoutSecs(?float $connectionTimeout): void
    {
        $this->connectionTimeout = $connectionTimeout;
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
        $this->closeCurlHandle();
        $this->request = '';
        $this->response = null;
        $this->responsePos = 0;
    }

    /**
     * @throws TTransportException if cannot read any more data
     */
    public function read(int $len): string
    {
        $response = (string) $this->response;
        if ($len >= strlen($response) - $this->responsePos) {
            return substr($response, $this->responsePos);
        }

        $ret = substr($response, $this->responsePos, $len);
        $this->responsePos += strlen($ret);

        return $ret;
    }

    /**
     * Guarantees that the full amount of data is read. Since TCurlClient gets entire payload at
     * once, parent readAll cannot be used.
     *
     * @throws TTransportException if cannot read data
     */
    public function readAll(int $len): string
    {
        $data = $this->read($len);

        if (strlen($data) !== $len) {
            throw new TTransportException('TCurlClient could not read ' . $len . ' bytes');
        }

        return $data;
    }

    /**
     * @throws TTransportException if writing fails
     */
    public function write(string $buf): void
    {
        $this->request .= $buf;
    }

    /**
     * Opens and sends the actual request over the HTTP connection
     *
     * @throws TTransportException if a writing error occurs
     */
    public function flush(): void
    {
        if (!$this->curlHandle) {
            $this->curlHandle = curl_init();
            curl_setopt($this->curlHandle, CURLOPT_RETURNTRANSFER, true);
            curl_setopt($this->curlHandle, CURLOPT_USERAGENT, 'PHP/TCurlClient');
            curl_setopt($this->curlHandle, CURLOPT_CUSTOMREQUEST, 'POST');
            // curl follows no redirect; flush() follows one itself, within the origin of the URL.
            curl_setopt($this->curlHandle, CURLOPT_FOLLOWLOCATION, false);
        }
        // God, PHP really has some esoteric ways of doing simple things.
        $host = $this->host . ($this->port != 80 ? ':' . $this->port : '');
        $origin = $this->scheme . "://" . $host;
        $fullUrl = $origin . $this->uri;

        $headers = [];
        $defaultHeaders = [
            'Accept' => 'application/x-thrift',
            'Content-Type' => 'application/x-thrift',
            'Content-Length' => strlen($this->request)
        ];
        foreach (array_merge($defaultHeaders, $this->headers) as $key => $value) {
            $headers[] = "$key: $value";
        }

        curl_setopt($this->curlHandle, CURLOPT_HTTPHEADER, $headers);

        if ($this->timeout > 0) {
            if ($this->timeout < 1.0) {
                // Timestamps smaller than 1 second are ignored when CURLOPT_TIMEOUT is used
                curl_setopt($this->curlHandle, CURLOPT_TIMEOUT_MS, 1000 * $this->timeout);
            } else {
                curl_setopt($this->curlHandle, CURLOPT_TIMEOUT, $this->timeout);
            }
        }
        if ($this->connectionTimeout > 0) {
            if ($this->connectionTimeout < 1.0) {
                // Timestamps smaller than 1 second are ignored when CURLOPT_CONNECTTIMEOUT is used
                curl_setopt($this->curlHandle, CURLOPT_CONNECTTIMEOUT_MS, 1000 * $this->connectionTimeout);
            } else {
                curl_setopt($this->curlHandle, CURLOPT_CONNECTTIMEOUT, $this->connectionTimeout);
            }
        }
        curl_setopt($this->curlHandle, CURLOPT_POSTFIELDS, $this->request);
        $this->request = '';

        curl_setopt($this->curlHandle, CURLOPT_URL, $fullUrl);
        $this->response = curl_exec($this->curlHandle);
        $code = curl_getinfo($this->curlHandle, CURLINFO_HTTP_CODE);

        // Follow one redirect, and only within the origin of the URL: the request goes
        // out again, with its headers and body, to the new path and query.
        if ($this->response !== false && $code >= 300 && $code < 400) {
            $redirectUrl = self::redirectWithinOrigin($origin, curl_getinfo($this->curlHandle, CURLINFO_REDIRECT_URL));
            if ($redirectUrl !== null) {
                $fullUrl = $redirectUrl;
                curl_setopt($this->curlHandle, CURLOPT_URL, $fullUrl);
                $this->response = curl_exec($this->curlHandle);
                $code = curl_getinfo($this->curlHandle, CURLINFO_HTTP_CODE);
            }
        }
        $this->responsePos = 0;
        $responseError = curl_error($this->curlHandle);

        // Handle non 200 status code / connect failure
        if ($this->response === false || $code !== 200) {
            $this->curlHandle = null;
            $this->response = null;
            $error = 'TCurlClient: Could not connect to ' . $fullUrl;
            if ($responseError) {
                $error .= ', ' . $responseError;
            }
            if ($code) {
                $error .= ', HTTP status code: ' . $code;
            }
            throw new TTransportException($error, TTransportException::UNKNOWN);
        }
    }

    public function closeCurlHandle(): void
    {
        // Dropping the reference frees the handle. curl_close() has had no effect
        // since PHP 8.0, and PHP 8.5 deprecates it.
        $this->curlHandle = null;
    }

    /**
     * @param array<string, string|int> $headers
     */
    public function addHeaders(array $headers): void
    {
        $this->headers = array_merge($this->headers, $headers);
    }

    /**
     * The URL to send the request to again after a redirect to $location: the
     * path and query of $location, under $origin. Null when $location is not a
     * URL with the scheme, host and port of $origin.
     */
    private static function redirectWithinOrigin(string $origin, mixed $location): ?string
    {
        $target = is_string($location) ? self::originOf($location) : null;
        if ($target === null || $target !== self::originOf($origin)) {
            return null;
        }
        $path = parse_url($location, PHP_URL_PATH);
        $query = parse_url($location, PHP_URL_QUERY);

        return $origin . (is_string($path) ? $path : '/') . (is_string($query) ? '?' . $query : '');
    }

    /**
     * The scheme, host and port of $url, with the port the scheme implies when
     * $url names none. Null when $url has no scheme or host.
     *
     * @return array{string, string, int|null}|null
     */
    private static function originOf(string $url): ?array
    {
        $parts = parse_url($url);
        if (!is_array($parts) || !isset($parts['scheme'], $parts['host'])) {
            return null;
        }
        $scheme = strtolower($parts['scheme']);

        return [$scheme, strtolower($parts['host']), $parts['port'] ?? self::DEFAULT_PORTS[$scheme] ?? null];
    }
}
