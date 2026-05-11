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
class THttpClient extends TTransport
{
    /**
     * The URI to request
     */
    protected string $uri;

    /**
     * Buffer for the HTTP request data
     */
    protected string $buf = '';

    /**
     * Input socket stream.
     *
     * @var resource|null
     */
    protected $handle = null;

    /**
     * Read timeout in seconds.
     */
    protected ?float $timeout = null;

    /**
     * http headers
     *
     * @var array<string, string|int>
     */
    protected array $headers = [];

    /**
     * Make a new HTTP client.
     *
     * @param array<string, mixed> $context Context additional options
     */
    public function __construct(
        protected string $host,
        protected int $port = 80,
        string $uri = '',
        protected string $scheme = 'http',
        protected array $context = [],
    ) {
        $this->uri = ($uri === '' || str_starts_with($uri, '/')) ? $uri : '/' . $uri;
    }

    public function setTimeoutSecs(?float $timeout): void
    {
        $this->timeout = $timeout;
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
        if ($this->handle) {
            @fclose($this->handle);
            $this->handle = null;
        }
    }

    /**
     * @throws TTransportException if cannot read any more data
     */
    public function read(int $len): string
    {
        $data = @fread($this->handle, $len);
        if ($data === false || $data === '') {
            $md = stream_get_meta_data($this->handle);
            if ($md['timed_out']) {
                throw new TTransportException(
                    'THttpClient: timed out reading ' . $len . ' bytes from ' .
                    $this->host . ':' . $this->port . $this->uri,
                    TTransportException::TIMED_OUT
                );
            } else {
                throw new TTransportException(
                    'THttpClient: Could not read ' . $len . ' bytes from ' .
                    $this->host . ':' . $this->port . $this->uri,
                    TTransportException::UNKNOWN
                );
            }
        }

        return $data;
    }

    /**
     * @throws TTransportException if writing fails
     */
    public function write(string $buf): void
    {
        $this->buf .= $buf;
    }

    /**
     * Opens and sends the actual request over the HTTP connection
     *
     * @throws TTransportException if a writing error occurs
     */
    public function flush(): void
    {
        // God, PHP really has some esoteric ways of doing simple things.
        $host = $this->host . ($this->port != 80 ? ':' . $this->port : '');

        $headers = [];
        $defaultHeaders = [
            'Host' => $host,
            'Accept' => 'application/x-thrift',
            'User-Agent' => 'PHP/THttpClient',
            'Content-Type' => 'application/x-thrift',
            'Content-Length' => strlen($this->buf)
        ];

        foreach (array_merge($defaultHeaders, $this->headers) as $key => $value) {
            $headers[] = "$key: $value";
        }

        $options = $this->context;

        $baseHttpOptions = isset($options["http"]) ? $options["http"] : [];

        $httpOptions = $baseHttpOptions + [
            'method' => 'POST',
            'header' => implode("\r\n", $headers),
            'max_redirects' => 1,
            'content' => $this->buf
        ];
        if ($this->timeout > 0) {
            $httpOptions['timeout'] = $this->timeout;
        }
        $this->buf = '';

        $options["http"] = $httpOptions;
        $contextid = stream_context_create($options);
        $this->handle = @fopen(
            $this->scheme . '://' . $host . $this->uri,
            'r',
            false,
            $contextid
        );

        // Connect failed?
        if ($this->handle === false) {
            $this->handle = null;
            $error = 'THttpClient: Could not connect to ' . $host . $this->uri;
            throw new TTransportException($error, TTransportException::NOT_OPEN);
        }
    }

    /**
     * @param array<string, string|int> $headers
     */
    public function addHeaders(array $headers): void
    {
        $this->headers = array_merge($this->headers, $headers);
    }
}
