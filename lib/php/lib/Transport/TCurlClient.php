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

namespace Thrift\Transport;

use Thrift\Exception\TTransportException;

/**
 * HTTP client for Thrift
 *
 * @package thrift.transport
 */
class TCurlClient extends TTransport
{
    /** @var \CurlHandle|null */
    private static $curlHandle;

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
     * Read timeout
     *
     * @var float|int|null
     */
    protected $timeout = null;

    /**
     * Connection timeout
     *
     * @var float|int|null
     */
    protected $connectionTimeout = null;

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

    /**
     * Set read timeout
     *
     * @param float $timeout
     */
    public function setTimeoutSecs($timeout)
    {
        $this->timeout = $timeout;
    }

    /**
     * Set connection timeout
     *
     * @param float $connectionTimeout
     */
    public function setConnectionTimeoutSecs($connectionTimeout)
    {
        $this->connectionTimeout = $connectionTimeout;
    }

    /**
     * Whether this transport is open.
     *
     * @return boolean true if open
     */
    public function isOpen()
    {
        return true;
    }

    /**
     * Open the transport for reading/writing
     *
     * @throws TTransportException if cannot open
     */
    public function open()
    {
    }

    /**
     * Close the transport.
     */
    public function close()
    {
        $this->request = '';
        $this->response = null;
    }

    /**
     * Read some data into the array.
     *
     * @param int $len How much to read
     * @return string The data that has been read
     * @throws TTransportException if cannot read any more data
     */
    public function read($len)
    {
        if ($len >= strlen($this->response)) {
            return $this->response;
        } else {
            $ret = substr($this->response, 0, $len);
            $this->response = substr($this->response, $len);

            return $ret;
        }
    }

    /**
     * Guarantees that the full amount of data is read. Since TCurlClient gets entire payload at
     * once, parent readAll cannot be used.
     *
     * @return string The data, of exact length
     * @throws TTransportException if cannot read data
     */
    public function readAll($len)
    {
        $data = $this->read($len);

        if (strlen($data) !== $len) {
            throw new TTransportException('TCurlClient could not read ' . $len . ' bytes');
        }

        return $data;
    }

    /**
     * Writes some data into the pending buffer
     *
     * @param string $buf The data to write
     * @throws TTransportException if writing fails
     */
    public function write($buf)
    {
        $this->request .= $buf;
    }

    /**
     * Opens and sends the actual request over the HTTP connection
     *
     * @throws TTransportException if a writing error occurs
     */
    public function flush()
    {
        if (!self::$curlHandle) {
            register_shutdown_function(['Thrift\\Transport\\TCurlClient', 'closeCurlHandle']);
            self::$curlHandle = curl_init();
            curl_setopt(self::$curlHandle, CURLOPT_RETURNTRANSFER, true);
            curl_setopt(self::$curlHandle, CURLOPT_USERAGENT, 'PHP/TCurlClient');
            curl_setopt(self::$curlHandle, CURLOPT_CUSTOMREQUEST, 'POST');
            curl_setopt(self::$curlHandle, CURLOPT_FOLLOWLOCATION, true);
            curl_setopt(self::$curlHandle, CURLOPT_MAXREDIRS, 1);
        }
        // God, PHP really has some esoteric ways of doing simple things.
        $host = $this->host . ($this->port != 80 ? ':' . $this->port : '');
        $fullUrl = $this->scheme . "://" . $host . $this->uri;

        $headers = [];
        $defaultHeaders = [
            'Accept' => 'application/x-thrift',
            'Content-Type' => 'application/x-thrift',
            'Content-Length' => strlen($this->request)
        ];
        foreach (array_merge($defaultHeaders, $this->headers) as $key => $value) {
            $headers[] = "$key: $value";
        }

        curl_setopt(self::$curlHandle, CURLOPT_HTTPHEADER, $headers);

        if ($this->timeout > 0) {
            if ($this->timeout < 1.0) {
                // Timestamps smaller than 1 second are ignored when CURLOPT_TIMEOUT is used
                curl_setopt(self::$curlHandle, CURLOPT_TIMEOUT_MS, 1000 * $this->timeout);
            } else {
                curl_setopt(self::$curlHandle, CURLOPT_TIMEOUT, $this->timeout);
            }
        }
        if ($this->connectionTimeout > 0) {
            if ($this->connectionTimeout < 1.0) {
                // Timestamps smaller than 1 second are ignored when CURLOPT_CONNECTTIMEOUT is used
                curl_setopt(self::$curlHandle, CURLOPT_CONNECTTIMEOUT_MS, 1000 * $this->connectionTimeout);
            } else {
                curl_setopt(self::$curlHandle, CURLOPT_CONNECTTIMEOUT, $this->connectionTimeout);
            }
        }
        curl_setopt(self::$curlHandle, CURLOPT_POSTFIELDS, $this->request);
        $this->request = '';

        curl_setopt(self::$curlHandle, CURLOPT_URL, $fullUrl);
        $this->response = curl_exec(self::$curlHandle);
        $responseError = curl_error(self::$curlHandle);

        $code = curl_getinfo(self::$curlHandle, CURLINFO_HTTP_CODE);

        // Handle non 200 status code / connect failure
        if ($this->response === false || $code !== 200) {
            curl_close(self::$curlHandle);
            self::$curlHandle = null;
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

    public static function closeCurlHandle()
    {
        try {
            if (self::$curlHandle) {
                // This function has no effect. Prior to PHP 8.0.0,
                // this function was used to close the resource.
                curl_close(self::$curlHandle);
                self::$curlHandle = null;
            }
        } catch (\Exception $x) {
            #it's not possible to throw an exception by calling a function that has no effect
            error_log('There was an error closing the curl handle: ' . $x->getMessage());
        }
    }

    public function addHeaders($headers)
    {
        $this->headers = array_merge($this->headers, $headers);
    }
}
