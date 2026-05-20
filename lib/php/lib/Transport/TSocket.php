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

use Closure;
use Psr\Log\LoggerInterface;
use Psr\Log\LogLevel;
use Psr\Log\NullLogger;
use Thrift\Exception\TException;
use Thrift\Exception\TTransportException;

/**
 * Sockets implementation of the TTransport interface.
 *
 * @package thrift.transport
 */
class TSocket extends TTransport
{
    /**
     * Default debug handler used when none is supplied to the constructor.
     *
     * @deprecated Callable / function-name debug handlers are deprecated and
     *             will be removed in the next version. Pass a
     *             Psr\Log\LoggerInterface to the constructor instead.
     */
    public const DEFAULT_DEBUG_HANDLER = 'error_log';

    private static ?bool $hasSocketsExtension = null;

    /**
     * Handle to PHP socket
     *
     * @var resource|null
     */
    protected $handle = null;

    /**
     * Send timeout in seconds.
     *
     * Combined with sendTimeoutUsec this is used for send timeouts.
     */
    protected int $sendTimeoutSec = 0;

    /**
     * Send timeout in microseconds.
     *
     * Combined with sendTimeoutSec this is used for send timeouts.
     */
    protected int $sendTimeoutUsec = 100000;

    /**
     * Recv timeout in seconds
     *
     * Combined with recvTimeoutUsec this is used for recv timeouts.
     */
    protected int $recvTimeoutSec = 0;

    /**
     * Recv timeout in microseconds
     *
     * Combined with recvTimeoutSec this is used for recv timeouts.
     */
    protected int $recvTimeoutUsec = 750000;

    /**
     * Debugging on? Gates the legacy callable $debugHandler. Has no effect
     * when a Psr\Log\LoggerInterface is used — that path is always invoked
     * and the logger is responsible for level filtering.
     *
     * @deprecated Used only with the legacy callable $debugHandler, which
     *             is itself deprecated. Will be removed alongside it in
     *             the next version.
     */
    protected bool $debug = false;

    /**
     * PSR-3 logger used for diagnostic output. Defaults to a NullLogger so the
     * transport is silent unless the caller supplies a real logger.
     */
    protected LoggerInterface $logger;

    /**
     * Legacy debug callback. Only populated when the caller passed a callable
     * (or, deprecated, a function-name string) instead of a LoggerInterface.
     */
    protected ?Closure $debugHandler = null;

    /**
     * Socket constructor
     *
     * @param LoggerInterface|callable|string|null $debugHandler PSR-3 logger
     *        for diagnostic output. Passing a callable or function-name string
     *        is deprecated and triggers E_USER_DEPRECATED; pass a
     *        Psr\Log\LoggerInterface instead.
     */
    public function __construct(
        protected string $host = 'localhost',
        protected int $port = 9090,
        protected bool $persist = false,
        LoggerInterface|callable|string|null $debugHandler = null,
    ) {
        if ($debugHandler instanceof LoggerInterface) {
            $this->logger = $debugHandler;
            return;
        }

        $this->logger = new NullLogger();

        if ($debugHandler === null) {
            return;
        }

        trigger_error(
            'Passing a callable as $debugHandler is deprecated and will be '
            . 'removed in the next version; pass a Psr\\Log\\LoggerInterface '
            . 'instead.',
            E_USER_DEPRECATED,
        );

        $this->debugHandler = Closure::fromCallable($debugHandler);
    }

    /**
     * @param resource $handle
     */
    public function setHandle($handle): void
    {
        $this->handle = $handle;
        stream_set_blocking($this->handle, false);
    }

    /**
     * @param int $timeout Timeout in milliseconds.
     */
    public function setSendTimeout(int $timeout): void
    {
        $this->sendTimeoutSec = intdiv($timeout, 1000);
        $this->sendTimeoutUsec =
            ($timeout - ($this->sendTimeoutSec * 1000)) * 1000;
    }

    /**
     * @param int $timeout Timeout in milliseconds.
     */
    public function setRecvTimeout(int $timeout): void
    {
        $this->recvTimeoutSec = intdiv($timeout, 1000);
        $this->recvTimeoutUsec =
            ($timeout - ($this->recvTimeoutSec * 1000)) * 1000;
    }

    /**
     * Enables or disables emission via the legacy callable $debugHandler.
     * Has no effect when a Psr\Log\LoggerInterface is in use — configure
     * the logger's level filter instead.
     *
     * @deprecated The full LoggerInterface migration is planned for the
     *             next version, at which point this gate becomes
     *             redundant and will be removed. Pass a configured
     *             Psr\Log\LoggerInterface to the constructor instead.
     */
    public function setDebug(bool $debug): void
    {
        trigger_error(
            __METHOD__ . '() is deprecated; pass a Psr\\Log\\LoggerInterface '
            . 'to the constructor and let the logger filter by level. This '
            . 'method will be removed in the next version.',
            E_USER_DEPRECATED,
        );

        $this->debug = $debug;
    }

    /**
     * Dispatches a diagnostic message.
     *
     * - Legacy callable $debugHandler: gated by setDebug() for BC.
     * - User-supplied Psr\Log\LoggerInterface: always invoked; the logger
     *   filters by level.
     * - No handler supplied (default NullLogger): falls back to PHP's
     *   error_log() when setDebug(true) is in effect, matching master.
     */
    protected function log(string $level, string $message): void
    {
        if ($this->debugHandler !== null) {
            if (!$this->debug) {
                return;
            }
            ($this->debugHandler)($message);
            return;
        }

        if (!($this->logger instanceof NullLogger)) {
            $this->logger->log($level, $message);
            return;
        }

        if ($this->debug) {
            error_log($message);
        }
    }

    public function getHost(): string
    {
        return $this->host;
    }

    public function getPort(): int
    {
        return $this->port;
    }

    public function isOpen(): bool
    {
        return is_resource($this->handle);
    }

    /**
     * Connects the socket.
     */
    public function open(): void
    {
        if ($this->isOpen()) {
            throw new TTransportException('Socket already connected', TTransportException::ALREADY_OPEN);
        }

        if (empty($this->host)) {
            throw new TTransportException('Cannot open null host', TTransportException::NOT_OPEN);
        }

        if ($this->port <= 0 && strpos($this->host, 'unix://') !== 0) {
            throw new TTransportException('Cannot open without port', TTransportException::NOT_OPEN);
        }

        if ($this->persist) {
            $this->handle = @pfsockopen(
                $this->host,
                $this->port,
                $errno,
                $errstr,
                $this->sendTimeoutSec + ($this->sendTimeoutUsec / 1000000)
            );
        } else {
            $this->handle = @fsockopen(
                $this->host,
                $this->port,
                $errno,
                $errstr,
                $this->sendTimeoutSec + ($this->sendTimeoutUsec / 1000000)
            );
        }

        // Connect failed?
        if ($this->handle === false) {
            $error = 'TSocket: Could not connect to ' .
                $this->host . ':' . $this->port . ' (' . $errstr . ' [' . $errno . '])';
            $this->log(LogLevel::ERROR, $error);
            throw new TException($error);
        }

        if (self::hasSocketsExtension()) {
            // warnings silenced due to bug https://bugs.php.net/bug.php?id=70939
            $socket = socket_import_stream($this->handle);
            if ($socket !== false) {
                @socket_set_option($socket, SOL_TCP, TCP_NODELAY, 1);
            }
        }
    }

    private static function hasSocketsExtension(): bool
    {
        return self::$hasSocketsExtension ??=
            function_exists('socket_import_stream') && function_exists('socket_set_option');
    }

    public function close(): void
    {
        @fclose($this->handle);
        $this->handle = null;
    }

    /**
     * Read from the socket at most $len bytes.
     *
     * This method will not wait for all the requested data, it will return as
     * soon as any data is received.
     */
    public function read(int $len): string
    {
        $null = null;
        $read = [$this->handle];
        $readable = @stream_select(
            $read,
            $null,
            $null,
            $this->recvTimeoutSec,
            $this->recvTimeoutUsec
        );

        if ($readable > 0) {
            $data = fread($this->handle, $len);
            if ($data === false) {
                throw new TTransportException('TSocket: Could not read ' . $len . ' bytes from ' .
                    $this->host . ':' . $this->port);
            } elseif ($data == '' && feof($this->handle)) {
                throw new TTransportException('TSocket read 0 bytes');
            }

            return $data;
        } elseif ($readable === 0) {
            throw new TTransportException('TSocket: timed out reading ' . $len . ' bytes from ' .
                $this->host . ':' . $this->port);
        } else {
            throw new TTransportException('TSocket: Could not read ' . $len . ' bytes from ' .
                $this->host . ':' . $this->port);
        }
    }

    /**
     * Write to the socket.
     */
    public function write(string $buf): void
    {
        $null = null;
        $write = [$this->handle];

        // keep writing until all the data has been written
        while (strlen($buf) > 0) {
            // wait for stream to become available for writing
            $writable = @stream_select(
                $null,
                $write,
                $null,
                $this->sendTimeoutSec,
                $this->sendTimeoutUsec
            );
            if ($writable > 0) {
                // write buffer to stream
                $written = fwrite($this->handle, $buf);
                $closed_socket = $written === 0 && feof($this->handle);
                if ($written === false || $closed_socket) {
                    throw new TTransportException(
                        'TSocket: Could not write ' . strlen($buf) . ' bytes ' .
                        $this->host . ':' . $this->port
                    );
                }
                // determine how much of the buffer is left to write
                $buf = substr($buf, $written);
            } elseif ($writable === 0) {
                throw new TTransportException(
                    'TSocket: timed out writing ' . strlen($buf) . ' bytes from ' .
                    $this->host . ':' . $this->port
                );
            } else {
                throw new TTransportException(
                    'TSocket: Could not write ' . strlen($buf) . ' bytes ' .
                    $this->host . ':' . $this->port
                );
            }
        }
    }

    /**
     * Since read(), readAll() and write() operate on the sockets directly,
     * this is a no-op. Wrap this TSocket in a TBufferedTransport if you
     * need flushable buffering.
     */
    public function flush(): void
    {
    }
}
