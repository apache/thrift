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
 * Buffered transport. Stores data to an internal buffer that it doesn't
 * actually write out until flush is called. For reading, we do a greedy
 * read and then serve data out of the internal buffer.
 *
 * @package thrift.transport
 */
class TBufferedTransport extends TTransport
{
    /**
     * The write buffer.
     */
    protected string $wBuf = '';

    /**
     * The read buffer.
     */
    protected string $rBuf = '';

    /**
     * Constructor. Creates a buffered transport around an underlying transport
     */
    public function __construct(
        protected TTransport $transport,
        protected int $rBufSize = 512,
        protected int $wBufSize = 512,
    ) {
    }

    public function isOpen(): bool
    {
        return $this->transport->isOpen();
    }

    public function open(): void
    {
        $this->transport->open();
    }

    public function close(): void
    {
        $this->transport->close();
    }

    public function putBack(string $data): void
    {
        if (strlen($this->rBuf) === 0) {
            $this->rBuf = $data;
        } else {
            $this->rBuf = ($data . $this->rBuf);
        }
    }

    /**
     * The reason that we customize readAll here is that the majority of PHP
     * streams are already internally buffered by PHP. The socket stream, for
     * example, buffers internally and blocks if you call read with $len greater
     * than the amount of data available, unlike recv() in C.
     *
     * Therefore, use the readAll method of the wrapped transport inside
     * the buffered readAll.
     */
    public function readAll(int $len): string
    {
        $have = strlen($this->rBuf);
        if ($have == 0) {
            $data = $this->transport->readAll($len);
        } elseif ($have < $len) {
            $data = $this->rBuf;
            $this->rBuf = '';
            $data .= $this->transport->readAll($len - $have);
        } elseif ($have == $len) {
            $data = $this->rBuf;
            $this->rBuf = '';
        } else {
            $data = substr($this->rBuf, 0, $len);
            $this->rBuf = substr($this->rBuf, $len);
        }

        return $data;
    }

    public function read(int $len): string
    {
        if (strlen($this->rBuf) === 0) {
            $this->rBuf = $this->transport->read($this->rBufSize);
        }

        if (strlen($this->rBuf) <= $len) {
            $ret = $this->rBuf;
            $this->rBuf = '';

            return $ret;
        }

        $ret = substr($this->rBuf, 0, $len);
        $this->rBuf = substr($this->rBuf, $len);

        return $ret;
    }

    public function write(string $buf): void
    {
        $this->wBuf .= $buf;
        if (strlen($this->wBuf) >= $this->wBufSize) {
            $out = $this->wBuf;

            // Clear the buffer before writing so we stay in a sane state
            // even if the underlying transport throws.
            $this->wBuf = '';
            $this->transport->write($out);
        }
    }

    public function flush(): void
    {
        if (strlen($this->wBuf) > 0) {
            $out = $this->wBuf;

            // Clear the buffer before writing so we stay in a sane state
            // even if the underlying transport throws.
            $this->wBuf = '';
            $this->transport->write($out);
        }
        $this->transport->flush();
    }
}
