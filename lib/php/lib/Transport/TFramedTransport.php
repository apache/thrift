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

/**
 * Framed transport. Writes and reads data in chunks that are stamped with
 * their length.
 *
 * @package thrift.transport
 */
class TFramedTransport extends TTransport
{
    /**
     * Buffer for read data.
     */
    private string $rBuf = '';

    /**
     * Buffer for queued output data
     */
    private string $wBuf = '';

    public function __construct(
        private TTransport $transport,
        private bool $read = true,
        private bool $write = true,
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

    /**
     * Reads from the buffer. When more data is required reads another entire
     * chunk and serves future reads out of that.
     */
    public function read(int $len): string
    {
        if (!$this->read) {
            return $this->transport->read($len);
        }

        if (strlen($this->rBuf) === 0) {
            $this->readFrame();
        }

        // Just return full buff
        if ($len >= strlen($this->rBuf)) {
            $out = $this->rBuf;
            $this->rBuf = '';

            return $out;
        }

        $out = substr($this->rBuf, 0, $len);
        $this->rBuf = substr($this->rBuf, $len);

        return $out;
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
     * Reads a chunk of data into the internal read buffer.
     */
    private function readFrame(): void
    {
        $buf = $this->transport->readAll(4);
        $val = unpack('N', $buf);
        $sz = $val[1];

        $this->rBuf = $this->transport->readAll($sz);
    }

    /**
     * Writes some data to the pending output buffer. When $len is provided,
     * truncates $buf to at most $len bytes.
     */
    public function write(string $buf, ?int $len = null): void
    {
        if (!$this->write) {
            $this->transport->write($buf);
            return;
        }

        if ($len !== null && $len < strlen($buf)) {
            $buf = substr($buf, 0, $len);
        }
        $this->wBuf .= $buf;
    }

    /**
     * Writes the output buffer to the stream in the format of a 4-byte length
     * followed by the actual data.
     */
    public function flush(): void
    {
        if (!$this->write || strlen($this->wBuf) == 0) {
            $this->transport->flush();
            return;
        }

        $out = pack('N', strlen($this->wBuf));
        $out .= $this->wBuf;

        // Clear the buffer before writing so we stay in a sane state
        // even if the underlying transport throws.
        $this->wBuf = '';
        $this->transport->write($out);
        $this->transport->flush();
    }
}
