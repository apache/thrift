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

/**
 * Framed transport. Writes and reads data in chunks that are stamped with
 * their length.
 *
 * @package thrift.transport
 */
class TFramedTransport extends TTransport
{
    /**
     * Underlying transport object.
     */
    private ?TTransport $transport;

    /**
     * Buffer for read data.
     */
    private string $rBuf = '';

    /**
     * Buffer for queued output data
     */
    private string $wBuf = '';

    /**
     * Whether to frame reads
     */
    private bool $read;

    /**
     * Whether to frame writes
     */
    private bool $write;

    /**
     * Constructor.
     *
     * @param TTransport $transport Underlying transport
     */
    public function __construct($transport = null, $read = true, $write = true)
    {
        $this->transport = $transport;
        $this->read = $read;
        $this->write = $write;
    }

    public function isOpen()
    {
        return $this->transport->isOpen();
    }

    public function open()
    {
        $this->transport->open();
    }

    public function close()
    {
        $this->transport->close();
    }

    /**
     * Reads from the buffer. When more data is required reads another entire
     * chunk and serves future reads out of that.
     *
     * @param int $len How much data
     */
    public function read($len)
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

    /**
     * Put previously read data back into the buffer
     *
     * @param string $data data to return
     */
    public function putBack($data)
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
    private function readFrame()
    {
        $buf = $this->transport->readAll(4);
        $val = unpack('N', $buf);
        $sz = $val[1];

        $this->rBuf = $this->transport->readAll($sz);
    }

    /**
     * Writes some data to the pending output buffer.
     *
     * @param string $buf The data
     * @param int $len Limit of bytes to write
     */
    public function write($buf, $len = null)
    {
        if (!$this->write) {
            return $this->transport->write($buf, $len);
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
    public function flush()
    {
        if (!$this->write || strlen($this->wBuf) == 0) {
            return $this->transport->flush();
        }

        $out = pack('N', strlen($this->wBuf));
        $out .= $this->wBuf;

        // Note that we clear the internal wBuf_ prior to the underlying write
        // to ensure we're in a sane state (i.e. internal buffer cleaned)
        // if the underlying write throws up an exception
        $this->wBuf = '';
        $this->transport->write($out);
        $this->transport->flush();
    }
}
