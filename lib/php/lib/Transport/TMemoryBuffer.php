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
 * A memory buffer is a tranpsort that simply reads from and writes to an
 * in-memory string buffer. Anytime you call write on it, the data is simply
 * placed into a buffer, and anytime you call read, data is read from that
 * buffer.
 *
 * @package thrift.transport
 */
class TMemoryBuffer extends TTransport
{
    /**
     * Constructor. Optionally pass an initial value for the buffer.
     */
    public function __construct(protected string $buf = '')
    {
    }

    public function isOpen()
    {
        return true;
    }

    public function open()
    {
    }

    public function close()
    {
    }

    public function write($buf)
    {
        $this->buf .= $buf;
    }

    public function read($len)
    {
        $bufLength = strlen($this->buf);

        if ($bufLength === 0) {
            throw new TTransportException(
                'TMemoryBuffer: Could not read ' .
                $len . ' bytes from buffer.',
                TTransportException::UNKNOWN
            );
        }

        if ($bufLength <= $len) {
            $ret = $this->buf;
            $this->buf = '';

            return $ret;
        }

        $ret = substr($this->buf, 0, $len);
        $this->buf = substr($this->buf, $len);

        return $ret;
    }

    public function getBuffer()
    {
        return $this->buf;
    }

    public function available()
    {
        return strlen($this->buf);
    }

    public function putBack($data)
    {
        $this->buf = $data . $this->buf;
    }
}
