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
 * Buffered transport. Stores data to an internal buffer that it doesn't
 * actually write out until flush is called. For reading, we do a greedy
 * read and then serve data out of the internal buffer.
 *
 * @package thrift.transport
 */
class TBufferedTransport extends TTransport
{
    /**
     * The underlying transport
     */
    protected TTransport $transport;

    /**
     * The receive buffer size
     */
    protected int $rBufSize = 512;

    /**
     * The write buffer size
     */
    protected int $wBufSize = 512;

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
    public function __construct($transport, $rBufSize = 512, $wBufSize = 512)
    {
        $this->transport = $transport;
        $this->rBufSize = $rBufSize;
        $this->wBufSize = $wBufSize;
    }

    public function isOpen()
    {
        return $this->transport->isOpen();
    }

    /**
     * @inheritdoc
     *
     * @throws TTransportException
     */
    public function open()
    {
        $this->transport->open();
    }

    public function close()
    {
        $this->transport->close();
    }

    public function putBack($data)
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
     *
     * @throws TTransportException
     */
    public function readAll($len)
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

    /**
     * @inheritdoc
     *
     * @param int $len
     * @return string
     * @throws TTransportException
     */
    public function read($len)
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

    /**
     * @inheritdoc
     *
     * @param string $buf
     * @throws TTransportException
     */
    public function write($buf)
    {
        $this->wBuf .= $buf;
        if (strlen($this->wBuf) >= $this->wBufSize) {
            $out = $this->wBuf;

            // Note that we clear the internal wBuf_ prior to the underlying write
            // to ensure we're in a sane state (i.e. internal buffer cleaned)
            // if the underlying write throws up an exception
            $this->wBuf = '';
            $this->transport->write($out);
        }
    }

    /**
     * @inheritdoc
     *
     * @throws TTransportException
     */
    public function flush()
    {
        if (strlen($this->wBuf) > 0) {
            $out = $this->wBuf;

            // Note that we clear the internal wBuf_ prior to the underlying write
            // to ensure we're in a sane state (i.e. internal buffer cleaned)
            // if the underlying write throws up an exception
            $this->wBuf = '';
            $this->transport->write($out);
        }
        $this->transport->flush();
    }
}
