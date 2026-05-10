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

use Thrift\Exception\TException;

/**
 * Php stream transport. Reads to and writes from the php standard streams
 * php://input and php://output
 *
 * @package thrift.transport
 */
class TPhpStream extends TTransport
{
    public const MODE_R = 1;
    public const MODE_W = 2;

    /** @var resource|null */
    private $inStream = null;

    /** @var resource|null */
    private $outStream = null;

    private bool $read = false;

    private bool $write = false;

    public function __construct($mode)
    {
        $this->read = $mode & self::MODE_R;
        $this->write = $mode & self::MODE_W;
    }

    public function open()
    {
        if ($this->read) {
            $this->inStream = @fopen($this->inStreamName(), 'r');
            if (!is_resource($this->inStream)) {
                throw new TException('TPhpStream: Could not open php://input');
            }
        }
        if ($this->write) {
            $this->outStream = @fopen('php://output', 'w');
            if (!is_resource($this->outStream)) {
                throw new TException('TPhpStream: Could not open php://output');
            }
        }
    }

    public function close()
    {
        if ($this->read) {
            @fclose($this->inStream);
            $this->inStream = null;
        }
        if ($this->write) {
            @fclose($this->outStream);
            $this->outStream = null;
        }
    }

    public function isOpen()
    {
        return
            (!$this->read || is_resource($this->inStream)) &&
            (!$this->write || is_resource($this->outStream));
    }

    public function read($len)
    {
        $data = @fread($this->inStream, $len);
        if ($data === false || $data === '') {
            throw new TException('TPhpStream: Could not read ' . $len . ' bytes');
        }

        return $data;
    }

    public function write($buf)
    {
        while (strlen($buf) > 0) {
            $got = @fwrite($this->outStream, $buf);
            if ($got === 0 || $got === false) {
                throw new TException(
                    'TPhpStream: Could not write ' . strlen($buf) . ' bytes'
                );
            }
            $buf = substr($buf, $got);
        }
    }

    public function flush()
    {
        @fflush($this->outStream);
    }

    private function inStreamName()
    {
        if (php_sapi_name() == 'cli') {
            return 'php://stdin';
        }

        return 'php://input';
    }
}
