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
 * @package thrift.protocol
 */

namespace Thrift\Factory;

use Thrift\Protocol\TBinaryProtocol;
use Thrift\Transport\TTransport;

/**
 * Binary Protocol Factory
 */
class TBinaryProtocolFactory implements TProtocolFactory
{
    /**
     * @var bool
     */
    private $strictRead_ = false;
    /**
     * @var bool
     */
    private $strictWrite_ = false;

    /**
     * @param bool $strictRead
     * @param bool $strictWrite
     */
    public function __construct($strictRead = false, $strictWrite = false)
    {
        $this->strictRead_ = $strictRead;
        $this->strictWrite_ = $strictWrite;
    }

    /**
     * @param TTransport $trans
     * @return TBinaryProtocol
     */
    public function getProtocol($trans)
    {
        return new TBinaryProtocol($trans, $this->strictRead_, $this->strictWrite_);
    }
}
