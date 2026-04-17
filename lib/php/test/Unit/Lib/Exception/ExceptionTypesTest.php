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
 */

namespace Test\Thrift\Unit\Lib\Exception;

use PHPUnit\Framework\TestCase;
use Thrift\Exception\TProtocolException;
use Thrift\Exception\TTransportException;

class ExceptionTypesTest extends TestCase
{
    public function testProtocolExceptionPreservesMessageAndCode(): void
    {
        $exception = new TProtocolException(
            'invalid payload',
            TProtocolException::BAD_VERSION
        );

        $this->assertSame('invalid payload', $exception->getMessage());
        $this->assertSame(TProtocolException::BAD_VERSION, $exception->getCode());
    }

    public function testTransportExceptionPreservesMessageAndCode(): void
    {
        $exception = new TTransportException(
            'timed out',
            TTransportException::TIMED_OUT
        );

        $this->assertSame('timed out', $exception->getMessage());
        $this->assertSame(TTransportException::TIMED_OUT, $exception->getCode());
    }
}
