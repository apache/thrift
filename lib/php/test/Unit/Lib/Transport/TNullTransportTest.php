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

namespace Test\Thrift\Unit\Lib\Transport;

use PHPUnit\Framework\TestCase;
use Thrift\Exception\TTransportException;
use Thrift\Transport\TNullTransport;

class TNullTransportTest extends TestCase
{
    public function testIsOpen()
    {
        $transport = new TNullTransport();
        $this->assertTrue($transport->isOpen());
    }

    public function testOpen()
    {
        $transport = new TNullTransport();
        $this->assertNull($transport->open());
    }

    public function testClose()
    {
        $transport = new TNullTransport();
        $this->assertNull($transport->close());
    }

    public function testRead()
    {
        $transport = new TNullTransport();
        $this->expectException(TTransportException::class);
        $this->expectExceptionMessage("Can't read from TNullTransport.");
        $this->expectExceptionCode(0);
        $transport->read(1);
    }

    public function testWrite()
    {
        $transport = new TNullTransport();
        $this->assertNull($transport->write('test'));
    }
}
