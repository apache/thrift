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
use Thrift\Exception\TException;

class TExceptionTest extends TestCase
{
    public function testExceptionWithMessageAndCode()
    {
        $message = 'Test exception message';
        $code = 42;

        $exception = new TException($message, $code);

        $this->assertInstanceOf(TException::class, $exception);
        $this->assertSame($message, $exception->getMessage());
        $this->assertSame($code, $exception->getCode());
    }

    public function testExceptionWithSpecAndVals()
    {
        $spec = [
            ['var' => 'string'],
            ['var' => 'int'],
            ['var' => 'bool'],
        ];

        $vals = [
            'string' => 'Test value',
            'int' => 123456,
            'bool' => true,
        ];
        $exception = new TException($spec, $vals);

        $this->assertEquals('Test value', $exception->string);
        $this->assertEquals(123456, $exception->int);
        $this->assertEquals(true, $exception->bool);
    }
}
