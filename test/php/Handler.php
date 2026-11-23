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

use Thrift\Exception\TException;
use ThriftTest\Insanity;
use ThriftTest\Numberz;
use ThriftTest\ThriftTestIf;
use ThriftTest\Xception;
use ThriftTest\Xception2;
use ThriftTest\Xtruct;
use ThriftTest\Xtruct2;

class Handler implements ThriftTestIf
{
    public function testVoid(): void
    {
    }

    public function testString(?string $thing): ?string
    {
        return $thing;
    }

    public function testBool(?bool $thing): ?bool
    {
        return $thing;
    }

    public function testByte(?int $thing): ?int
    {
        return $thing;
    }

    public function testI32(?int $thing): ?int
    {
        return $thing;
    }

    public function testI64(?int $thing): ?int
    {
        return $thing;
    }

    public function testDouble(?float $thing): ?float
    {
        return $thing;
    }

    public function testBinary(?string $thing): ?string
    {
        return $thing;
    }

    public function testUuid(?string $thing): ?string
    {
        return $thing;
    }

    public function testStruct(?Xtruct $thing): ?Xtruct
    {
        return $thing;
    }

    public function testNest(?Xtruct2 $thing): ?Xtruct2
    {
        return $thing;
    }

    public function testMap(?array $thing): ?array
    {
        return $thing;
    }

    public function testStringMap(?array $thing): ?array
    {
        return $thing;
    }

    public function testSet(?array $thing): ?array
    {
        return $thing;
    }

    public function testList(?array $thing): ?array
    {
        return $thing;
    }

    public function testEnum(?int $thing): ?int
    {
        return $thing;
    }

    public function testTypedef(?int $thing): ?int
    {
        return $thing;
    }

    public function testMapMap(?int $hello): ?array
    {
        return [
            -4 => [
                -4 => -4,
                -3 => -3,
                -2 => -2,
                -1 => -1,
            ],
            4 => [
                4 => 4,
                3 => 3,
                2 => 2,
                1 => 1,
            ],
        ];
    }

    public function testInsanity(?Insanity $argument): ?array
    {
        $looney = new Insanity();

        return [
            1 => [
                Numberz::TWO => $argument,
                Numberz::THREE => $argument,
            ],
            2 => [
                Numberz::SIX => $looney,
            ],
        ];
    }

    public function testMulti(
        ?int $arg0,
        ?int $arg1,
        ?int $arg2,
        ?array $arg3,
        ?int $arg4,
        ?int $arg5
    ): ?Xtruct {
        $result = new Xtruct();
        $result->string_thing = 'Hello2';
        $result->byte_thing = $arg0;
        $result->i32_thing = $arg1;
        $result->i64_thing = $arg2;

        return $result;
    }

    public function testException(?string $arg): void
    {
        if ($arg === 'Xception') {
            $exception = new Xception();
            $exception->errorCode = 1001;
            $exception->message = $arg;
            throw $exception;
        }

        if ($arg === 'TException') {
            throw new TException('This is a TException');
        }
    }

    public function testMultiException(?string $arg0, ?string $arg1): ?Xtruct
    {
        if ($arg0 === 'Xception') {
            $exception = new Xception();
            $exception->errorCode = 1001;
            $exception->message = 'This is an Xception';
            throw $exception;
        }

        if ($arg0 === 'Xception2') {
            $exception = new Xception2();
            $exception->errorCode = 2002;
            $exception->struct_thing = new Xtruct();
            $exception->struct_thing->string_thing = 'This is an Xception2';
            throw $exception;
        }

        $result = new Xtruct();
        $result->string_thing = $arg1;

        return $result;
    }

    public function testOneway(?int $secondsToSleep): void
    {
        // Keep the oneway test quick so the cross-test matrix measures fire-and-forget behavior,
        // not a full second of handler blocking in the single-threaded PHP test server.
        usleep((int)$secondsToSleep * 300000);
    }
}
