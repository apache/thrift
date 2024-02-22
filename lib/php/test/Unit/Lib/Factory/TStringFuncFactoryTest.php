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

namespace Test\Thrift\Unit\Lib\Factory;

use PHPUnit\Framework\TestCase;
use Thrift\Factory\TStringFuncFactory;
use Thrift\StringFunc\Core;
use Thrift\StringFunc\Mbstring;
use Thrift\StringFunc\TStringFunc;

class TStringFuncFactoryTest extends TestCase
{
    /**
     * @return void
     */
    public function testCreate()
    {
        $factory = new TStringFuncFactory();
        $stringFunc = $factory::create();
        $this->assertInstanceOf(TStringFunc::class, $stringFunc);
        $this->assertInstanceOf(Mbstring::class, $stringFunc);

        /**
         * it is a hack to nullable the instance of TStringFuncFactory, and get a new instance based on the new ini_get value
         */
        $ref = new \ReflectionClass($factory);
        $refInstance = $ref->getProperty('_instance');
        $refInstance->setAccessible(true);
        $refInstance->setValue($factory, null);

        $stringFunc = $factory::create();
        $this->assertInstanceOf(TStringFunc::class, $stringFunc);
        $this->assertInstanceOf(Core::class, $stringFunc);
    }
}


namespace Thrift\Factory;

function ini_get($key)
{
    static $count = 0;
    if ($key === 'mbstring.func_overload') {
        if ($count === 0) {
            $count++;
            return 2;
        } else {
            return 0;
        }
    } else {
        return \ini_get($key);
    }
}
