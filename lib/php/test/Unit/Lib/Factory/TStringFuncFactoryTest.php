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

namespace Test\Thrift\Unit\Lib\Factory;

use phpmock\phpunit\PHPMock;
use PHPUnit\Framework\TestCase;
use Thrift\Factory\TStringFuncFactory;
use Thrift\StringFunc\Core;
use Thrift\StringFunc\Mbstring;
use Thrift\StringFunc\TStringFunc;

class TStringFuncFactoryTest extends TestCase
{
    use PHPMock;

    /**
     * @dataProvider createDataProvider
     */
    public function testCreate(
        $mbstringFuncOverload,
        $expectedClass
    ) {
        $this->getFunctionMock('Thrift\Factory', 'ini_get')
             ->expects($this->once())
             ->with('mbstring.func_overload')
             ->willReturn($mbstringFuncOverload);

        $factory = new TStringFuncFactory();
        /**
         * it is a hack to nullable the instance of TStringFuncFactory, and get a new instance based on the new ini_get value
         */
        $ref = new \ReflectionClass($factory);
        $refInstance = $ref->getProperty('_instance');
        $refInstance->setAccessible(true);
        $refInstance->setValue($factory, null);

        $stringFunc = $factory::create();

        $this->assertInstanceOf(TStringFunc::class, $stringFunc);
        $this->assertInstanceOf($expectedClass, $stringFunc);
    }

    public function createDataProvider()
    {
        yield 'mbstring' => [
            'mbstring.func_overload' => 2,
            'expected' => Mbstring::class
        ];

        yield 'string' => [
            'mbstring.func_overload' => 0,
            'expected' => Core::class
        ];
    }
}
