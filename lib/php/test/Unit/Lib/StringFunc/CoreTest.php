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
 */

namespace Test\Thrift\Unit\Lib\StringFunc;

use PHPUnit\Framework\TestCase;
use Thrift\StringFunc\Core;

class CoreTest extends TestCase
{
    /**
     * @dataProvider substrDataProvider
     */
    public function testSubstr(
        $expected,
        $str,
        $start = 0,
        $length = null
    ) {
        $core = new Core();
        $this->assertEquals($expected, $core->substr($str, $start, $length));
    }

    /**
     * @dataProvider strlenDataProvider
     */
    public function testStrlen(
        $expectedLength,
        $str
    ) {
        $core = new Core();
        $this->assertEquals($expectedLength, $core->strlen($str));
    }

    public function substrDataProvider()
    {
        yield 'Afrikaans' => [
            'expected' => 'Afrikaans',
            'str' => 'Afrikaans',
        ];
        yield 'Alemannisch' => [
            'expected' => 'Alemannisch',
            'str' => 'Alemannisch',
        ];
        yield 'Aragonés' => [
            'expected' => 'Aragonés',
            'str' => 'Aragonés',
        ];
        yield 'العربية' => [
            'expected' => 'العربية',
            'str' => 'العربية',
        ];
        yield 'مصرى' => [
            'expected' => 'مصرى',
            'str' => 'مصرى',
        ];
        yield 'മലയാളം' => [
            'expected' => 'മലയാളം',
            'str' => 'മലയാളം',
        ];
        yield 'Slovenščina' => [
            'expected' => 'Slovenščina',
            'str' => 'Slovenščina',
        ];
        yield 'Українська' => [
            'expected' => 'Українська',
            'str' => 'Українська',
        ];
        yield 'اردو' => [
            'expected' => 'اردو',
            'str' => 'اردو',
        ];
        yield '中文' => [
            'expected' => '中文',
            'str' => '中文',
        ];
        yield '粵語' => [
            'expected' => '粵語',
            'str' => '粵語',
        ];
        yield 'Afrikaans_SUB' => [
            'expected' => 'rikaan',
            'str' => 'Afrikaans',
            'start' => 2,
            'length' => 6,
        ];
        yield 'Alemannisch_SUB' => [
            'expected' => 'emanni',
            'str' => 'Alemannisch',
            'start' => 2,
            'length' => 6,
        ];
        yield 'Aragonés_SUB' => [
            'expected' => 'agoné',
            'str' => 'Aragonés',
            'start' => 2,
            'length' => 6,
        ];
        yield 'العربية_SUB' => [
            'expected' => 'لعر',
            'str' => 'العربية',
            'start' => 2,
            'length' => 6,
        ];
        yield 'مصرى_SUB' => [
            'expected' => 'صرى',
            'str' => 'مصرى',
            'start' => 2,
            'length' => 6,
        ];
        yield 'മലയാളം_SUB' => [
            'expected' => 'ലയ',
            'str' => 'മലയാളം',
            'start' => 3,
            'length' => 6,
        ];
        yield 'Slovenščina_SUB' => [
            'expected' => 'ovenš',
            'str' => 'Slovenščina',
            'start' => 2,
            'length' => 6,
        ];
        yield 'Українська_SUB' => [
            'expected' => 'кра',
            'str' => 'Українська',
            'start' => 2,
            'length' => 6,
        ];
        yield 'اردو_SUB' => [
            'expected' => 'ردو',
            'str' => 'اردو',
            'start' => 2,
            'length' => 6,
        ];
        yield '中文_SUB' => [
            'expected' => '文',
            'str' => '中文',
            'start' => 3,
            'length' => 3,
        ];
        yield '粵語_SUB' => [
            'expected' => '語',
            'str' => '粵語',
            'start' => 3,
            'length' => 3,
        ];
    }

    public function strlenDataProvider()
    {
        yield 'Afrikaans' => [
            'expectedLength' => 9,
            'str' => 'Afrikaans',
        ];
        yield 'Alemannisch' => [
            'expectedLength' => 11,
            'str' => 'Alemannisch',
        ];
        yield 'Aragonés' => [
            'expectedLength' => 9,
            'str' => 'Aragonés',
        ];
        yield 'العربية' => [
            'expectedLength' => 14,
            'str' => 'العربية',
        ];
        yield 'مصرى' => [
            'expectedLength' => 8,
            'str' => 'مصرى',
        ];
        yield 'മലയാളം' => [
            'expectedLength' => 18,
            'str' => 'മലയാളം',
        ];
        yield 'Slovenščina' => [
            'expectedLength' => 13,
            'str' => 'Slovenščina',
        ];
        yield 'Українська' => [
            'expectedLength' => 20,
            'str' => 'Українська',
        ];
        yield 'اردو' => [
            'expectedLength' => 8,
            'str' => 'اردو',
        ];
        yield '中文' => [
            'expectedLength' => 6,
            'str' => '中文',
        ];
        yield '粵語' => [
            'expectedLength' => 6,
            'str' => '粵語',
        ];
    }
}
