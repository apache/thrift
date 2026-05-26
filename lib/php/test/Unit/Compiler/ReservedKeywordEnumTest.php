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

declare(strict_types=1);

namespace Test\Thrift\Unit\Compiler;

use PHPUnit\Framework\TestCase;

/**
 * Regression for THRIFT-1209: PHP-reserved identifiers (GLOBAL, STATIC, LIST,
 * RETURN) must round-trip through the generator as `public const` declarations
 * on the emitted enum class. PHP 7.1+ permits reserved identifiers as class
 * constants (except `class`); historically the generator emitted code that
 * triggered a parse error here.
 *
 * The matching `ReservedKeywordEnum` is declared in
 * lib/php/test/Resources/ThriftTest.thrift and generated into
 * lib/php/test/Resources/packages/php by the build before phpunit runs.
 */
class ReservedKeywordEnumTest extends TestCase
{
    public function testReservedKeywordsRoundTripAsClassConstants(): void
    {
        $this->assertSame(1, \Basic\TestValidators\ReservedKeywordEnum::GLOBAL);
        $this->assertSame(2, \Basic\TestValidators\ReservedKeywordEnum::STATIC);
        $this->assertSame(3, \Basic\TestValidators\ReservedKeywordEnum::LIST);
        $this->assertSame(4, \Basic\TestValidators\ReservedKeywordEnum::RETURN);
    }
}
