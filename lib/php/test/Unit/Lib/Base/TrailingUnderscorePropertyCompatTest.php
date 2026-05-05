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

namespace Test\Thrift\Unit\Lib\Base;

use PHPUnit\Framework\TestCase;
use Thrift\Base\TrailingUnderscorePropertyCompat;

class TrailingUnderscorePropertyCompatFixture
{
    use TrailingUnderscorePropertyCompat;

    public $publicProp = 'public-default';
    protected $protectedProp = 'protected-default';
    private $privateProp = 'private-default';
}

class TrailingUnderscorePropertyCompatTest extends TestCase
{
    private TrailingUnderscorePropertyCompatFixture $fixture;

    protected function setUp(): void
    {
        $this->fixture = new TrailingUnderscorePropertyCompatFixture();
    }

    public function testReadingTrailingUnderscoreReturnsRealPropertyValueAndTriggersDeprecation(): void
    {
        $deprecation = null;
        set_error_handler(static function (int $errno, string $errstr) use (&$deprecation): bool {
            $deprecation = $errstr;
            return true;
        }, E_USER_DEPRECATED);
        try {
            $value = $this->fixture->publicProp_;
        } finally {
            restore_error_handler();
        }
        $this->assertSame('public-default', $value);
        $this->assertNotNull($deprecation, 'Reading $publicProp_ should trigger E_USER_DEPRECATED');
        $this->assertStringContainsString('publicProp_', $deprecation);
        $this->assertStringContainsString('publicProp', $deprecation);
    }

    public function testReadingProtectedTrailingUnderscoreFromOutsideStillResolves(): void
    {
        $deprecation = null;
        set_error_handler(static function (int $errno, string $errstr) use (&$deprecation): bool {
            $deprecation = $errstr;
            return true;
        }, E_USER_DEPRECATED);
        try {
            $value = $this->fixture->protectedProp_;
        } finally {
            restore_error_handler();
        }
        $this->assertSame('protected-default', $value);
        $this->assertNotNull($deprecation);
    }

    public function testWritingTrailingUnderscoreUpdatesRealPropertyAndTriggersDeprecation(): void
    {
        $deprecation = null;
        set_error_handler(static function (int $errno, string $errstr) use (&$deprecation): bool {
            $deprecation = $errstr;
            return true;
        }, E_USER_DEPRECATED);
        try {
            $this->fixture->publicProp_ = 'new-value';
        } finally {
            restore_error_handler();
        }
        $this->assertSame('new-value', $this->fixture->publicProp);
        $this->assertNotNull($deprecation);
    }

    public function testIssetOnTrailingUnderscoreDoesNotTriggerDeprecation(): void
    {
        $deprecation = null;
        set_error_handler(static function (int $errno, string $errstr) use (&$deprecation): bool {
            $deprecation = $errstr;
            return true;
        }, E_USER_DEPRECATED);
        try {
            $isSet = isset($this->fixture->publicProp_);
        } finally {
            restore_error_handler();
        }
        $this->assertTrue($isSet);
        $this->assertNull($deprecation, 'isset() should not emit deprecation noise');
    }

    public function testNameWithoutTrailingUnderscoreFallsThroughToDefault(): void
    {
        $notice = null;
        set_error_handler(static function (int $errno, string $errstr) use (&$notice): bool {
            $notice = $errstr;
            return true;
        }, E_USER_NOTICE);
        try {
            $value = $this->fixture->doesNotExist;
        } finally {
            restore_error_handler();
        }
        $this->assertNull($value);
        $this->assertNotNull($notice, 'Reading an unknown property should still emit a notice');
        $this->assertStringContainsString('doesNotExist', $notice);
    }

    public function testTrailingUnderscoreThatHasNoUnderlyingPropertyDoesNotShim(): void
    {
        $notice = null;
        $deprecation = null;
        set_error_handler(static function (int $errno, string $errstr) use (&$notice, &$deprecation): bool {
            if ($errno === E_USER_DEPRECATED) {
                $deprecation = $errstr;
            } else {
                $notice = $errstr;
            }
            return true;
        }, E_USER_NOTICE | E_USER_DEPRECATED);
        try {
            $value = $this->fixture->ghost_;
        } finally {
            restore_error_handler();
        }
        $this->assertNull($value);
        $this->assertNull($deprecation, 'No deprecation when no underlying property exists');
        $this->assertNotNull($notice, 'Falls through to undefined-property notice');
    }
}
