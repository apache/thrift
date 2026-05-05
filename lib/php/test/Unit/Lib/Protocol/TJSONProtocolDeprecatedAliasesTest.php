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

namespace Test\Thrift\Unit\Lib\Protocol;

use PHPUnit\Framework\Attributes\DataProvider;
use PHPUnit\Framework\TestCase;
use Thrift\Protocol\TJSONProtocol;
use Thrift\Transport\TMemoryBuffer;

/**
 * THRIFT-5959 renamed three public properties on TJSONProtocol from the
 * trailing-underscore form to plain camelCase. Magic accessors keep the
 * old names working (with an E_USER_DEPRECATED notice) for one
 * deprecation cycle. These tests pin that behaviour.
 */
class TJSONProtocolDeprecatedAliasesTest extends TestCase
{
    private TJSONProtocol $protocol;

    protected function setUp(): void
    {
        $this->protocol = new TJSONProtocol(new TMemoryBuffer());
    }

    /**
     * @return iterable<string, array{string, string}>
     */
    public static function aliasProvider(): iterable
    {
        yield 'contextStack' => ['contextStack_', 'contextStack'];
        yield 'context' => ['context_', 'context'];
        yield 'reader' => ['reader_', 'reader'];
    }

    #[DataProvider('aliasProvider')]
    public function testReadingDeprecatedNameReturnsCurrentValue(string $deprecated, string $current): void
    {
        $deprecation = null;
        set_error_handler(static function (int $errno, string $errstr) use (&$deprecation): bool {
            $deprecation = $errstr;
            return true;
        }, E_USER_DEPRECATED);
        try {
            $value = $this->protocol->$deprecated;
        } finally {
            restore_error_handler();
        }
        $this->assertSame($this->protocol->$current, $value);
        $this->assertNotNull($deprecation, 'reading $' . $deprecated . ' must trigger E_USER_DEPRECATED');
        $this->assertStringContainsString($deprecated, $deprecation);
        $this->assertStringContainsString($current, $deprecation);
    }

    #[DataProvider('aliasProvider')]
    public function testWritingDeprecatedNameUpdatesCurrentProperty(string $deprecated, string $current): void
    {
        $sentinel = new \stdClass();
        $deprecation = null;
        set_error_handler(static function (int $errno, string $errstr) use (&$deprecation): bool {
            $deprecation = $errstr;
            return true;
        }, E_USER_DEPRECATED);
        try {
            $this->protocol->$deprecated = $sentinel;
        } finally {
            restore_error_handler();
        }
        $this->assertSame($sentinel, $this->protocol->$current);
        $this->assertNotNull($deprecation);
    }

    #[DataProvider('aliasProvider')]
    public function testIssetDeprecatedNameMatchesCurrent(string $deprecated, string $current): void
    {
        $deprecation = null;
        set_error_handler(static function (int $errno, string $errstr) use (&$deprecation): bool {
            $deprecation = $errstr;
            return true;
        }, E_USER_DEPRECATED);
        try {
            $isSetDeprecated = isset($this->protocol->$deprecated);
        } finally {
            restore_error_handler();
        }
        $this->assertSame(isset($this->protocol->$current), $isSetDeprecated);
        $this->assertNull($deprecation, 'isset() must not emit deprecation noise');
    }

    public function testUnknownPropertyFallsThroughToNotice(): void
    {
        $notice = null;
        set_error_handler(static function (int $errno, string $errstr) use (&$notice): bool {
            $notice = $errstr;
            return true;
        }, E_USER_NOTICE);
        try {
            $value = $this->protocol->ghost;
        } finally {
            restore_error_handler();
        }
        $this->assertNull($value);
        $this->assertNotNull($notice);
        $this->assertStringContainsString('ghost', $notice);
    }
}
