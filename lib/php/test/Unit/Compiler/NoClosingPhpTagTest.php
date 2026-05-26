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

use FilesystemIterator;
use PHPUnit\Framework\Attributes\DataProvider;
use PHPUnit\Framework\TestCase;
use RecursiveDirectoryIterator;
use RecursiveIteratorIterator;

/**
 * Regression for THRIFT-1636: generated PHP must not end with a `?>` closing
 * tag. A stray newline after `?>` in an included file is emitted to the
 * response, which is the primary reason PSR-12 forbids the tag.
 *
 * Scans every package generated into lib/php/test/Resources/packages by the
 * build step and asserts the file does not end with `?>`.
 */
class NoClosingPhpTagTest extends TestCase
{
    /**
     * @return iterable<string, array{string}>
     */
    public static function generatedPhpFileProvider(): iterable
    {
        $root = realpath(__DIR__ . '/../../Resources/packages');
        if ($root === false) {
            // Build step has not run yet; emit a single sentinel so phpunit
            // reports a skipped case instead of an opaque empty provider.
            yield 'packages dir missing' => ['__missing__'];

            return;
        }

        $iter = new RecursiveIteratorIterator(
            new RecursiveDirectoryIterator($root, FilesystemIterator::SKIP_DOTS),
        );
        foreach ($iter as $file) {
            if (!$file->isFile() || $file->getExtension() !== 'php') {
                continue;
            }
            $relative = substr((string) $file->getRealPath(), strlen($root) + 1);
            yield $relative => [(string) $file->getRealPath()];
        }
    }

    #[DataProvider('generatedPhpFileProvider')]
    public function testGeneratedFileHasNoClosingTag(string $path): void
    {
        if ($path === '__missing__') {
            $this->markTestSkipped('lib/php/test/Resources/packages not generated yet; run the build first.');
        }

        $contents = file_get_contents($path);
        $this->assertNotFalse($contents, "Cannot read $path");
        $trimmed = rtrim($contents);
        $this->assertStringEndsNotWith('?>', $trimmed, "$path ends with a PHP closing tag");
    }
}
