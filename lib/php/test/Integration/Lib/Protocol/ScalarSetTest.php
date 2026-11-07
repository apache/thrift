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

namespace Test\Thrift\Integration\Lib\Protocol;

use Classmap\ThriftTest\ThriftTestIf;
use Classmap\ThriftTest\ThriftTestRest;
use Classmap\ThriftTest\ThriftTest_testSet_args;
use PHPUnit\Framework\Attributes\DataProvider;
use PHPUnit\Framework\TestCase;
use Thrift\ClassLoader\ThriftClassLoader;
use Thrift\Protocol\TBinaryProtocol;
use Thrift\Transport\TMemoryBuffer;

class ScalarSetTest extends TestCase
{
    #[DataProvider('scalarSetCoercionProvider')]
    public function testScalarSetCoercionIsConsistent(
        string $class,
        array $fields,
        array $values,
        array $expected,
        bool $inlined
    ): void {
        $value = new $class(array_fill_keys($fields, $values));
        $restored = new $class();
        if ($inlined) {
            $buffer = '';
            $value->write($buffer);
            $restored->read(new TMemoryBuffer($buffer));
        } else {
            $protocol = new TBinaryProtocol(new TMemoryBuffer());
            $value->write($protocol);
            $restored->read($protocol);
        }

        foreach ($fields as $field) {
            $this->assertSame($expected, $restored->$field);
        }
    }

    public static function scalarSetCoercionProvider(): iterable
    {
        foreach (['Basic', 'BasicInline', 'ValidateOop'] as $namespace) {
            $inlined = $namespace === 'BasicInline';
            yield $namespace . ' numeric strings' => [
                $namespace . '\\ThriftTest\\ThriftTest_testSet_args',
                ['thing'], ['10', '20'], [10 => true, 20 => true], $inlined,
            ];
            yield $namespace . ' integer booleans' => [
                $namespace . '\\TestValidators\\BoolSetTest',
                ['direct', 'aliased', 'chained'], [1, 0], [1 => true, 0 => true], $inlined,
            ];
        }
    }

    #[DataProvider('restSetProvider')]
    public function testRestSetPreservesElementsThroughSerialization(array $elements): void
    {
        $loader = new ThriftClassLoader();
        $loader->registerDefinition('Classmap', __DIR__ . '/../../../Resources/packages/phpcm');
        $loader->register();

        try {
            $expected = array_fill_keys($elements, true);
            $handler = $this->createMock(ThriftTestIf::class);
            $handler->expects($this->once())->method('testSet')
                ->willReturnCallback(function (array $values): array {
                    $protocol = new TBinaryProtocol(new TMemoryBuffer());
                    $args = new ThriftTest_testSet_args(['thing' => $values]);
                    $args->write($protocol);
                    $restored = new ThriftTest_testSet_args();
                    $restored->read($protocol);
                    return $restored->thing;
                });

            $rest = new ThriftTestRest($handler);
            $this->assertSame($expected, $rest->testSet(['thing' => json_encode($elements)]));
        } finally {
            spl_autoload_unregister([$loader, 'loadClass']);
        }
    }

    public static function restSetProvider(): iterable
    {
        yield 'empty' => [[]];
        yield 'zero and one' => [[0, 1]];
        yield 'zero through two' => [[0, 1, 2]];
        yield 'nonsequential' => [[-5, 10]];
    }
}
