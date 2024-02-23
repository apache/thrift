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

namespace Test\Thrift\Unit\Lib\ClassLoader;

use PHPUnit\Framework\TestCase;
use Thrift\ClassLoader\ThriftClassLoader;

/***
 * This test depends on running the compiler against the ./Resources/ThriftTest.thrift file:
 * lib/php/test$ ../../../compiler/cpp/thrift --gen php:classmap,server,rest -r  --out ./Resources/packages/phpcm ./Resources/ThriftTest.thrift
 */
class ThriftClassLoaderTest extends TestCase
{
    const APCU_PREFIX = 'test';

    /**
     * @dataProvider registerNamespaceDataProvider
     */
    public function testRegisterNamespace(
        $namespaces,
        $class,
        $isClassExist = true,
        $useApcu = false,
        $apcuPrefix = null
    ) {
        $loader = new ThriftClassLoader($useApcu, $apcuPrefix);
        foreach ($namespaces as $namespace => $paths) {
            $loader->registerNamespace($namespace, $paths);
        }
        $loader->register();
        $loader->loadClass($class);
        if ($isClassExist) {
            $this->assertTrue(class_exists($class, false), "->loadClass() loads '$class'");
        } else {
            $this->assertFalse(class_exists($class, false), "->loadClass() loads '$class'");
        }
    }

    public function registerNamespaceDataProvider()
    {
        yield 'default' => [
            'namespaces' => [
                'A' => __DIR__ . '/Fixtures',
            ],
            'class' => 'A\TestClass',
        ];
        yield 'missedClass' => [
            'namespaces' => [
                'A' => __DIR__ . '/Fixtures',
            ],
            'class' => 'A\MissedClass',
            'isClassExist' => false,
        ];
        yield 'pathAsArray' => [
            'namespaces' => [
                'B' => [__DIR__ . '/Fixtures'],
            ],
            'class' => 'B\TestClass',
        ];
        yield 'loadClassWithSlash' => [
            'namespaces' => [
                'C' => __DIR__ . '/Fixtures',
            ],
            'class' => '\C\TestClass',
            ];
        yield 'severalNamespaces' => [
            'namespaces' => [
                'D' => __DIR__ . '/Fixtures',
                'E' => __DIR__ . '/Fixtures',
            ],
            'class' => '\E\TestClass',
        ];
        yield 'useApcu' => [
            'namespaces' => [
                'D' => __DIR__ . '/Fixtures',
                'E' => __DIR__ . '/Fixtures',
            ],
            'class' => '\E\TestClass',
            'isClassExist' => true,
            'useApcu' => true,
            'apcuPrefix' => self::APCU_PREFIX,
        ];
    }

    /**
     * @dataProvider registerDefinitionDataProvider
     */
    public function testRegisterDefinition(
        $definitions,
        $class,
        $checkInterfaceExist = false,
        $useApcu = false,
        $apcuPrefix = null
    ) {
        $loader = new ThriftClassLoader($useApcu, $apcuPrefix);
        foreach ($definitions as $namespace => $paths) {
            $loader->registerDefinition($namespace, $paths);
        }
        $loader->register();

        $loader->loadClass($class);
        if ($checkInterfaceExist) {
            $this->assertTrue(interface_exists($class, false), "->loadClass() loads '$class'");
        } else {
            $this->assertTrue(class_exists($class, false), "->loadClass() loads '$class'");
        }
    }

    public function registerDefinitionDataProvider()
    {
        yield 'loadType' => [
            'definitions' => [
                'ThriftTest' => __DIR__ . '/../../../Resources/packages/phpcm',
            ],
            'class' => 'ThriftTest\Xtruct',
        ];
        yield 'loadInterface' => [
            'definitions' => [
                'ThriftTest' => __DIR__ . '/../../../Resources/packages/phpcm',
            ],
            'class' => '\ThriftTest\ThriftTestIf',
            'checkInterfaceExist' => true,
        ];
        yield 'loadClient' => [
            'definitions' => [
                'ThriftTest' => __DIR__ . '/../../../Resources/packages/phpcm',
            ],
            'class' => '\ThriftTest\ThriftTestClient',
        ];
        yield 'loadProcessor' => [
            'definitions' => [
                'ThriftTest' => __DIR__ . '/../../../Resources/packages/phpcm',
            ],
            'class' => '\ThriftTest\ThriftTestProcessor',
        ];
        yield 'loadRest' => [
            'definitions' => [
                'ThriftTest' => __DIR__ . '/../../../Resources/packages/phpcm',
            ],
            'class' => '\ThriftTest\ThriftTestRest',
        ];
        yield 'load_args' => [
            'definitions' => [
                'ThriftTest' => __DIR__ . '/../../../Resources/packages/phpcm',
            ],
            'class' => '\ThriftTest\ThriftTest_testVoid_args',
        ];
        yield 'load_result' => [
            'definitions' => [
                'ThriftTest' => __DIR__ . '/../../../Resources/packages/phpcm',
            ],
            'class' => '\ThriftTest\ThriftTest_testVoid_result',
        ];
        yield 'pathAsArray' => [
            'definitions' => [
                'ThriftTest' => [__DIR__ . '/../../../Resources/packages/phpcm'],
            ],
            'class' => 'ThriftTest\Xtruct',
        ];
        yield 'severalDefinitions' => [
            'definitions' => [
                'ThriftTest' => [__DIR__ . '/../../../Resources/packages/phpcm'],
                'TestValidators' => [__DIR__ . '/../../../Resources/packages/phpcm'],
            ],
            'class' => '\TestValidators\TestServiceClient',
        ];
        yield 'useApcu' => [
            'definitions' => [
                'ThriftTest' => [__DIR__ . '/../../../Resources/packages/phpcm'],
                'TestValidators' => [__DIR__ . '/../../../Resources/packages/phpcm'],
            ],
            'class' => '\TestValidators\TestServiceClient',
            'checkInterfaceExist' => false,
            'useApcu' => true,
            'apcuPrefix' => self::APCU_PREFIX,
        ];
    }
}

namespace Thrift\ClassLoader;

use Test\Thrift\Unit\Lib\ClassLoader\ThriftClassLoaderTest;

if (!function_exists('apcu_fetch')) {
    {
        function apcu_fetch($key, &$success = null)
        {
            if (strpos($key, ThriftClassLoaderTest::APCU_PREFIX) === false) {
                throw new \Exception('apcu_fetch error, invalid key');
            }

            return false;
        }

        function apcu_store($key, $var, $ttl = 0)
        {
            if (strpos($key, ThriftClassLoaderTest::APCU_PREFIX) === false) {
                throw new \Exception('apcu_store error, invalid key');
            }

            return false;
        }
    }
}
