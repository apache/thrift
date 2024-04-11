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

use phpmock\phpunit\PHPMock;
use PHPUnit\Framework\TestCase;
use Thrift\ClassLoader\ThriftClassLoader;

class ThriftClassLoaderTest extends TestCase
{
    use PHPMock;

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
        $this->getFunctionMock('Thrift\ClassLoader', 'apcu_fetch')
             ->expects($useApcu ? $this->once() : $this->never())
             ->with($apcuPrefix . $class)
             ->willReturn(false);

        $this->getFunctionMock('Thrift\ClassLoader', 'apcu_store')
             ->expects($useApcu ? $this->once() : $this->never())
             ->with($apcuPrefix . $class, $this->anything())
             ->willReturn(true);

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
            'apcuPrefix' => 'APCU_PREFIX',
        ];
    }
}
