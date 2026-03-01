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

namespace Test\Thrift\Integration\Lib\ClassLoader;

use phpmock\phpunit\PHPMock;
use PHPUnit\Framework\TestCase;
use Thrift\ClassLoader\ThriftClassLoader;

/***
 * This test depends on running the compiler against the ./Resources/ThriftTest.thrift file:
 * lib/php/test$ ../../../compiler/cpp/thrift --gen php:classmap,server,rest,nsglobal="Classmap" -r  --out ./Resources/packages/phpcm ./Resources/ThriftTest.thrift
 */
class ThriftClassLoaderTest extends TestCase
{
    use PHPMock;

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
        $this->getFunctionMock('Thrift\ClassLoader', 'apcu_fetch')
             ->expects($useApcu ? $this->any() : $this->never())
             ->with($apcuPrefix . $class)
             ->willReturn(false);

        $this->getFunctionMock('Thrift\ClassLoader', 'apcu_store')
            ->expects($useApcu ? $this->any() : $this->never())
             ->with($apcuPrefix . $class, $this->anything())
             ->willReturn(true);

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
                'Classmap' => __DIR__ . '/../../../Resources/packages/phpcm',
            ],
            'class' => 'Classmap\ThriftTest\Xtruct',
        ];
        yield 'loadInterface' => [
            'definitions' => [
                'Classmap' => __DIR__ . '/../../../Resources/packages/phpcm',
            ],
            'class' => '\Classmap\ThriftTest\ThriftTestIf',
            'checkInterfaceExist' => true,
        ];
        yield 'loadClient' => [
            'definitions' => [
                'Classmap' => __DIR__ . '/../../../Resources/packages/phpcm',
            ],
            'class' => '\Classmap\ThriftTest\ThriftTestClient',
        ];
        yield 'loadProcessor' => [
            'definitions' => [
                'Classmap' => __DIR__ . '/../../../Resources/packages/phpcm',
            ],
            'class' => '\Classmap\ThriftTest\ThriftTestProcessor',
        ];
        yield 'loadRest' => [
            'definitions' => [
                'Classmap' => __DIR__ . '/../../../Resources/packages/phpcm',
            ],
            'class' => '\Classmap\ThriftTest\ThriftTestRest',
        ];
        yield 'load_args' => [
            'definitions' => [
                'Classmap' => __DIR__ . '/../../../Resources/packages/phpcm',
            ],
            'class' => '\Classmap\ThriftTest\ThriftTest_testVoid_args',
        ];
        yield 'load_result' => [
            'definitions' => [
                'Classmap' => __DIR__ . '/../../../Resources/packages/phpcm',
            ],
            'class' => '\Classmap\ThriftTest\ThriftTest_testVoid_result',
        ];
        yield 'pathAsArray' => [
            'definitions' => [
                'Classmap' => [__DIR__ . '/../../../Resources/packages/phpcm'],
            ],
            'class' => 'Classmap\ThriftTest\Xtruct',
        ];
        yield 'severalDefinitions' => [
            'definitions' => [
                'Classmap\ThriftTest' => [__DIR__ . '/../../../Resources/packages/phpcm'],
                'Classmap\TestValidators' => [__DIR__ . '/../../../Resources/packages/phpcm'],
            ],
            'class' => '\Classmap\TestValidators\TestServiceClient',
        ];
        yield 'useApcu' => [
            'definitions' => [
                'Classmap\ThriftTest' => [__DIR__ . '/../../../Resources/packages/phpcm'],
                'Classmap\TestValidators' => [__DIR__ . '/../../../Resources/packages/phpcm'],
            ],
            'class' => '\Classmap\TestValidators\TestServiceClient',
            'checkInterfaceExist' => false,
            'useApcu' => true,
            'apcuPrefix' => 'APCU_PREFIX',
        ];
    }
}
