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

use Thrift\ClassLoader\ThriftClassLoader;

require_once __DIR__ . '/../../../vendor/autoload.php';

$loader = new ThriftClassLoader();
$loader->registerNamespace('Basic', __DIR__ . '/Resources/packages/php');
$loader->registerNamespace('BasicInline', __DIR__ . '/Resources/packages/phpi');
$loader->registerNamespace('Validate', __DIR__ . '/Resources/packages/phpv');
$loader->registerNamespace('ValidateOop', __DIR__ . '/Resources/packages/phpvo');
$loader->registerNamespace('Json', __DIR__ . '/Resources/packages/phpjs');
$loader->registerNamespace('PhpRec', __DIR__ . '/Resources/packages/phprec');
$loader->registerNamespace('PhpRecOop', __DIR__ . '/Resources/packages/phprecoop');
$loader->registerNamespace('PhpRecInline', __DIR__ . '/Resources/packages/phpreci');

#do not load this namespace here, it will be loaded in ClassLoaderTest
//$loader->registerNamespace('Server', __DIR__ . '/Resources/packages/phpcm');

$loader->register();
