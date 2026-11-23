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

use Thrift\Protocol\TCompactProtocol;
use Thrift\Transport\THttpClient;

error_reporting(E_ALL);

require_once __DIR__ . '/../../vendor/autoload.php';

$loader = new Thrift\ClassLoader\ThriftClassLoader();
$loader->registerDefinition('ThriftTest', __DIR__ . '/../../lib/php/test/Resources/packages/phpcm');
$loader->register();


$transport = new THttpClient('localhost', 80);

$transport->setTimeoutSecs($this->timeoutSec);

$transport->addHeaders($this->generateAuthHeader());

$protocol = new TCompactProtocol($transport);

$transport->open();

$client = new \ThriftTest\ThriftTestClient($protocol);
$client->testVoid();
