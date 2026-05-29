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

/**
 * Two roles, dispatched on PHP_SAPI:
 *
 *  - CLI (sourced via require from TestServer.php): `pcntl_exec` into
 *    `php -S` with this file as the per-request router. $port and $protocol
 *    are read from the caller's scope.
 *  - cli-server: handle one HTTP request via HttpServer. The protocol is
 *    forwarded from the launcher through the THRIFT_TEST_PROTOCOL env var.
 */

// cli-server: per-request handler.
if (PHP_SAPI === 'cli-server') {
    require_once __DIR__ . '/../../vendor/autoload.php';
    require_once __DIR__ . '/protocols.php';
    require_once __DIR__ . '/HttpServer.php';

    // ThriftTest generated classes are not in Composer's PSR-4 map; register
    // the classmap loader before requiring Handler.php so its
    // `implements ThriftTestIf` resolves correctly.
    $loader = new \Thrift\ClassLoader\ThriftClassLoader();
    $loader->registerDefinition('ThriftTest', __DIR__ . '/gen-php-classmap');
    $loader->register();

    require_once __DIR__ . '/Handler.php';

    $protocolFactory = thrift_test_protocol_factory(getenv('THRIFT_TEST_PROTOCOL') ?: 'binary');
    $processor = new ThriftTest\ThriftTestProcessor(new Handler());

    (new HttpServer($processor, $protocolFactory))->serve();
    return;
}

// CLI launcher: sourced from TestServer.php; $port and $protocol are in scope.
if (!function_exists('pcntl_exec')) {
    fwrite(STDERR, "PHP HTTP cross-test requires ext-pcntl.\n");
    exit(1);
}

$env = getenv();
$env['THRIFT_TEST_PROTOCOL'] = $protocol;

echo "Starting the Test server (HTTP via php -S)...\n";

// -d flags mirror the launcher invocation in test/tests.json — keep in sync.
pcntl_exec(PHP_BINARY, [
    '-dextension_dir=php_ext_dir',
    '-dextension=thrift_protocol.so',
    '-ddisplay_errors=stderr',
    '-dlog_errors=0',
    '-derror_reporting=E_ALL',
    '-S', '127.0.0.1:' . $port,
    __FILE__,
], $env);

fwrite(STDERR, "pcntl_exec failed\n");
exit(1);
