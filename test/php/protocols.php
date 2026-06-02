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
 * Build a protocol factory by cross-test protocol name. Shared between the
 * socket-server CLI entry point (TestServer.php) and the cli-server HTTP
 * router (HttpRouter.php).
 *
 * Uses error_log() rather than STDERR because the constant is undefined in
 * the cli-server SAPI.
 */
function thrift_test_protocol_factory(string $protocol): \Thrift\Factory\TProtocolFactory
{
    switch ($protocol) {
        case 'binary':
            return new \Thrift\Factory\TBinaryProtocolFactory(false, true);
        case 'accel':
            if (!function_exists('thrift_protocol_write_binary')) {
                error_log('Acceleration extension is not loaded');
                exit(1);
            }
            return new \Thrift\Factory\TBinaryProtocolAcceleratedFactory();
        case 'compact':
            return new \Thrift\Factory\TCompactProtocolFactory();
        case 'json':
            return new \Thrift\Factory\TJSONProtocolFactory();
        default:
            error_log('--protocol must be one of {binary|compact|json|accel}');
            exit(1);
    }
}
