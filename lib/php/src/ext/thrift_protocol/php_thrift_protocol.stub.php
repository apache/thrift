<?php
/** @generate-function-entries */

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

// php /path/to/php-src/build/gen_stub.php lib/php/src/ext/thrift_protocol/php_thrift_protocol.stub.php

function thrift_protocol_write_binary(object $protocol, string $method_name, int $msgtype, object $request_struct, int $seqID, bool $strict_write): void {}

function thrift_protocol_read_binary(object $protocol, string $obj_typename, bool $strict_read, int $buffer_size=8192): object {}

function thrift_protocol_read_binary_after_message_begin(object $protocol, string $obj_typename, bool $strict_read, int $buffer_size=8192): object {}
