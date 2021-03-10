<?php
/** @generate-function-entries */

// php /path/to/php-src/build/gen_stub.php lib/php/src/ext/thrift_protocol/php_thrift_protocol.stub.php

function thrift_protocol_write_binary(object $protocol, string $method_name, int $msgtype, object $request_struct, int $seqID, bool $strict_write): void {}

function thrift_protocol_read_binary(object $protocol, string $obj_typename, bool $strict_read, int $buffer_size=8192): object {}

function thrift_protocol_read_binary_after_message_begin(object $protocol, string $obj_typename, bool $strict_read, int $buffer_size=8192): object {}
