--TEST--
Scalar sets accept value lists and preserve legacy keyed sets
--SKIPIF--
<?php
if (!extension_loaded('thrift_protocol')) {
    echo 'skip thrift_protocol extension not loaded';
}
?>
--FILE--
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

use Thrift\Protocol\TBinaryProtocol;
use Thrift\Transport\TMemoryBuffer;
use Thrift\Type\TType;

spl_autoload_register(function ($class) {
    if (strpos($class, 'Thrift\\') === 0) {
        require __DIR__ . '/../../../../lib/'
            . str_replace('\\', '/', substr($class, 7)) . '.php';
    }
});

class ScalarSetPayload
{
    public static $tspec;
    public static $isValidate = false;
    public $items;
}

$cases = [
    'empty' => [TType::I32, [], []],
    'integer list' => [TType::I32, [42, -7, 19], [42, -7, 19]],
    'legacy sequential keys' => [TType::I32, [0 => true, 1 => true], [0, 1]],
    'string list' => [TType::STRING, ['a', '123'], ['a', '123']],
    'legacy numeric string' => [TType::STRING, ['123' => true], ['123']],
    'bool list' => [TType::BOOL, [true, false], [true, false]],
    'ambiguous bool' => [TType::BOOL, [true], [false]],
    'legacy bool' => [TType::BOOL, [1 => true], [true]],
];
foreach ($cases as $name => [$type, $input, $elements]) {
    ScalarSetPayload::$tspec = [1 => [
        'var' => 'items', 'type' => TType::SET,
        'etype' => $type, 'elem' => ['type' => $type],
    ]];
    $payload = new ScalarSetPayload();
    $payload->items = $input;
    $actual = new TMemoryBuffer();
    thrift_protocol_write_binary(new TBinaryProtocol($actual), 'test', 1, $payload, 0, true);

    $expected = new TMemoryBuffer();
    $protocol = new TBinaryProtocol($expected);
    $protocol->writeMessageBegin('test', 1, 0);
    $protocol->writeStructBegin('ScalarSetPayload');
    $protocol->writeFieldBegin('items', TType::SET, 1);
    $protocol->writeSetBegin($type, count($elements));
    $writer = [TType::I32 => 'writeI32', TType::STRING => 'writeString', TType::BOOL => 'writeBool'][$type];
    foreach ($elements as $element) {
        $protocol->$writer($element);
    }
    $protocol->writeSetEnd();
    $protocol->writeFieldEnd();
    $protocol->writeFieldStop();
    $protocol->writeStructEnd();
    $protocol->writeMessageEnd();
    echo $name, ': ', $actual->getBuffer() === $expected->getBuffer() ? 'OK' : 'FAIL', "\n";
}
?>
--EXPECT--
empty: OK
integer list: OK
legacy sequential keys: OK
string list: OK
legacy numeric string: OK
bool list: OK
ambiguous bool: OK
legacy bool: OK
