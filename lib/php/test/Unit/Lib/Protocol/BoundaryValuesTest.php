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
 *
 */

namespace Test\Thrift\Unit\Lib\Protocol;

use PHPUnit\Framework\TestCase;
use Thrift\Protocol\TBinaryProtocol;
use Thrift\Protocol\TCompactProtocol;
use Thrift\Protocol\TJSONProtocol;
use Thrift\Transport\TMemoryBuffer;
use Thrift\Type\TType;

class BoundaryValuesTest extends TestCase
{
    private static $WRITE_TYPE_MAP = [
        'writeByte' => TType::BYTE,
        'writeI16' => TType::I16,
        'writeI32' => TType::I32,
        'writeI64' => TType::I64,
        'writeDouble' => TType::DOUBLE,
        'writeString' => TType::STRING,
        'writeBool' => TType::BOOL,
    ];

    /**
     * @dataProvider boundaryProvider
     */
    public function testBoundaryValues($protocolClass, $writeMethod, $readMethod, $value)
    {
        $this->assertRoundtrip($protocolClass, $writeMethod, $readMethod, $value);
    }

    public static function boundaryProvider(): array
    {
        $cases = [
            // integers
            'byte min' => ['writeMethod' => 'writeByte', 'readMethod' => 'readByte', 'value' => -128],
            'byte max' => ['writeMethod' => 'writeByte', 'readMethod' => 'readByte', 'value' => 127],
            'byte zero' => ['writeMethod' => 'writeByte', 'readMethod' => 'readByte', 'value' => 0],
            'i16 min' => ['writeMethod' => 'writeI16', 'readMethod' => 'readI16', 'value' => -32768],
            'i16 max' => ['writeMethod' => 'writeI16', 'readMethod' => 'readI16', 'value' => 32767],
            'i16 zero' => ['writeMethod' => 'writeI16', 'readMethod' => 'readI16', 'value' => 0],
            'i32 min' => ['writeMethod' => 'writeI32', 'readMethod' => 'readI32', 'value' => -2147483648],
            'i32 max' => ['writeMethod' => 'writeI32', 'readMethod' => 'readI32', 'value' => 2147483647],
            'i32 zero' => ['writeMethod' => 'writeI32', 'readMethod' => 'readI32', 'value' => 0],
            'i64 min' => ['writeMethod' => 'writeI64', 'readMethod' => 'readI64', 'value' => PHP_INT_MIN],
            'i64 max' => ['writeMethod' => 'writeI64', 'readMethod' => 'readI64', 'value' => PHP_INT_MAX],
            'i64 zero' => ['writeMethod' => 'writeI64', 'readMethod' => 'readI64', 'value' => 0],

            // doubles
            'double zero' => ['writeMethod' => 'writeDouble', 'readMethod' => 'readDouble', 'value' => 0.0],
            'double negative zero' => ['writeMethod' => 'writeDouble', 'readMethod' => 'readDouble', 'value' => -0.0],
            // TODO: replace literals with PHP_FLOAT_MAX/MIN/EPSILON when PHP 7.1 support is dropped (available since PHP 7.2)
            'double max' => ['writeMethod' => 'writeDouble', 'readMethod' => 'readDouble', 'value' => 1.7976931348623158e+308],
            'double min' => ['writeMethod' => 'writeDouble', 'readMethod' => 'readDouble', 'value' => 2.2250738585072014e-308],
            'double epsilon' => ['writeMethod' => 'writeDouble', 'readMethod' => 'readDouble', 'value' => 2.2204460492503131e-16],
            'double very small' => ['writeMethod' => 'writeDouble', 'readMethod' => 'readDouble', 'value' => 1e-300],

            // strings
            'empty string' => ['writeMethod' => 'writeString', 'readMethod' => 'readString', 'value' => ''],
            'null byte' => ['writeMethod' => 'writeString', 'readMethod' => 'readString', 'value' => "\x00"],
            'unicode' => ['writeMethod' => 'writeString', 'readMethod' => 'readString', 'value' => "Привіт 🌍"],
            'long string' => ['writeMethod' => 'writeString', 'readMethod' => 'readString', 'value' => str_repeat('a', 1024)],

            // booleans
            'bool true' => ['writeMethod' => 'writeBool', 'readMethod' => 'readBool', 'value' => true],
            'bool false' => ['writeMethod' => 'writeBool', 'readMethod' => 'readBool', 'value' => false],
        ];

        return self::expandWithProtocols($cases);
    }

    private static function expandWithProtocols(array $cases): array
    {
        $protocols = [
            'Binary' => TBinaryProtocol::class,
            'Compact' => TCompactProtocol::class,
            'JSON' => TJSONProtocol::class,
        ];

        $expanded = [];
        foreach ($protocols as $protoName => $protoClass) {
            foreach ($cases as $caseName => $case) {
                $expanded[$protoName . ' ' . $caseName] = [
                    'protocolClass' => $protoClass,
                    'writeMethod' => $case['writeMethod'],
                    'readMethod' => $case['readMethod'],
                    'value' => $case['value'],
                ];
            }
        }

        return $expanded;
    }

    private function assertRoundtrip($protocolClass, $writeMethod, $readMethod, $value)
    {
        $fieldType = self::$WRITE_TYPE_MAP[$writeMethod];
        $transport = new TMemoryBuffer();
        $protocol = new $protocolClass($transport);

        $protocol->writeStructBegin('Test');
        $protocol->writeFieldBegin('field', $fieldType, 1);
        $protocol->$writeMethod($value);
        $protocol->writeFieldEnd();
        $protocol->writeFieldStop();
        $protocol->writeStructEnd();

        $transport2 = new TMemoryBuffer($transport->getBuffer());
        $protocol2 = new $protocolClass($transport2);
        $protocol2->readStructBegin($name);
        $protocol2->readFieldBegin($fname, $ftype, $fid);
        $protocol2->$readMethod($result);
        $protocol2->readFieldEnd();
        $protocol2->readStructEnd();

        $this->assertEquals($value, $result);
    }
}
