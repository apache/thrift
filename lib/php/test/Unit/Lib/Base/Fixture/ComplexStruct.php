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

namespace Test\Thrift\Unit\Lib\Base\Fixture;

use Thrift\Base\TBase;
use Thrift\Type\TType;

class ComplexStruct extends TBase
{
    public static $_TSPEC = [
        1 => [
            'var' => 'flag',
            'type' => TType::BOOL,
        ],
        2 => [
            'var' => 'name',
            'type' => TType::STRING,
        ],
        3 => [
            'var' => 'child',
            'type' => TType::STRUCT,
            'class' => NestedStruct::class,
        ],
        4 => [
            'var' => 'mapField',
            'type' => TType::MAP,
            'ktype' => TType::STRING,
            'vtype' => TType::I32,
            'key' => [
                'type' => TType::STRING,
            ],
            'val' => [
                'type' => TType::I32,
            ],
        ],
        5 => [
            'var' => 'listField',
            'type' => TType::LST,
            'etype' => TType::STRUCT,
            'elem' => [
                'type' => TType::STRUCT,
                'class' => NestedStruct::class,
            ],
        ],
        6 => [
            'var' => 'setField',
            'type' => TType::SET,
            'etype' => TType::I16,
            'elem' => [
                'type' => TType::I16,
            ],
        ],
        7 => [
            'var' => 'mapOfLists',
            'type' => TType::MAP,
            'ktype' => TType::I32,
            'vtype' => TType::LST,
            'key' => [
                'type' => TType::I32,
            ],
            'val' => [
                'type' => TType::LST,
                'etype' => TType::I32,
                'elem' => [
                    'type' => TType::I32,
                ],
            ],
        ],
        8 => [
            'var' => 'optionalField',
            'type' => TType::STRING,
        ],
    ];

    public $flag = null;
    public $name = null;
    public $child = null;
    public $mapField = null;
    public $listField = null;
    public $setField = null;
    public $mapOfLists = null;
    public $optionalField = null;

    public function read($input)
    {
        return $this->_read(self::class, self::$_TSPEC, $input);
    }

    public function write($output)
    {
        return $this->_write('ComplexStruct', self::$_TSPEC, $output);
    }
}
