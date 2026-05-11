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

namespace Test\Thrift\Unit\Lib\Fixture;

use Thrift\Exception\TException;
use Thrift\Type\TType;

class TestRichException extends TException
{
    public static $tspec = [];

    public $message = null;
    public $code = null;
    public $stringField = null;
    public $intField = null;
    public $boolField = null;
    public $doubleField = null;
    public $mapField = null;
    public $listField = null;
    public $setField = null;

    public function __construct($vals = null)
    {
        self::$tspec = [
            1 => [
                'var' => 'stringField',
                'type' => TType::STRING,
            ],
            2 => [
                'var' => 'intField',
                'type' => TType::I32,
            ],
            3 => [
                'var' => 'boolField',
                'type' => TType::BOOL,
            ],
            4 => [
                'var' => 'doubleField',
                'type' => TType::DOUBLE,
            ],
            5 => [
                'var' => 'mapField',
                'type' => TType::MAP,
                'ktype' => TType::STRING,
                'vtype' => TType::I32,
                'key' => ['type' => TType::STRING],
                'val' => ['type' => TType::I32],
            ],
            6 => [
                'var' => 'listField',
                'type' => TType::LST,
                'etype' => TType::STRING,
                'elem' => ['type' => TType::STRING],
            ],
            7 => [
                'var' => 'setField',
                'type' => TType::SET,
                'etype' => TType::I32,
                'elem' => ['type' => TType::I32],
            ],
        ];

        if (is_array($vals)) {
            parent::__construct(self::$tspec, $vals);
        }
    }

    public function getName()
    {
        return 'TestRichException';
    }

    public function read($input)
    {
        return $this->readStruct(self::class, self::$tspec, $input);
    }

    public function write($output)
    {
        return $this->writeStruct('TestRichException', self::$tspec, $output);
    }
}
