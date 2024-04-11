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

namespace Test\Thrift\Integration\Lib\Serializer;

use PHPUnit\Framework\TestCase;
use stdClass;
use Thrift\ClassLoader\ThriftClassLoader;

/***
 * This test suite depends on running the compiler against the ./Resources/ThriftTest.thrift file:
 * lib/php/test$ ../../../compiler/cpp/thrift --gen php:json,nsglobal="Json" -r  --out ./Resources/packages/phpjs ./Resources/ThriftTest.thrift
 */
class JsonSerializeTest extends TestCase
{
    public function testEmptyStruct()
    {
        $empty = new \Json\ThriftTest\EmptyStruct(array('non_existing_key' => 'bar'));
        $this->assertEquals(new stdClass(), json_decode(json_encode($empty)));
    }

    public function testStringsAndInts()
    {
        $input = array(
            'string_thing' => 'foo',
            'i64_thing' => 1234567890,
        );
        $xtruct = new \Json\ThriftTest\Xtruct($input);

        // Xtruct's 'i32_thing' and 'byte_thing' fields should not be present here!
        $expected = new stdClass();
        $expected->string_thing = $input['string_thing'];
        $expected->i64_thing = $input['i64_thing'];
        $this->assertEquals($expected, json_decode(json_encode($xtruct)));
    }

    public function testNestedStructs()
    {
        $xtruct2 = new \Json\ThriftTest\Xtruct2(array(
            'byte_thing' => 42,
            'struct_thing' => new \Json\ThriftTest\Xtruct(array(
                'i32_thing' => 123456,
            )),
        ));

        $expected = new stdClass();
        $expected->byte_thing = $xtruct2->byte_thing;
        $expected->struct_thing = new stdClass();
        $expected->struct_thing->i32_thing = $xtruct2->struct_thing->i32_thing;
        $this->assertEquals($expected, json_decode(json_encode($xtruct2)));
    }

    public function testInsanity()
    {
        $xinput = array('string_thing' => 'foo');
        $xtruct = new \Json\ThriftTest\Xtruct($xinput);
        $insanity = new \Json\ThriftTest\Insanity(array(
            'xtructs' => array($xtruct, $xtruct, $xtruct)
        ));
        $expected = new stdClass();
        $expected->xtructs = array((object)$xinput, (object)$xinput, (object)$xinput);
        $this->assertEquals($expected, json_decode(json_encode($insanity)));
    }

    public function testNestedLists()
    {
        $bonk = new \Json\ThriftTest\Bonk(array('message' => 'foo'));
        $nested = new \Json\ThriftTest\NestedListsBonk(array('bonk' => array(array(array($bonk)))));
        $expected = new stdClass();
        $expected->bonk = array(array(array((object)array('message' => 'foo'))));
        $this->assertEquals($expected, json_decode(json_encode($nested)));
    }

    public function testMaps()
    {
        $intmap = new \Json\ThriftTest\ThriftTest_testMap_args(['thing' => [0 => 'zero']]);
        $emptymap = new \Json\ThriftTest\ThriftTest_testMap_args([]);
        $this->assertEquals('{"thing":{"0":"zero"}}', json_encode($intmap));
        $this->assertEquals('{}', json_encode($emptymap));
    }

    public function testScalarTypes()
    {
        $b = new \Json\ThriftTest\Bools(['im_true' => '1', 'im_false' => '0']);
        $this->assertEquals('{"im_true":true,"im_false":false}', json_encode($b));
        $s = new \Json\ThriftTest\StructA(['s' => 42]);
        $this->assertEquals('{"s":"42"}', json_encode($s));
    }
}
