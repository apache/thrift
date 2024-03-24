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

namespace Test\Thrift\Integration;

/***
 * This test suite depends on running the compiler against the ./Resources/ThriftTest.thrift file:
 * lib/php/test$ ../../../compiler/cpp/thrift --gen php:validate,nsglobal="Validate" -r  --out ./Resources/packages/phpv ./Resources/ThriftTest.thrift
 */
class ValidatorTest extends BaseValidatorTest
{
    public function getNsGlobal()
    {
        return 'Validate';
    }
}
