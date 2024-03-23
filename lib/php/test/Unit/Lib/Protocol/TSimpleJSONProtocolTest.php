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

namespace Unit\Lib\Protocol;

use PHPUnit\Framework\TestCase;
use Thrift\Protocol\SimpleJSON\Context;
use Thrift\Protocol\SimpleJSON\ListContext;
use Thrift\Protocol\SimpleJSON\StructContext;
use Thrift\Protocol\TSimpleJSONProtocol;
use Thrift\Transport\TTransport;
use Thrift\Type\TType;

class TSimpleJSONProtocolTest extends TestCase
{
    private const COMMA = ',';
    private const COLON = ':';
    private const LBRACE = '{';
    private const RBRACE = '}';
    private const LBRACKET = '[';
    private const RBRACKET = ']';
    private const QUOTE = '"';

    public function testWriteMessage()
    {
        $name = 'test';
        $type = TType::STRING;
        $seqid = 1;

        $transport = $this->createMock(TTransport::class);
        $protocol = new TSimpleJSONProtocol($transport);
        $reflection = new \ReflectionClass($protocol);
        $writeContext = $reflection->getProperty('writeContext_');
        $writeContext->setAccessible(true);
        $writeContextStack = $reflection->getProperty('writeContextStack_');
        $writeContextStack->setAccessible(true);

        $transport
            ->expects($this->exactly(7))
            ->method('write')
            ->withConsecutive(
                ...[
                       [self::LBRACKET],
                       ['"'.$name.'"'],
                       [','],
                       [$type],
                       [','],
                       [$seqid],
                      [self::RBRACKET],
                   ]
            );

        $protocol->writeMessageBegin($name, $type, $seqid);

        $this->assertInstanceOf(ListContext::class, $writeContext->getValue($protocol));
        $this->assertCount(1, $writeContextStack->getValue($protocol));

        $protocol->writeMessageEnd();

        $this->assertInstanceOf(Context::class, $writeContext->getValue($protocol));
        $this->assertCount(0, $writeContextStack->getValue($protocol));
    }

    public function testWriteStruct()
    {
        $name = 'test';

        $transport = $this->createMock(TTransport::class);
        $protocol = new TSimpleJSONProtocol($transport);
        $reflection = new \ReflectionClass($protocol);
        $writeContext = $reflection->getProperty('writeContext_');
        $writeContext->setAccessible(true);
        $writeContextStack = $reflection->getProperty('writeContextStack_');
        $writeContextStack->setAccessible(true);

        $transport
            ->expects($this->exactly(2))
            ->method('write')
            ->withConsecutive(
                ...[
                       [self::LBRACE],
                       [self::RBRACE],
                   ]
            );

        $protocol->writeStructBegin($name);

        $this->assertInstanceOf(StructContext::class, $writeContext->getValue($protocol));
        $this->assertCount(1, $writeContextStack->getValue($protocol));

        $protocol->writeStructEnd();

        $this->assertInstanceOf(Context::class, $writeContext->getValue($protocol));
        $this->assertCount(0, $writeContextStack->getValue($protocol));
    }

    public function testWriteField()
    {
        $fieldName = 'fieldName';
        $fieldType = TType::STRING;
        $fieldId = 1;

        $transport = $this->createMock(TTransport::class);
        $protocol = new TSimpleJSONProtocol($transport);
        $reflection = new \ReflectionClass($protocol);
        $writeContext = $reflection->getProperty('writeContext_');
        $writeContext->setAccessible(true);
        $writeContextStack = $reflection->getProperty('writeContextStack_');
        $writeContextStack->setAccessible(true);

        $transport
            ->expects($this->exactly(1))
            ->method('write')
            ->withConsecutive(
                ...[
                       ['"'.$fieldName.'"'],
                   ]
            );

        $protocol->writeFieldBegin($fieldName, $fieldType, $fieldId);

        $this->assertInstanceOf(Context::class, $writeContext->getValue($protocol));
        $this->assertCount(0, $writeContextStack->getValue($protocol));

        $protocol->writeFieldEnd();

        $this->assertInstanceOf(Context::class, $writeContext->getValue($protocol));
        $this->assertCount(0, $writeContextStack->getValue($protocol));

        $protocol->writeFieldStop();

        $this->assertInstanceOf(Context::class, $writeContext->getValue($protocol));
        $this->assertCount(0, $writeContextStack->getValue($protocol));
    }


    public function testWriteList()
    {
        $elemType = TType::STRING;
        $size = 10;

        $transport = $this->createMock(TTransport::class);
        $protocol = new TSimpleJSONProtocol($transport);
        $reflection = new \ReflectionClass($protocol);
        $writeContext = $reflection->getProperty('writeContext_');
        $writeContext->setAccessible(true);
        $writeContextStack = $reflection->getProperty('writeContextStack_');
        $writeContextStack->setAccessible(true);

        $transport
            ->expects($this->exactly(2))
            ->method('write')
            ->withConsecutive(
                ...[
                       [self::LBRACKET],
                       [self::RBRACKET],
                   ]
            );

        $protocol->writeListBegin($elemType, $size);

        $this->assertInstanceOf(ListContext::class, $writeContext->getValue($protocol));
        $this->assertCount(1, $writeContextStack->getValue($protocol));

        $protocol->writeListEnd();

        $this->assertInstanceOf(Context::class, $writeContext->getValue($protocol));
        $this->assertCount(0, $writeContextStack->getValue($protocol));
    }
}
