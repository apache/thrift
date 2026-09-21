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

namespace Test\Thrift\Unit\Lib\Server\Fixture;

use Thrift\Factory\TProtocolFactory;
use Thrift\Protocol\TProtocol;
use Thrift\Transport\TMemoryBuffer;
use Thrift\Type\TMessageType;
use Thrift\Type\TType;

/**
 * Reads a call the way a generated processor does - the message header first,
 * then the arguments - answers it with an empty result and counts it.
 */
class CountingProcessor
{
    public int $served = 0;

    public function process(TProtocol $input, TProtocol $output): bool
    {
        $input->readMessageBegin($name, $type, $seqid);
        $input->skip(TType::STRUCT);
        $input->readMessageEnd();
        ++$this->served;

        $output->writeMessageBegin($name, TMessageType::REPLY, $seqid);
        $output->writeStructBegin('result');
        $output->writeFieldStop();
        $output->writeStructEnd();
        $output->writeMessageEnd();
        $output->getTransport()->flush();

        return true;
    }

    /**
     * A call to a method without arguments, as the factory's protocol writes it.
     */
    public static function call(TProtocolFactory $factory): string
    {
        $buffer = new TMemoryBuffer();
        $protocol = $factory->getProtocol($buffer);
        $protocol->writeMessageBegin('ping', TMessageType::CALL, 1);
        $protocol->writeStructBegin('ping_args');
        $protocol->writeFieldStop();
        $protocol->writeStructEnd();
        $protocol->writeMessageEnd();

        return $buffer->getBuffer();
    }
}
