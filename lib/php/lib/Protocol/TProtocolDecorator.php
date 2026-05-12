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
 * @package thrift.protocol
 */

declare(strict_types=1);

namespace Thrift\Protocol;

use Thrift\Exception\TException;

/**
 * <code>TProtocolDecorator</code> forwards all requests to an enclosed
 * <code>TProtocol</code> instance, providing a way to author concise
 * concrete decorator subclasses. While it has no abstract methods, it
 * is marked abstract as a reminder that by itself, it does not modify
 * the behaviour of the enclosed <code>TProtocol</code>.
 *
 * @package Thrift\Protocol
 */
abstract class TProtocolDecorator extends TProtocol
{
    /**
     * Instance of protocol, to which all operations will be forwarded.
     */
    private TProtocol $concreteProtocol;

    /**
     * Constructor of <code>TProtocolDecorator</code> class.
     * Encloses the specified protocol.
     *
     * @param TProtocol $protocol All operations will be forward to this instance. Must be non-null.
     */
    protected function __construct(TProtocol $protocol)
    {
        parent::__construct($protocol->getTransport());
        $this->concreteProtocol = $protocol;
    }

    public function writeMessageBegin(string $name, int $type, int $seqid): int
    {
        return $this->concreteProtocol->writeMessageBegin($name, $type, $seqid);
    }

    public function writeMessageEnd(): int
    {
        return $this->concreteProtocol->writeMessageEnd();
    }

    /**
     * @throws TException on write error
     */
    public function writeStructBegin(string $name): int
    {
        return $this->concreteProtocol->writeStructBegin($name);
    }

    /**
     * @throws TException on write error
     */
    public function writeStructEnd(): int
    {
        return $this->concreteProtocol->writeStructEnd();
    }

    public function writeFieldBegin(string $fieldName, int $fieldType, int $fieldId): int
    {
        return $this->concreteProtocol->writeFieldBegin($fieldName, $fieldType, $fieldId);
    }

    public function writeFieldEnd(): int
    {
        return $this->concreteProtocol->writeFieldEnd();
    }

    public function writeFieldStop(): int
    {
        return $this->concreteProtocol->writeFieldStop();
    }

    public function writeMapBegin(int $keyType, int $valType, int $size): int
    {
        return $this->concreteProtocol->writeMapBegin($keyType, $valType, $size);
    }

    public function writeMapEnd(): int
    {
        return $this->concreteProtocol->writeMapEnd();
    }

    public function writeListBegin(int $elemType, int $size): int
    {
        return $this->concreteProtocol->writeListBegin($elemType, $size);
    }

    public function writeListEnd(): int
    {
        return $this->concreteProtocol->writeListEnd();
    }

    public function writeSetBegin(int $elemType, int $size): int
    {
        return $this->concreteProtocol->writeSetBegin($elemType, $size);
    }

    public function writeSetEnd(): int
    {
        return $this->concreteProtocol->writeSetEnd();
    }

    public function writeBool(bool $bool): int
    {
        return $this->concreteProtocol->writeBool($bool);
    }

    public function writeByte(int $byte): int
    {
        return $this->concreteProtocol->writeByte($byte);
    }

    public function writeI16(int $i16): int
    {
        return $this->concreteProtocol->writeI16($i16);
    }

    public function writeI32(int $i32): int
    {
        return $this->concreteProtocol->writeI32($i32);
    }

    public function writeI64(int $i64): int
    {
        return $this->concreteProtocol->writeI64($i64);
    }

    public function writeDouble(float $dub): int
    {
        return $this->concreteProtocol->writeDouble($dub);
    }

    public function writeString(string $str): int
    {
        return $this->concreteProtocol->writeString($str);
    }

    public function writeUuid(string $uuid): int
    {
        return $this->concreteProtocol->writeUuid($uuid);
    }

    public function readMessageBegin(?string &$name, ?int &$type, ?int &$seqid): int
    {
        return $this->concreteProtocol->readMessageBegin($name, $type, $seqid);
    }

    public function readMessageEnd(): int
    {
        return $this->concreteProtocol->readMessageEnd();
    }

    public function readStructBegin(?string &$name): int
    {
        return $this->concreteProtocol->readStructBegin($name);
    }

    public function readStructEnd(): int
    {
        return $this->concreteProtocol->readStructEnd();
    }

    public function readFieldBegin(?string &$name, ?int &$fieldType, ?int &$fieldId): int
    {
        return $this->concreteProtocol->readFieldBegin($name, $fieldType, $fieldId);
    }

    public function readFieldEnd(): int
    {
        return $this->concreteProtocol->readFieldEnd();
    }

    public function readMapBegin(?int &$keyType, ?int &$valType, ?int &$size): int
    {
        return $this->concreteProtocol->readMapBegin($keyType, $valType, $size);
    }

    public function readMapEnd(): int
    {
        return $this->concreteProtocol->readMapEnd();
    }

    public function readListBegin(?int &$elemType, ?int &$size): int
    {
        return $this->concreteProtocol->readListBegin($elemType, $size);
    }

    public function readListEnd(): int
    {
        return $this->concreteProtocol->readListEnd();
    }

    public function readSetBegin(?int &$elemType, ?int &$size): int
    {
        return $this->concreteProtocol->readSetBegin($elemType, $size);
    }

    public function readSetEnd(): int
    {
        return $this->concreteProtocol->readSetEnd();
    }

    public function readBool(?bool &$bool): int
    {
        return $this->concreteProtocol->readBool($bool);
    }

    public function readByte(?int &$byte): int
    {
        return $this->concreteProtocol->readByte($byte);
    }

    public function readI16(?int &$i16): int
    {
        return $this->concreteProtocol->readI16($i16);
    }

    public function readI32(?int &$i32): int
    {
        return $this->concreteProtocol->readI32($i32);
    }

    public function readI64(?int &$i64): int
    {
        return $this->concreteProtocol->readI64($i64);
    }

    public function readDouble(?float &$dub): int
    {
        return $this->concreteProtocol->readDouble($dub);
    }

    public function readString(?string &$str): int
    {
        return $this->concreteProtocol->readString($str);
    }

    public function readUuid(?string &$uuid): int
    {
        return $this->concreteProtocol->readUuid($uuid);
    }
}
