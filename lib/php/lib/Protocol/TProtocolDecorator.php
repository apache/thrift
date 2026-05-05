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
     *
     * @var TProtocol
     */
    private $concreteProtocol;

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

    /**
     * Writes the message header.
     *
     * @param string $name  Function name
     * @param int    $type  message type TMessageType::CALL or TMessageType::REPLY
     * @param int    $seqid The sequence id of this message
     */
    public function writeMessageBegin($name, $type, $seqid)
    {
        return $this->concreteProtocol->writeMessageBegin($name, $type, $seqid);
    }

    /**
     * Closes the message.
     */
    public function writeMessageEnd()
    {
        return $this->concreteProtocol->writeMessageEnd();
    }

    /**
     * Writes a struct header.
     *
     * @param string $name Struct name
     *
     * @throws TException on write error
     * @return int        How many bytes written
     */
    public function writeStructBegin($name)
    {
        return $this->concreteProtocol->writeStructBegin($name);
    }

    /**
     * Close a struct.
     *
     * @throws TException on write error
     * @return int        How many bytes written
     */
    public function writeStructEnd()
    {
        return $this->concreteProtocol->writeStructEnd();
    }

    public function writeFieldBegin($fieldName, $fieldType, $fieldId)
    {
        return $this->concreteProtocol->writeFieldBegin($fieldName, $fieldType, $fieldId);
    }

    public function writeFieldEnd()
    {
        return $this->concreteProtocol->writeFieldEnd();
    }

    public function writeFieldStop()
    {
        return $this->concreteProtocol->writeFieldStop();
    }

    public function writeMapBegin($keyType, $valType, $size)
    {
        return $this->concreteProtocol->writeMapBegin($keyType, $valType, $size);
    }

    public function writeMapEnd()
    {
        return $this->concreteProtocol->writeMapEnd();
    }

    public function writeListBegin($elemType, $size)
    {
        return $this->concreteProtocol->writeListBegin($elemType, $size);
    }

    public function writeListEnd()
    {
        return $this->concreteProtocol->writeListEnd();
    }

    public function writeSetBegin($elemType, $size)
    {
        return $this->concreteProtocol->writeSetBegin($elemType, $size);
    }

    public function writeSetEnd()
    {
        return $this->concreteProtocol->writeSetEnd();
    }

    public function writeBool($bool)
    {
        return $this->concreteProtocol->writeBool($bool);
    }

    public function writeByte($byte)
    {
        return $this->concreteProtocol->writeByte($byte);
    }

    public function writeI16($i16)
    {
        return $this->concreteProtocol->writeI16($i16);
    }

    public function writeI32($i32)
    {
        return $this->concreteProtocol->writeI32($i32);
    }

    public function writeI64($i64)
    {
        return $this->concreteProtocol->writeI64($i64);
    }

    public function writeDouble($dub)
    {
        return $this->concreteProtocol->writeDouble($dub);
    }

    public function writeString($str)
    {
        return $this->concreteProtocol->writeString($str);
    }

    public function writeUuid($uuid)
    {
        return $this->concreteProtocol->writeUuid($uuid);
    }

    /**
     * Reads the message header
     *
     * @param string $name  Function name
     * @param int    $type  message type TMessageType::CALL or TMessageType::REPLY
     * @param int    $seqid The sequence id of this message
     */
    public function readMessageBegin(&$name, &$type, &$seqid)
    {
        return $this->concreteProtocol->readMessageBegin($name, $type, $seqid);
    }

    /**
     * Read the close of message
     */
    public function readMessageEnd()
    {
        return $this->concreteProtocol->readMessageEnd();
    }

    public function readStructBegin(&$name)
    {
        return $this->concreteProtocol->readStructBegin($name);
    }

    public function readStructEnd()
    {
        return $this->concreteProtocol->readStructEnd();
    }

    public function readFieldBegin(&$name, &$fieldType, &$fieldId)
    {
        return $this->concreteProtocol->readFieldBegin($name, $fieldType, $fieldId);
    }

    public function readFieldEnd()
    {
        return $this->concreteProtocol->readFieldEnd();
    }

    public function readMapBegin(&$keyType, &$valType, &$size)
    {
        $this->concreteProtocol->readMapBegin($keyType, $valType, $size);
    }

    public function readMapEnd()
    {
        return $this->concreteProtocol->readMapEnd();
    }

    public function readListBegin(&$elemType, &$size)
    {
        $this->concreteProtocol->readListBegin($elemType, $size);
    }

    public function readListEnd()
    {
        return $this->concreteProtocol->readListEnd();
    }

    public function readSetBegin(&$elemType, &$size)
    {
        return $this->concreteProtocol->readSetBegin($elemType, $size);
    }

    public function readSetEnd()
    {
        return $this->concreteProtocol->readSetEnd();
    }

    public function readBool(&$bool)
    {
        return $this->concreteProtocol->readBool($bool);
    }

    public function readByte(&$byte)
    {
        return $this->concreteProtocol->readByte($byte);
    }

    public function readI16(&$i16)
    {
        return $this->concreteProtocol->readI16($i16);
    }

    public function readI32(&$i32)
    {
        return $this->concreteProtocol->readI32($i32);
    }

    public function readI64(&$i64)
    {
        return $this->concreteProtocol->readI64($i64);
    }

    public function readDouble(&$dub)
    {
        return $this->concreteProtocol->readDouble($dub);
    }

    public function readString(&$str)
    {
        return $this->concreteProtocol->readString($str);
    }

    public function readUuid(&$uuid)
    {
        return $this->concreteProtocol->readUuid($uuid);
    }
}
