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
use Thrift\Transport\TTransport;
use Thrift\Type\TType;
use Thrift\Exception\TProtocolException;

/**
 * Protocol base class module.
 */
abstract class TProtocol
{
    protected function __construct(protected TTransport $trans)
    {
    }

    public function getTransport(): TTransport
    {
        return $this->trans;
    }

    abstract public function writeMessageBegin(string $name, int $type, int $seqid): int;

    abstract public function writeMessageEnd(): int;

    /**
     * @throws TException on write error
     */
    abstract public function writeStructBegin(string $name): int;

    /**
     * @throws TException on write error
     */
    abstract public function writeStructEnd(): int;

    /**
     * @throws TException on write error
     */
    abstract public function writeFieldBegin(string $fieldName, int $fieldType, int $fieldId): int;

    abstract public function writeFieldEnd(): int;

    abstract public function writeFieldStop(): int;

    abstract public function writeMapBegin(int $keyType, int $valType, int $size): int;

    abstract public function writeMapEnd(): int;

    abstract public function writeListBegin(int $elemType, int $size): int;

    abstract public function writeListEnd(): int;

    abstract public function writeSetBegin(int $elemType, int $size): int;

    abstract public function writeSetEnd(): int;

    abstract public function writeBool(bool $bool): int;

    abstract public function writeByte(int $byte): int;

    abstract public function writeI16(int $i16): int;

    abstract public function writeI32(int $i32): int;

    abstract public function writeI64(int $i64): int;

    abstract public function writeDouble(float $dub): int;

    abstract public function writeString(string $str): int;

    abstract public function writeUuid(string $uuid): int;

    abstract public function readMessageBegin(?string &$name, ?int &$type, ?int &$seqid): int;

    abstract public function readMessageEnd(): int;

    abstract public function readStructBegin(?string &$name): int;

    abstract public function readStructEnd(): int;

    abstract public function readFieldBegin(?string &$name, ?int &$fieldType, ?int &$fieldId): int;

    abstract public function readFieldEnd(): int;

    abstract public function readMapBegin(?int &$keyType, ?int &$valType, ?int &$size): int;

    abstract public function readMapEnd(): int;

    abstract public function readListBegin(?int &$elemType, ?int &$size): int;

    abstract public function readListEnd(): int;

    abstract public function readSetBegin(?int &$elemType, ?int &$size): int;

    abstract public function readSetEnd(): int;

    abstract public function readBool(?bool &$bool): int;

    abstract public function readByte(?int &$byte): int;

    abstract public function readI16(?int &$i16): int;

    abstract public function readI32(?int &$i32): int;

    abstract public function readI64(?int &$i64): int;

    abstract public function readDouble(?float &$dub): int;

    abstract public function readString(?string &$str): int;

    abstract public function readUuid(?string &$uuid): int;

    /**
     * Parses past unrecognized data without causing corruption.
     */
    public function skip(int $type): int
    {
        return match ($type) {
            TType::BOOL => $this->readBool($bool),
            TType::BYTE => $this->readByte($byte),
            TType::I16 => $this->readI16($i16),
            TType::I32 => $this->readI32($i32),
            TType::I64 => $this->readI64($i64),
            TType::DOUBLE => $this->readDouble($dub),
            TType::STRING => $this->readString($str),
            TType::UUID => $this->readUuid($uuid),
            TType::STRUCT => $this->skipStruct(),
            TType::MAP => $this->skipMap(),
            TType::SET => $this->skipSet(),
            TType::LST => $this->skipList(),
            default => throw new TProtocolException(
                'Unknown field type: ' . $type,
                TProtocolException::INVALID_DATA
            ),
        };
    }

    private function skipStruct(): int
    {
        $result = $this->readStructBegin($name);
        while (true) {
            $result += $this->readFieldBegin($name, $ftype, $fid);
            if ($ftype == TType::STOP) {
                break;
            }
            $result += $this->skip($ftype);
            $result += $this->readFieldEnd();
        }
        $result += $this->readStructEnd();

        return $result;
    }

    private function skipMap(): int
    {
        $result = $this->readMapBegin($keyType, $valType, $size);
        for ($i = 0; $i < $size; $i++) {
            $result += $this->skip($keyType);
            $result += $this->skip($valType);
        }
        $result += $this->readMapEnd();

        return $result;
    }

    private function skipSet(): int
    {
        $result = $this->readSetBegin($elemType, $size);
        for ($i = 0; $i < $size; $i++) {
            $result += $this->skip($elemType);
        }
        $result += $this->readSetEnd();

        return $result;
    }

    private function skipList(): int
    {
        $result = $this->readListBegin($elemType, $size);
        for ($i = 0; $i < $size; $i++) {
            $result += $this->skip($elemType);
        }
        $result += $this->readListEnd();

        return $result;
    }

    /**
     * Utility for skipping binary data without parsing it.
     */
    public static function skipBinary(TTransport $itrans, int $type): int
    {
        return match ($type) {
            TType::BOOL, TType::BYTE => self::skipBinaryFixed($itrans, 1),
            TType::I16 => self::skipBinaryFixed($itrans, 2),
            TType::I32 => self::skipBinaryFixed($itrans, 4),
            TType::I64, TType::DOUBLE => self::skipBinaryFixed($itrans, 8),
            TType::UUID => self::skipBinaryFixed($itrans, 16),
            TType::STRING => self::skipBinaryString($itrans),
            TType::STRUCT => self::skipBinaryStruct($itrans),
            TType::MAP => self::skipBinaryMap($itrans),
            TType::SET, TType::LST => self::skipBinaryCollection($itrans),
            default => throw new TProtocolException(
                'Unknown field type: ' . $type,
                TProtocolException::INVALID_DATA
            ),
        };
    }

    private static function skipBinaryFixed(TTransport $itrans, int $bytes): int
    {
        $itrans->readAll($bytes);

        return $bytes;
    }

    private static function skipBinaryString(TTransport $itrans): int
    {
        $len = self::readI32Signed($itrans);
        $itrans->readAll($len);

        return 4 + $len;
    }

    private static function skipBinaryStruct(TTransport $itrans): int
    {
        $result = 0;
        while (true) {
            $ftype = unpack('c', $itrans->readAll(1))[1];
            $result += 1;
            if ($ftype == TType::STOP) {
                break;
            }
            $itrans->readAll(2);
            $result += 2;
            $result += self::skipBinary($itrans, $ftype);
        }

        return $result;
    }

    private static function skipBinaryMap(TTransport $itrans): int
    {
        $ktype = unpack('c', $itrans->readAll(1))[1];
        $vtype = unpack('c', $itrans->readAll(1))[1];
        $size = self::readI32Signed($itrans);
        $result = 1 + 1 + 4;
        for ($i = 0; $i < $size; $i++) {
            $result += self::skipBinary($itrans, $ktype);
            $result += self::skipBinary($itrans, $vtype);
        }

        return $result;
    }

    private static function skipBinaryCollection(TTransport $itrans): int
    {
        $vtype = unpack('c', $itrans->readAll(1))[1];
        $size = self::readI32Signed($itrans);
        $result = 1 + 4;
        for ($i = 0; $i < $size; $i++) {
            $result += self::skipBinary($itrans, $vtype);
        }

        return $result;
    }

    private static function readI32Signed(TTransport $itrans): int
    {
        $n = unpack('N', $itrans->readAll(4))[1];

        return $n > 0x7fffffff ? 0 - (($n - 1) ^ 0xffffffff) : $n;
    }
}
