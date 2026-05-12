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
        switch ($type) {
            case TType::BOOL:
                return $this->readBool($bool);
            case TType::BYTE:
                return $this->readByte($byte);
            case TType::I16:
                return $this->readI16($i16);
            case TType::I32:
                return $this->readI32($i32);
            case TType::I64:
                return $this->readI64($i64);
            case TType::DOUBLE:
                return $this->readDouble($dub);
            case TType::STRING:
                return $this->readString($str);
            case TType::UUID:
                return $this->readUuid($uuid);
            case TType::STRUCT:
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

            case TType::MAP:
                $result = $this->readMapBegin($keyType, $valType, $size);
                for ($i = 0; $i < $size; $i++) {
                    $result += $this->skip($keyType);
                    $result += $this->skip($valType);
                }
                $result += $this->readMapEnd();

                return $result;

            case TType::SET:
                $result = $this->readSetBegin($elemType, $size);
                for ($i = 0; $i < $size; $i++) {
                    $result += $this->skip($elemType);
                }
                $result += $this->readSetEnd();

                return $result;

            case TType::LST:
                $result = $this->readListBegin($elemType, $size);
                for ($i = 0; $i < $size; $i++) {
                    $result += $this->skip($elemType);
                }
                $result += $this->readListEnd();

                return $result;

            default:
                throw new TProtocolException(
                    'Unknown field type: ' . $type,
                    TProtocolException::INVALID_DATA
                );
        }
    }

    /**
     * Utility for skipping binary data without parsing it.
     */
    public static function skipBinary(TTransport $itrans, int $type): int
    {
        switch ($type) {
            case TType::BOOL:
                $itrans->readAll(1);

                return 1;
            case TType::BYTE:
                $itrans->readAll(1);

                return 1;
            case TType::I16:
                $itrans->readAll(2);

                return 2;
            case TType::I32:
                $itrans->readAll(4);

                return 4;
            case TType::I64:
                $itrans->readAll(8);

                return 8;
            case TType::DOUBLE:
                $itrans->readAll(8);

                return 8;
            case TType::UUID:
                $itrans->readAll(16);

                return 16;
            case TType::STRING:
                $len = unpack('N', $itrans->readAll(4));
                $len = $len[1];
                if ($len > 0x7fffffff) {
                    $len = 0 - (($len - 1) ^ 0xffffffff);
                }

                $itrans->readAll($len);

                return 4 + $len;

            case TType::STRUCT:
                $result = 0;
                while (true) {
                    $data = $itrans->readAll(1);
                    $result += 1;
                    $arr = unpack('c', $data);
                    $ftype = $arr[1];
                    if ($ftype == TType::STOP) {
                        break;
                    }
                    // I16 field id
                    $itrans->readAll(2);
                    $result += 2;
                    $result += self::skipBinary($itrans, $ftype);
                }

                return $result;

            case TType::MAP:
                // Ktype
                $data = $itrans->readAll(1);
                $arr = unpack('c', $data);
                $ktype = $arr[1];
                // Vtype
                $data = $itrans->readAll(1);
                $arr = unpack('c', $data);
                $vtype = $arr[1];
                // Size
                $data = $itrans->readAll(4);
                $arr = unpack('N', $data);
                $size = $arr[1];
                if ($size > 0x7fffffff) {
                    $size = 0 - (($size - 1) ^ 0xffffffff);
                }
                $result = 6;
                for ($i = 0; $i < $size; $i++) {
                    $result += self::skipBinary($itrans, $ktype);
                    $result += self::skipBinary($itrans, $vtype);
                }

                return $result;

            case TType::SET:
            case TType::LST:
                // Vtype
                $data = $itrans->readAll(1);
                $arr = unpack('c', $data);
                $vtype = $arr[1];
                // Size
                $data = $itrans->readAll(4);
                $arr = unpack('N', $data);
                $size = $arr[1];
                if ($size > 0x7fffffff) {
                    $size = 0 - (($size - 1) ^ 0xffffffff);
                }
                $result = 5;
                for ($i = 0; $i < $size; $i++) {
                    $result += self::skipBinary($itrans, $vtype);
                }

                return $result;

            default:
                throw new TProtocolException(
                    'Unknown field type: ' . $type,
                    TProtocolException::INVALID_DATA
                );
        }
    }
}
