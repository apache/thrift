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

use Thrift\Transport\TTransport;
use Thrift\Type\TType;
use Thrift\Exception\TProtocolException;

/**
 * Binary implementation of the Thrift protocol.
 *
 */
class TBinaryProtocol extends TProtocol
{
    public const VERSION_MASK = 0xffff0000;
    public const VERSION_1 = 0x80010000;

    public function __construct(
        TTransport $trans,
        protected bool $strictRead = false,
        protected bool $strictWrite = true,
    ) {
        parent::__construct($trans);
    }

    public function writeMessageBegin(string $name, int $type, int $seqid): int
    {
        if ($this->strictWrite) {
            $version = self::VERSION_1 | $type;

            return
                $this->writeI32($version) +
                $this->writeString($name) +
                $this->writeI32($seqid);
        } else {
            return
                $this->writeString($name) +
                $this->writeByte($type) +
                $this->writeI32($seqid);
        }
    }

    public function writeMessageEnd(): int
    {
        return 0;
    }

    public function writeStructBegin(string $name): int
    {
        return 0;
    }

    public function writeStructEnd(): int
    {
        return 0;
    }

    public function writeFieldBegin(string $fieldName, int $fieldType, int $fieldId): int
    {
        return
            $this->writeByte($fieldType) +
            $this->writeI16($fieldId);
    }

    public function writeFieldEnd(): int
    {
        return 0;
    }

    public function writeFieldStop(): int
    {
        return $this->writeByte(TType::STOP);
    }

    public function writeMapBegin(int $keyType, int $valType, int $size): int
    {
        return
            $this->writeByte($keyType) +
            $this->writeByte($valType) +
            $this->writeI32($size);
    }

    public function writeMapEnd(): int
    {
        return 0;
    }

    public function writeListBegin(int $elemType, int $size): int
    {
        return
            $this->writeByte($elemType) +
            $this->writeI32($size);
    }

    public function writeListEnd(): int
    {
        return 0;
    }

    public function writeSetBegin(int $elemType, int $size): int
    {
        return
            $this->writeByte($elemType) +
            $this->writeI32($size);
    }

    public function writeSetEnd(): int
    {
        return 0;
    }

    public function writeBool(bool $bool): int
    {
        $data = pack('c', $bool ? 1 : 0);
        $this->trans->write($data);

        return 1;
    }

    public function writeByte(int $byte): int
    {
        $data = pack('c', $byte);
        $this->trans->write($data);

        return 1;
    }

    public function writeI16(int $i16): int
    {
        $data = pack('n', $i16);
        $this->trans->write($data);

        return 2;
    }

    public function writeI32(int $i32): int
    {
        $data = pack('N', $i32);
        $this->trans->write($data);

        return 4;
    }

    public function writeI64(int $i64): int
    {
        // If we are on a 32bit architecture we have to explicitly deal with
        // 64-bit twos-complement arithmetic since PHP wants to treat all ints
        // as signed and any int over 2^31 - 1 as a float
        if (PHP_INT_SIZE == 4) {
            $neg = $i64 < 0;

            if ($neg) {
                $i64 *= -1;
            }

            $hi = (int)($i64 / 4294967296);
            $lo = (int)$i64;

            if ($neg) {
                $hi = ~$hi;
                $lo = ~$lo;
                if (($lo & (int)0xffffffff) == (int)0xffffffff) {
                    $lo = 0;
                    $hi++;
                } else {
                    $lo++;
                }
            }
            $data = pack('N2', $hi, $lo);
        } else {
            $hi = $i64 >> 32;
            $lo = $i64 & 0xFFFFFFFF;
            $data = pack('N2', $hi, $lo);
        }

        $this->trans->write($data);

        return 8;
    }

    public function writeDouble(float $dub): int
    {
        $data = pack('d', $dub);
        $this->trans->write(strrev($data));

        return 8;
    }

    public function writeString(string $str): int
    {
        $len = strlen($str);
        $result = $this->writeI32($len);
        if ($len) {
            $this->trans->write($str);
        }

        return $result + $len;
    }

    public function writeUuid(string $uuid): int
    {
        $data = hex2bin(str_replace('-', '', $uuid));
        $this->trans->write($data);

        return 16;
    }

    public function readMessageBegin(?string &$name, ?int &$type, ?int &$seqid): int
    {
        $result = $this->readI32($sz);
        if ($sz < 0) {
            $version = (int)($sz & self::VERSION_MASK);
            if ($version != (int)self::VERSION_1) {
                throw new TProtocolException('Bad version identifier: ' . $sz, TProtocolException::BAD_VERSION);
            }
            $type = $sz & 0x000000ff;
            $result +=
                $this->readString($name) +
                $this->readI32($seqid);
        } else {
            if ($this->strictRead) {
                throw new TProtocolException(
                    'No version identifier, old protocol client?',
                    TProtocolException::BAD_VERSION
                );
            } else {
                // Handle pre-versioned input
                $name = $this->trans->readAll($sz);
                $result +=
                    $sz +
                    $this->readByte($type) +
                    $this->readI32($seqid);
            }
        }

        return $result;
    }

    public function readMessageEnd(): int
    {
        return 0;
    }

    public function readStructBegin(?string &$name): int
    {
        $name = '';

        return 0;
    }

    public function readStructEnd(): int
    {
        return 0;
    }

    public function readFieldBegin(?string &$name, ?int &$fieldType, ?int &$fieldId): int
    {
        $result = $this->readByte($fieldType);
        if ($fieldType == TType::STOP) {
            $fieldId = 0;

            return $result;
        }
        $result += $this->readI16($fieldId);

        return $result;
    }

    public function readFieldEnd(): int
    {
        return 0;
    }

    public function readMapBegin(?int &$keyType, ?int &$valType, ?int &$size): int
    {
        return
            $this->readByte($keyType) +
            $this->readByte($valType) +
            $this->readI32($size);
    }

    public function readMapEnd(): int
    {
        return 0;
    }

    public function readListBegin(?int &$elemType, ?int &$size): int
    {
        return
            $this->readByte($elemType) +
            $this->readI32($size);
    }

    public function readListEnd(): int
    {
        return 0;
    }

    public function readSetBegin(?int &$elemType, ?int &$size): int
    {
        return
            $this->readByte($elemType) +
            $this->readI32($size);
    }

    public function readSetEnd(): int
    {
        return 0;
    }

    public function readBool(?bool &$bool): int
    {
        $data = $this->trans->readAll(1);
        $arr = unpack('c', $data);
        $bool = $arr[1] == 1;

        return 1;
    }

    public function readByte(?int &$byte): int
    {
        $data = $this->trans->readAll(1);
        $arr = unpack('c', $data);
        $byte = $arr[1];

        return 1;
    }

    public function readI16(?int &$i16): int
    {
        $data = $this->trans->readAll(2);
        $arr = unpack('n', $data);
        $i16 = $arr[1];
        if ($i16 > 0x7fff) {
            $i16 = 0 - (($i16 - 1) ^ 0xffff);
        }

        return 2;
    }

    public function readI32(?int &$i32): int
    {
        $data = $this->trans->readAll(4);
        $arr = unpack('N', $data);
        $i32 = $arr[1];
        if ($i32 > 0x7fffffff) {
            $i32 = 0 - (($i32 - 1) ^ 0xffffffff);
        }

        return 4;
    }

    public function readI64(?int &$i64): int
    {
        $data = $this->trans->readAll(8);

        $arr = unpack('N2', $data);

        // If we are on a 32bit architecture we have to explicitly deal with
        // 64-bit twos-complement arithmetic since PHP wants to treat all ints
        // as signed and any int over 2^31 - 1 as a float
        if (PHP_INT_SIZE == 4) {
            $hi = (int)$arr[1];
            $lo = (int)$arr[2];
            $isNeg = $hi < 0;

            // Check for a negative
            if ($isNeg) {
                $hi = ~$hi & (int)0xffffffff;
                $lo = ~$lo & (int)0xffffffff;

                if ($lo == (int)0xffffffff) {
                    $hi++;
                    $lo = 0;
                } else {
                    $lo++;
                }
            }

            // Force 32bit words in excess of 2G to pe positive - we deal wigh sign
            // explicitly below

            if ($hi & (int)0x80000000) {
                $hi &= (int)0x7fffffff;
                $hi += 0x80000000;
            }

            if ($lo & (int)0x80000000) {
                $lo &= (int)0x7fffffff;
                $lo += 0x80000000;
            }

            $i64 = $hi * 4294967296 + $lo;

            if ($isNeg) {
                $i64 = 0 - $i64;
            }
        } else {
            // Upcast negatives in LSB bit
            if ($arr[2] & 0x80000000) {
                $arr[2] = $arr[2] & 0xffffffff;
            }

            // Check for a negative
            if ($arr[1] & 0x80000000) {
                $arr[1] = $arr[1] & 0xffffffff;
                $arr[1] = $arr[1] ^ 0xffffffff;
                $arr[2] = $arr[2] ^ 0xffffffff;
                $i64 = 0 - $arr[1] * 4294967296 - $arr[2] - 1;
            } else {
                $i64 = $arr[1] * 4294967296 + $arr[2];
            }
        }

        return 8;
    }

    public function readDouble(?float &$dub): int
    {
        $data = strrev($this->trans->readAll(8));
        $arr = unpack('d', $data);
        $dub = $arr[1];

        return 8;
    }

    public function readString(?string &$str): int
    {
        $result = $this->readI32($len);
        if ($len) {
            $str = $this->trans->readAll($len);
        } else {
            $str = '';
        }

        return $result + $len;
    }

    public function readUuid(?string &$uuid): int
    {
        $data = $this->trans->readAll(16);
        $hex = bin2hex($data);
        $uuid = substr($hex, 0, 8) . '-' .
                 substr($hex, 8, 4) . '-' .
                 substr($hex, 12, 4) . '-' .
                 substr($hex, 16, 4) . '-' .
                 substr($hex, 20, 12);

        return 16;
    }
}
