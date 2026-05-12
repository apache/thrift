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
 * Compact implementation of the Thrift protocol.
 *
 */
class TCompactProtocol extends TProtocol
{
    public const COMPACT_STOP = 0x00;
    public const COMPACT_TRUE = 0x01;
    public const COMPACT_FALSE = 0x02;
    public const COMPACT_BYTE = 0x03;
    public const COMPACT_I16 = 0x04;
    public const COMPACT_I32 = 0x05;
    public const COMPACT_I64 = 0x06;
    public const COMPACT_DOUBLE = 0x07;
    public const COMPACT_BINARY = 0x08;
    public const COMPACT_LIST = 0x09;
    public const COMPACT_SET = 0x0A;
    public const COMPACT_MAP = 0x0B;
    public const COMPACT_STRUCT = 0x0C;
    public const COMPACT_UUID = 0x0D;

    public const STATE_CLEAR = 0;
    public const STATE_FIELD_WRITE = 1;
    public const STATE_VALUE_WRITE = 2;
    public const STATE_CONTAINER_WRITE = 3;
    public const STATE_BOOL_WRITE = 4;
    public const STATE_FIELD_READ = 5;
    public const STATE_CONTAINER_READ = 6;
    public const STATE_VALUE_READ = 7;
    public const STATE_BOOL_READ = 8;

    public const VERSION_MASK = 0x1f;
    public const VERSION = 1;
    public const PROTOCOL_ID = 0x82;
    public const TYPE_MASK = 0xe0;
    public const TYPE_BITS = 0x07;
    public const TYPE_SHIFT_AMOUNT = 5;

    public const MAX_VARINT_BYTES = 10; // ceil(64/7); matches protobuf wire format

    /** @var array<int, int> */
    protected static array $ctypes = [
        TType::STOP => TCompactProtocol::COMPACT_STOP,
        TType::BOOL => TCompactProtocol::COMPACT_TRUE, // used for collection
        TType::BYTE => TCompactProtocol::COMPACT_BYTE,
        TType::I16 => TCompactProtocol::COMPACT_I16,
        TType::I32 => TCompactProtocol::COMPACT_I32,
        TType::I64 => TCompactProtocol::COMPACT_I64,
        TType::DOUBLE => TCompactProtocol::COMPACT_DOUBLE,
        TType::STRING => TCompactProtocol::COMPACT_BINARY,
        TType::STRUCT => TCompactProtocol::COMPACT_STRUCT,
        TType::LST => TCompactProtocol::COMPACT_LIST,
        TType::SET => TCompactProtocol::COMPACT_SET,
        TType::MAP => TCompactProtocol::COMPACT_MAP,
        TType::UUID => TCompactProtocol::COMPACT_UUID,
    ];

    /** @var array<int, int> */
    protected static array $ttypes = [
        TCompactProtocol::COMPACT_STOP => TType::STOP,
        TCompactProtocol::COMPACT_TRUE => TType::BOOL, // used for collection
        TCompactProtocol::COMPACT_FALSE => TType::BOOL,
        TCompactProtocol::COMPACT_BYTE => TType::BYTE,
        TCompactProtocol::COMPACT_I16 => TType::I16,
        TCompactProtocol::COMPACT_I32 => TType::I32,
        TCompactProtocol::COMPACT_I64 => TType::I64,
        TCompactProtocol::COMPACT_DOUBLE => TType::DOUBLE,
        TCompactProtocol::COMPACT_BINARY => TType::STRING,
        TCompactProtocol::COMPACT_STRUCT => TType::STRUCT,
        TCompactProtocol::COMPACT_LIST => TType::LST,
        TCompactProtocol::COMPACT_SET => TType::SET,
        TCompactProtocol::COMPACT_MAP => TType::MAP,
        TCompactProtocol::COMPACT_UUID => TType::UUID,
    ];

    protected int $state = TCompactProtocol::STATE_CLEAR;
    protected int $lastFid = 0;
    protected int $boolFid = 0;
    protected ?bool $boolValue = null;
    /** @var list<array{0: int, 1: int}> */
    protected array $structs = [];
    /** @var list<int> */
    protected array $containers = [];

    // Some varint / zigzag helper methods
    public function toZigZag(int $n, int $bits): int
    {
        return ($n << 1) ^ ($n >> ($bits - 1));
    }

    public function fromZigZag(int $n): int
    {
        return ($n >> 1) ^ -($n & 1);
    }

    public function getVarint(int $data): string
    {
        $out = "";
        while (true) {
            if (($data & ~0x7f) === 0) {
                $out .= chr($data);
                break;
            } else {
                $out .= chr(($data & 0xff) | 0x80);
                $data = $data >> 7;
            }
        }

        return $out;
    }

    public function writeVarint(int $data): int
    {
        $out = $this->getVarint($data);
        $result = strlen($out);
        $this->trans->write($out);

        return $result;
    }

    public function readVarint(?int &$result): int
    {
        $idx = 0;
        $shift = 0;
        $result = 0;
        while ($idx < self::MAX_VARINT_BYTES) {
            $x = $this->trans->readAll(1);
            $arr = unpack('C', $x);
            $byte = $arr[1];
            $idx += 1;
            $result |= ($byte & 0x7f) << $shift;
            if (($byte >> 7) === 0) {
                return $idx;
            }
            $shift += 7;
        }
        throw new TProtocolException('Variable-length int over 10 bytes.', TProtocolException::INVALID_DATA);
    }

    public function __construct(TTransport $trans)
    {
        parent::__construct($trans);
    }

    public function writeMessageBegin(string $name, int $type, int $seqid): int
    {
        $written =
            $this->writeUByte(TCompactProtocol::PROTOCOL_ID) +
            $this->writeUByte(TCompactProtocol::VERSION |
                ($type << TCompactProtocol::TYPE_SHIFT_AMOUNT)) +
            $this->writeVarint($seqid) +
            $this->writeString($name);
        $this->state = TCompactProtocol::STATE_VALUE_WRITE;

        return $written;
    }

    public function writeMessageEnd(): int
    {
        $this->state = TCompactProtocol::STATE_CLEAR;

        return 0;
    }

    public function writeStructBegin(string $name): int
    {
        $this->structs[] = [$this->state, $this->lastFid];
        $this->state = TCompactProtocol::STATE_FIELD_WRITE;
        $this->lastFid = 0;

        return 0;
    }

    public function writeStructEnd(): int
    {
        $old_values = array_pop($this->structs);
        $this->state = $old_values[0];
        $this->lastFid = $old_values[1];

        return 0;
    }

    public function writeFieldStop(): int
    {
        return $this->writeByte(0);
    }

    public function writeFieldHeader(int $type, int $fid): int
    {
        $written = 0;
        $delta = $fid - $this->lastFid;
        if (0 < $delta && $delta <= 15) {
            $written = $this->writeUByte(($delta << 4) | $type);
        } else {
            $written = $this->writeByte($type) +
                $this->writeI16($fid);
        }
        $this->lastFid = $fid;

        return $written;
    }

    public function writeFieldBegin(string $field_name, int $field_type, int $field_id): int
    {
        if ($field_type == TType::BOOL) {
            $this->state = TCompactProtocol::STATE_BOOL_WRITE;
            $this->boolFid = $field_id;

            return 0;
        } else {
            $this->state = TCompactProtocol::STATE_VALUE_WRITE;

            return $this->writeFieldHeader(self::$ctypes[$field_type], $field_id);
        }
    }

    public function writeFieldEnd(): int
    {
        $this->state = TCompactProtocol::STATE_FIELD_WRITE;

        return 0;
    }

    public function writeCollectionBegin(int $etype, int $size): int
    {
        $written = 0;
        if ($size <= 14) {
            $written = $this->writeUByte($size << 4 |
                self::$ctypes[$etype]);
        } else {
            $written = $this->writeUByte(0xf0 |
                    self::$ctypes[$etype]) +
                $this->writeVarint($size);
        }
        $this->containers[] = $this->state;
        $this->state = TCompactProtocol::STATE_CONTAINER_WRITE;

        return $written;
    }

    public function writeMapBegin(int $key_type, int $val_type, int $size): int
    {
        $written = 0;
        if ($size == 0) {
            $written = $this->writeByte(0);
        } else {
            $written = $this->writeVarint($size) +
                $this->writeUByte(self::$ctypes[$key_type] << 4 |
                    self::$ctypes[$val_type]);
        }
        $this->containers[] = $this->state;

        return $written;
    }

    public function writeCollectionEnd(): int
    {
        $this->state = array_pop($this->containers);

        return 0;
    }

    public function writeMapEnd(): int
    {
        return $this->writeCollectionEnd();
    }

    public function writeListBegin(int $elem_type, int $size): int
    {
        return $this->writeCollectionBegin($elem_type, $size);
    }

    public function writeListEnd(): int
    {
        return $this->writeCollectionEnd();
    }

    public function writeSetBegin(int $elem_type, int $size): int
    {
        return $this->writeCollectionBegin($elem_type, $size);
    }

    public function writeSetEnd(): int
    {
        return $this->writeCollectionEnd();
    }

    public function writeBool(bool $bool): int
    {
        if ($this->state == TCompactProtocol::STATE_BOOL_WRITE) {
            $ctype = TCompactProtocol::COMPACT_FALSE;
            if ($bool) {
                $ctype = TCompactProtocol::COMPACT_TRUE;
            }

            return $this->writeFieldHeader($ctype, $this->boolFid);
        } elseif ($this->state == TCompactProtocol::STATE_CONTAINER_WRITE) {
            return $this->writeByte($bool ? 1 : 0);
        } else {
            throw new TProtocolException('Invalid state in compact protocol');
        }
    }

    public function writeByte(int $byte): int
    {
        $data = pack('c', $byte);
        $this->trans->write($data);

        return 1;
    }

    public function writeUByte(int $byte): int
    {
        $this->trans->write(pack('C', $byte));

        return 1;
    }

    public function writeI16(int $i16): int
    {
        $thing = $this->toZigZag($i16, 16);

        return $this->writeVarint($thing);
    }

    public function writeI32(int $i32): int
    {
        $thing = $this->toZigZag($i32, 32);

        return $this->writeVarint($thing);
    }

    public function writeDouble(float $dub): int
    {
        $data = pack('d', $dub);
        $this->trans->write($data);

        return 8;
    }

    public function writeString(string $str): int
    {
        $len = strlen($str);
        $result = $this->writeVarint($len);
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

    public function readFieldBegin(?string &$name, ?int &$field_type, ?int &$field_id): int
    {
        $result = $this->readUByte($compact_type_and_delta);

        $compact_type = $compact_type_and_delta & 0x0f;

        if ($compact_type == TType::STOP) {
            $field_type = $compact_type;
            $field_id = 0;

            return $result;
        }
        $delta = $compact_type_and_delta >> 4;
        if ($delta == 0) {
            $result += $this->readI16($field_id);
        } else {
            $field_id = $this->lastFid + $delta;
        }
        $this->lastFid = $field_id;
        $field_type = $this->getTType($compact_type);

        if ($compact_type == TCompactProtocol::COMPACT_TRUE) {
            $this->state = TCompactProtocol::STATE_BOOL_READ;
            $this->boolValue = true;
        } elseif ($compact_type == TCompactProtocol::COMPACT_FALSE) {
            $this->state = TCompactProtocol::STATE_BOOL_READ;
            $this->boolValue = false;
        } else {
            $this->state = TCompactProtocol::STATE_VALUE_READ;
        }

        return $result;
    }

    public function readFieldEnd(): int
    {
        $this->state = TCompactProtocol::STATE_FIELD_READ;

        return 0;
    }

    public function readUByte(?int &$value): int
    {
        $data = $this->trans->readAll(1);
        $arr = unpack('C', $data);
        $value = $arr[1];

        return 1;
    }

    public function readByte(?int &$byte): int
    {
        $data = $this->trans->readAll(1);
        $arr = unpack('c', $data);
        $byte = $arr[1];

        return 1;
    }

    public function readZigZag(?int &$value): int
    {
        $result = $this->readVarint($value);
        $value = $this->fromZigZag($value);

        return $result;
    }

    public function readMessageBegin(?string &$name, ?int &$type, ?int &$seqid): int
    {
        $protoId = 0;
        $result = $this->readUByte($protoId);
        if ($protoId != TCompactProtocol::PROTOCOL_ID) {
            throw new TProtocolException('Bad protocol id in TCompact message');
        }
        $verType = 0;
        $result += $this->readUByte($verType);
        $type = ($verType >> TCompactProtocol::TYPE_SHIFT_AMOUNT) & TCompactProtocol::TYPE_BITS;
        $version = $verType & TCompactProtocol::VERSION_MASK;
        if ($version != TCompactProtocol::VERSION) {
            throw new TProtocolException('Bad version in TCompact message');
        }
        $result += $this->readVarint($seqid);
        $result += $this->readString($name);

        return $result;
    }

    public function readMessageEnd(): int
    {
        return 0;
    }

    public function readStructBegin(?string &$name): int
    {
        $name = ''; // unused
        $this->structs[] = [$this->state, $this->lastFid];
        $this->state = TCompactProtocol::STATE_FIELD_READ;
        $this->lastFid = 0;

        return 0;
    }

    public function readStructEnd(): int
    {
        $last = array_pop($this->structs);
        $this->state = $last[0];
        $this->lastFid = $last[1];

        return 0;
    }

    public function readCollectionBegin(?int &$type, ?int &$size): int
    {
        $sizeType = 0;
        $result = $this->readUByte($sizeType);
        $size = $sizeType >> 4;
        $type = $this->getTType($sizeType);
        if ($size == 15) {
            $result += $this->readVarint($size);
        }
        $this->containers[] = $this->state;
        $this->state = TCompactProtocol::STATE_CONTAINER_READ;

        return $result;
    }

    public function readMapBegin(?int &$key_type, ?int &$val_type, ?int &$size): int
    {
        $result = $this->readVarint($size);
        $types = 0;
        if ($size > 0) {
            $result += $this->readUByte($types);
        }
        $val_type = $this->getTType($types);
        $key_type = $this->getTType($types >> 4);
        $this->containers[] = $this->state;
        $this->state = TCompactProtocol::STATE_CONTAINER_READ;

        return $result;
    }

    public function readCollectionEnd(): int
    {
        $this->state = array_pop($this->containers);

        return 0;
    }

    public function readMapEnd(): int
    {
        return $this->readCollectionEnd();
    }

    public function readListBegin(?int &$elem_type, ?int &$size): int
    {
        return $this->readCollectionBegin($elem_type, $size);
    }

    public function readListEnd(): int
    {
        return $this->readCollectionEnd();
    }

    public function readSetBegin(?int &$elem_type, ?int &$size): int
    {
        return $this->readCollectionBegin($elem_type, $size);
    }

    public function readSetEnd(): int
    {
        return $this->readCollectionEnd();
    }

    public function readBool(?bool &$bool): int
    {
        if ($this->state == TCompactProtocol::STATE_BOOL_READ) {
            $bool = $this->boolValue;

            return 0;
        } elseif ($this->state == TCompactProtocol::STATE_CONTAINER_READ) {
            return $this->readByte($bool);
        } else {
            throw new TProtocolException('Invalid state in compact protocol');
        }
    }

    public function readI16(?int &$i16): int
    {
        return $this->readZigZag($i16);
    }

    public function readI32(?int &$i32): int
    {
        return $this->readZigZag($i32);
    }

    public function readDouble(?float &$dub): int
    {
        $data = $this->trans->readAll(8);
        $arr = unpack('d', $data);
        $dub = $arr[1];

        return 8;
    }

    public function readString(?string &$str): int
    {
        $result = $this->readVarint($len);
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

    public function getTType(int $byte): int
    {
        return self::$ttypes[$byte & 0x0f];
    }

    // If we are on a 32bit architecture we have to explicitly deal with
    // 64-bit twos-complement arithmetic since PHP wants to treat all ints
    // as signed and any int over 2^31 - 1 as a float

    // Read and write I64 as two 32 bit numbers $hi and $lo

    public function readI64(?int &$i64): int
    {
        // Read varint from wire
        $hi = 0;
        $lo = 0;

        $idx = 0;
        $shift = 0;

        while (true) {
            $x = $this->trans->readAll(1);
            $arr = unpack('C', $x);
            $byte = $arr[1];
            $idx += 1;
            // Shift hi and lo together.
            if ($shift < 28) {
                $lo |= (($byte & 0x7f) << $shift);
            } elseif ($shift == 28) {
                $lo |= (($byte & 0x0f) << 28);
                $hi |= (($byte & 0x70) >> 4);
            } else {
                $hi |= (($byte & 0x7f) << ($shift - 32));
            }
            if (($byte >> 7) === 0) {
                break;
            }
            $shift += 7;
        }

        // Now, unzig it.
        $xorer = 0;
        if ($lo & 1) {
            $xorer = 0xffffffff;
        }
        $lo = ($lo >> 1) & 0x7fffffff;
        $lo = $lo | (($hi & 1) << 31);
        $hi = ($hi >> 1) ^ $xorer;
        $lo = $lo ^ $xorer;

        // Now put $hi and $lo back together
        $isNeg = $hi < 0 || $hi & 0x80000000;

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

        // Force 32bit words in excess of 2G to be positive - we deal with sign
        // explicitly below

        if ($hi & (int)0x80000000) {
            $hi &= (int)0x7fffffff;
            $hi += 0x80000000;
        }

        if ($lo & (int)0x80000000) {
            $lo &= (int)0x7fffffff;
            $lo += 0x80000000;
        }

        // Create as negative value first, since we can store -2^63 but not 2^63
        $i64 = -$hi * 4294967296 - $lo;

        if (!$isNeg) {
            $i64 = -$i64;
        }

        return $idx;
    }

    public function writeI64(int $i64): int
    {
        if ($i64 === PHP_INT_MIN) {
            // PHP_INT_MIN (-2^63) cannot be safely negated: -PHP_INT_MIN overflows
            // the 64-bit signed integer range. Its zigzag encoding is the maximum
            // unsigned 64-bit varint (0xFFFFFFFFFFFFFFFF), so we write it directly.

            $out = "\xff\xff\xff\xff\xff\xff\xff\xff\xff\x01";
            $this->trans->write($out);

            return 10;
        } elseif (($i64 > 4294967296) || ($i64 < -4294967296)) {
            // Convert $i64 to $hi and $lo
            $neg = $i64 < 0;

            if ($neg) {
                $i64 *= -1;
            }

            $hi = (int)$i64 >> 32;
            $lo = (int)$i64 & 0xffffffff;

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

            // Now do the zigging and zagging.
            $xorer = 0;
            if ($neg) {
                $xorer = 0xffffffff;
            }
            $lowbit = ($lo >> 31) & 1;
            $hi = ($hi << 1) | $lowbit;
            $lo = ($lo << 1);
            $lo = ($lo ^ $xorer) & 0xffffffff;
            $hi = ($hi ^ $xorer) & 0xffffffff;

            // now write out the varint, ensuring we shift both hi and lo
            $out = "";
            while (true) {
                if (
                    ($lo & ~0x7f) === 0 &&
                    $hi === 0
                ) {
                    $out .= chr($lo);
                    break;
                } else {
                    $out .= chr(($lo & 0xff) | 0x80);
                    $lo = $lo >> 7;
                    $lo = $lo | ($hi << 25);
                    $hi = $hi >> 7;
                    // Right shift carries sign, but we don't want it to.
                    $hi = $hi & (127 << 25);
                }
            }

            $ret = strlen($out);
            $this->trans->write($out);

            return $ret;
        } else {
            return $this->writeVarint($this->toZigZag($i64, 64));
        }
    }
}
