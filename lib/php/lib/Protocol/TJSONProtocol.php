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
use Thrift\Exception\TProtocolException;
use Thrift\Protocol\JSON\BaseContext;
use Thrift\Protocol\JSON\ListContext;
use Thrift\Protocol\JSON\LookaheadReader;
use Thrift\Protocol\JSON\PairContext;
use Thrift\Transport\TTransport;
use Thrift\Type\TType;

/**
 * JSON implementation of thrift protocol, ported from Java.
 */
class TJSONProtocol extends TProtocol
{
    public const COMMA = ',';
    public const COLON = ':';
    public const LBRACE = '{';
    public const RBRACE = '}';
    public const LBRACKET = '[';
    public const RBRACKET = ']';
    public const QUOTE = '"';
    public const BACKSLASH = '\\';
    public const ZERO = '0';
    public const ESCSEQ = '\\';
    public const DOUBLEESC = '__DOUBLE_ESCAPE_SEQUENCE__';

    public const VERSION = 1;

    public static $JSON_CHAR_TABLE = [
        /*  0   1   2   3   4   5   6   7   8   9   A   B   C   D   E   F */
        0, 0, 0, 0, 0, 0, 0, 0, 'b', 't', 'n', 0, 'f', 'r', 0, 0, // 0
        0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, // 1
        1, 1, '"', 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, // 2
    ];

    public static $ESCAPE_CHARS = ['"', '\\', '/', "b", "f", "n", "r", "t"];

    public static $ESCAPE_CHAR_VALS = [
        '"', '\\', '/', "\x08", "\f", "\n", "\r", "\t",
    ];

    public const NAME_BOOL = "tf";
    public const NAME_BYTE = "i8";
    public const NAME_I16 = "i16";
    public const NAME_I32 = "i32";
    public const NAME_I64 = "i64";
    public const NAME_DOUBLE = "dbl";
    public const NAME_STRUCT = "rec";
    public const NAME_STRING = "str";
    public const NAME_MAP = "map";
    public const NAME_LIST = "lst";
    public const NAME_SET = "set";
    public const NAME_UUID = "uid";

    /** Quoted tokens used to round-trip non-finite doubles through the JSON protocol. */
    public const TOKEN_NAN = "NaN";
    public const TOKEN_POS_INFINITY = "Infinity";
    public const TOKEN_NEG_INFINITY = "-Infinity";

    private function getTypeNameForTypeID(int $typeID): string
    {
        switch ($typeID) {
            case TType::BOOL:
                return self::NAME_BOOL;
            case TType::BYTE:
                return self::NAME_BYTE;
            case TType::I16:
                return self::NAME_I16;
            case TType::I32:
                return self::NAME_I32;
            case TType::I64:
                return self::NAME_I64;
            case TType::DOUBLE:
                return self::NAME_DOUBLE;
            case TType::STRING:
                return self::NAME_STRING;
            case TType::STRUCT:
                return self::NAME_STRUCT;
            case TType::MAP:
                return self::NAME_MAP;
            case TType::SET:
                return self::NAME_SET;
            case TType::LST:
                return self::NAME_LIST;
            case TType::UUID:
                return self::NAME_UUID;
            default:
                throw new TProtocolException("Unrecognized type", TProtocolException::UNKNOWN);
        }
    }

    private function getTypeIDForTypeName(string $name): int
    {
        $result = TType::STOP;

        if (strlen((string) $name) > 1) {
            switch (substr($name, 0, 1)) {
                case 'd':
                    $result = TType::DOUBLE;
                    break;
                case 'i':
                    switch (substr($name, 1, 1)) {
                        case '8':
                            $result = TType::BYTE;
                            break;
                        case '1':
                            $result = TType::I16;
                            break;
                        case '3':
                            $result = TType::I32;
                            break;
                        case '6':
                            $result = TType::I64;
                            break;
                    }
                    break;
                case 'l':
                    $result = TType::LST;
                    break;
                case 'm':
                    $result = TType::MAP;
                    break;
                case 'r':
                    $result = TType::STRUCT;
                    break;
                case 's':
                    if (substr($name, 1, 1) == 't') {
                        $result = TType::STRING;
                    } elseif (substr($name, 1, 1) == 'e') {
                        $result = TType::SET;
                    }
                    break;
                case 't':
                    $result = TType::BOOL;
                    break;
                case 'u':
                    $result = TType::UUID;
                    break;
            }
        }
        if ($result == TType::STOP) {
            throw new TProtocolException("Unrecognized type", TProtocolException::INVALID_DATA);
        }

        return $result;
    }

    /** @var list<BaseContext> */
    private array $contextStack = [];
    private BaseContext $context;
    private LookaheadReader $reader;

    private function pushContext(BaseContext $c): void
    {
        array_push($this->contextStack, $this->context);
        $this->context = $c;
    }

    private function popContext(): void
    {
        $this->context = array_pop($this->contextStack) ?? new BaseContext();
    }

    public function __construct(TTransport $trans)
    {
        parent::__construct($trans);
        $this->context = new BaseContext();
        $this->reader = new LookaheadReader($this);
    }

    public function reset(): void
    {
        $this->contextStack = [];
        $this->context = new BaseContext();
        $this->reader = new LookaheadReader($this);
    }

    public function readJSONSyntaxChar(string $b): void
    {
        $ch = $this->reader->read();

        if (substr($ch, 0, 1) != $b) {
            throw new TProtocolException("Unexpected character: " . $ch, TProtocolException::INVALID_DATA);
        }
    }

    private function writeJSONString(mixed $b): void
    {
        $this->context->write();

        if (is_numeric($b) && $this->context->escapeNum()) {
            $this->trans->write(self::QUOTE . $b . self::QUOTE);
            return;
        }

        $this->trans->write(json_encode($b, JSON_UNESCAPED_UNICODE | JSON_UNESCAPED_SLASHES));
    }

    private function writeJSONInteger(int $num): void
    {
        $this->context->write();

        if ($this->context->escapeNum()) {
            $this->trans->write(self::QUOTE);
        }

        $this->trans->write((string) $num);

        if ($this->context->escapeNum()) {
            $this->trans->write(self::QUOTE);
        }
    }

    private function writeJSONDouble(float $num): void
    {
        $this->context->write();

        if (is_nan($num)) {
            $this->trans->write(self::QUOTE . self::TOKEN_NAN . self::QUOTE);
            return;
        }

        if (is_infinite($num)) {
            $token = $num > 0 ? self::TOKEN_POS_INFINITY : self::TOKEN_NEG_INFINITY;
            $this->trans->write(self::QUOTE . $token . self::QUOTE);
            return;
        }

        if ($this->context->escapeNum()) {
            $this->trans->write(self::QUOTE);
        }

        $this->trans->write(json_encode($num));

        if ($this->context->escapeNum()) {
            $this->trans->write(self::QUOTE);
        }
    }

    private function writeJSONObjectStart(): void
    {
        $this->context->write();
        $this->trans->write(self::LBRACE);
        $this->pushContext(new PairContext($this));
    }

    private function writeJSONObjectEnd(): void
    {
        $this->popContext();
        $this->trans->write(self::RBRACE);
    }

    private function writeJSONArrayStart(): void
    {
        $this->context->write();
        $this->trans->write(self::LBRACKET);
        $this->pushContext(new ListContext($this));
    }

    private function writeJSONArrayEnd(): void
    {
        $this->popContext();
        $this->trans->write(self::RBRACKET);
    }

    private function readJSONString(bool $skipContext): mixed
    {
        if (!$skipContext) {
            $this->context->read();
        }

        $jsonString = '';
        $lastChar = null;
        while (true) {
            $ch = $this->reader->read();
            $jsonString .= $ch;
            if (
                $ch == self::QUOTE &&
                $lastChar !== null &&
                $lastChar !== self::ESCSEQ
            ) {
                break;
            }
            if ($ch == self::ESCSEQ && $lastChar == self::ESCSEQ) {
                $lastChar = self::DOUBLEESC;
            } else {
                $lastChar = $ch;
            }
        }

        return json_decode($jsonString);
    }

    private function isJSONNumeric(string $b): bool
    {
        switch ($b) {
            case '+':
            case '-':
            case '.':
            case '0':
            case '1':
            case '2':
            case '3':
            case '4':
            case '5':
            case '6':
            case '7':
            case '8':
            case '9':
            case 'E':
            case 'e':
                return true;
        }

        return false;
    }

    private function readJSONNumericChars(): string
    {
        $strbld = [];

        while (true) {
            $ch = $this->reader->peek();

            if (!$this->isJSONNumeric($ch)) {
                break;
            }

            $strbld[] = $this->reader->read();
        }

        return implode("", $strbld);
    }

    private function readJSONInteger(): int
    {
        $this->context->read();

        if ($this->context->escapeNum()) {
            $this->readJSONSyntaxChar(self::QUOTE);
        }

        $str = $this->readJSONNumericChars();

        if ($this->context->escapeNum()) {
            $this->readJSONSyntaxChar(self::QUOTE);
        }

        if (!is_numeric($str)) {
            throw new TProtocolException("Invalid data in numeric: " . $str, TProtocolException::INVALID_DATA);
        }

        return intval($str);
    }

    /**
     * Identical to readJSONInteger but without the final cast.
     * Needed for proper handling of i64 on 32 bit machines.  Why a
     * separate function?  So we don't have to force the rest of the
     * use cases through the extra conditional.
     */
    private function readJSONIntegerAsString(): string
    {
        $this->context->read();

        if ($this->context->escapeNum()) {
            $this->readJSONSyntaxChar(self::QUOTE);
        }

        $str = $this->readJSONNumericChars();

        if ($this->context->escapeNum()) {
            $this->readJSONSyntaxChar(self::QUOTE);
        }

        if (!is_numeric($str)) {
            throw new TProtocolException("Invalid data in numeric: " . $str, TProtocolException::INVALID_DATA);
        }

        return $str;
    }

    private function readJSONDouble(): float
    {
        $this->context->read();

        if (substr($this->reader->peek(), 0, 1) == self::QUOTE) {
            $arr = $this->readJSONString(true);

            if ($arr === self::TOKEN_NAN) {
                return NAN;
            } elseif ($arr === self::TOKEN_POS_INFINITY) {
                return INF;
            } elseif ($arr === self::TOKEN_NEG_INFINITY) {
                return -INF;
            } elseif (!$this->context->escapeNum()) {
                throw new TProtocolException(
                    "Numeric data unexpectedly quoted " . $arr,
                    TProtocolException::INVALID_DATA
                );
            }

            return floatval($arr);
        } else {
            if ($this->context->escapeNum()) {
                $this->readJSONSyntaxChar(self::QUOTE);
            }

            return floatval($this->readJSONNumericChars());
        }
    }

    private function readJSONObjectStart(): void
    {
        $this->context->read();
        $this->readJSONSyntaxChar(self::LBRACE);
        $this->pushContext(new PairContext($this));
    }

    private function readJSONObjectEnd(): void
    {
        $this->readJSONSyntaxChar(self::RBRACE);
        $this->popContext();
    }

    private function readJSONArrayStart(): void
    {
        $this->context->read();
        $this->readJSONSyntaxChar(self::LBRACKET);
        $this->pushContext(new ListContext($this));
    }

    private function readJSONArrayEnd(): void
    {
        $this->readJSONSyntaxChar(self::RBRACKET);
        $this->popContext();
    }

    /**
     * Note on return values:
     * TJSONProtocol does not track precise byte counts for read/write
     * operations; it instead matches the historical contract that
     * generated callers accumulate via `$xfer += $protocol->readX(...)`.
     * Methods that previously returned `true` (cast to 1 in accumulation)
     * keep returning `1`; methods that previously returned nothing or
     * `0` keep returning `0`. Recomputing actual byte counts would
     * require threading through the writeJSON* helpers and is out of
     * scope for the typing pass.
     */
    public function writeMessageBegin(string $name, int $type, int $seqid): int
    {
        $this->writeJSONArrayStart();
        $this->writeJSONInteger(self::VERSION);
        $this->writeJSONString($name);
        $this->writeJSONInteger($type);
        $this->writeJSONInteger($seqid);

        return 0;
    }

    public function writeMessageEnd(): int
    {
        $this->writeJSONArrayEnd();

        return 0;
    }

    /**
     * @throws TException on write error
     */
    public function writeStructBegin(string $name): int
    {
        $this->writeJSONObjectStart();

        return 0;
    }

    /**
     * @throws TException on write error
     */
    public function writeStructEnd(): int
    {
        $this->writeJSONObjectEnd();

        return 0;
    }

    public function writeFieldBegin(string $fieldName, int $fieldType, int $fieldId): int
    {
        $this->writeJSONInteger($fieldId);
        $this->writeJSONObjectStart();
        $this->writeJSONString($this->getTypeNameForTypeID($fieldType));

        return 0;
    }

    public function writeFieldEnd(): int
    {
        $this->writeJsonObjectEnd();

        return 0;
    }

    public function writeFieldStop(): int
    {
        return 0;
    }

    public function writeMapBegin(int $keyType, int $valType, int $size): int
    {
        $this->writeJSONArrayStart();
        $this->writeJSONString($this->getTypeNameForTypeID($keyType));
        $this->writeJSONString($this->getTypeNameForTypeID($valType));
        $this->writeJSONInteger($size);
        $this->writeJSONObjectStart();

        return 0;
    }

    public function writeMapEnd(): int
    {
        $this->writeJSONObjectEnd();
        $this->writeJSONArrayEnd();

        return 0;
    }

    public function writeListBegin(int $elemType, int $size): int
    {
        $this->writeJSONArrayStart();
        $this->writeJSONString($this->getTypeNameForTypeID($elemType));
        $this->writeJSONInteger($size);

        return 0;
    }

    public function writeListEnd(): int
    {
        $this->writeJSONArrayEnd();

        return 0;
    }

    public function writeSetBegin(int $elemType, int $size): int
    {
        $this->writeJSONArrayStart();
        $this->writeJSONString($this->getTypeNameForTypeID($elemType));
        $this->writeJSONInteger($size);

        return 0;
    }

    public function writeSetEnd(): int
    {
        $this->writeJSONArrayEnd();

        return 0;
    }

    public function writeBool(bool $bool): int
    {
        $this->writeJSONInteger($bool ? 1 : 0);

        return 0;
    }

    public function writeByte(int $byte): int
    {
        $this->writeJSONInteger($byte);

        return 0;
    }

    public function writeI16(int $i16): int
    {
        $this->writeJSONInteger($i16);

        return 0;
    }

    public function writeI32(int $i32): int
    {
        $this->writeJSONInteger($i32);

        return 0;
    }

    public function writeI64(int $i64): int
    {
        $this->writeJSONInteger($i64);

        return 0;
    }

    public function writeDouble(float $dub): int
    {
        $this->writeJSONDouble($dub);

        return 0;
    }

    public function writeString(string $str): int
    {
        $this->writeJSONString($str);

        return 0;
    }

    public function writeUuid(string $uuid): int
    {
        $this->writeJSONString($uuid);

        return 0;
    }

    public function readMessageBegin(?string &$name, ?int &$type, ?int &$seqid): int
    {
        $this->readJSONArrayStart();

        if ($this->readJSONInteger() != self::VERSION) {
            throw new TProtocolException("Message contained bad version", TProtocolException::BAD_VERSION);
        }

        $name = $this->readJSONString(false);
        $type = $this->readJSONInteger();
        $seqid = $this->readJSONInteger();

        return 1;
    }

    public function readMessageEnd(): int
    {
        $this->readJSONArrayEnd();

        return 0;
    }

    public function readStructBegin(?string &$name): int
    {
        $this->readJSONObjectStart();

        return 0;
    }

    public function readStructEnd(): int
    {
        $this->readJSONObjectEnd();

        return 0;
    }

    public function readFieldBegin(?string &$name, ?int &$fieldType, ?int &$fieldId): int
    {
        $ch = $this->reader->peek();
        $name = "";

        if (substr($ch, 0, 1) == self::RBRACE) {
            $fieldType = TType::STOP;
        } else {
            $fieldId = $this->readJSONInteger();
            $this->readJSONObjectStart();
            $fieldType = $this->getTypeIDForTypeName($this->readJSONString(false));
        }

        return 0;
    }

    public function readFieldEnd(): int
    {
        $this->readJSONObjectEnd();

        return 0;
    }

    public function readMapBegin(?int &$keyType, ?int &$valType, ?int &$size): int
    {
        $this->readJSONArrayStart();
        $keyType = $this->getTypeIDForTypeName($this->readJSONString(false));
        $valType = $this->getTypeIDForTypeName($this->readJSONString(false));
        $size = $this->readJSONInteger();
        $this->readJSONObjectStart();

        return 0;
    }

    public function readMapEnd(): int
    {
        $this->readJSONObjectEnd();
        $this->readJSONArrayEnd();

        return 0;
    }

    public function readListBegin(?int &$elemType, ?int &$size): int
    {
        $this->readJSONArrayStart();
        $elemType = $this->getTypeIDForTypeName($this->readJSONString(false));
        $size = $this->readJSONInteger();

        return 1;
    }

    public function readListEnd(): int
    {
        $this->readJSONArrayEnd();

        return 0;
    }

    public function readSetBegin(?int &$elemType, ?int &$size): int
    {
        $this->readJSONArrayStart();
        $elemType = $this->getTypeIDForTypeName($this->readJSONString(false));
        $size = $this->readJSONInteger();

        return 1;
    }

    public function readSetEnd(): int
    {
        $this->readJSONArrayEnd();

        return 0;
    }

    public function readBool(?bool &$bool): int
    {
        $bool = $this->readJSONInteger() == 0 ? false : true;

        return 1;
    }

    public function readByte(?int &$byte): int
    {
        $byte = $this->readJSONInteger();

        return 1;
    }

    public function readI16(?int &$i16): int
    {
        $i16 = $this->readJSONInteger();

        return 1;
    }

    public function readI32(?int &$i32): int
    {
        $i32 = $this->readJSONInteger();

        return 1;
    }

    public function readI64(?int &$i64): int
    {
        if (PHP_INT_SIZE === 4) {
            $i64 = $this->readJSONIntegerAsString();
        } else {
            $i64 = $this->readJSONInteger();
        }

        return 1;
    }

    public function readDouble(?float &$dub): int
    {
        $dub = $this->readJSONDouble();

        return 1;
    }

    public function readString(?string &$str): int
    {
        $str = $this->readJSONString(false);

        return 1;
    }

    public function readUuid(?string &$uuid): int
    {
        $uuid = $this->readJSONString(false);

        return 1;
    }
}
