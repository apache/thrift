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
use Thrift\Type\TType;
use Thrift\Exception\TProtocolException;
use Thrift\Protocol\JSON\BaseContext;
use Thrift\Protocol\JSON\LookaheadReader;
use Thrift\Protocol\JSON\PairContext;
use Thrift\Protocol\JSON\ListContext;

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

    private function getTypeNameForTypeID($typeID)
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

    private function getTypeIDForTypeName($name)
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

    private function pushContext($c)
    {
        array_push($this->contextStack, $this->context);
        $this->context = $c;
    }

    private function popContext()
    {
        $this->context = array_pop($this->contextStack);
    }

    public function __construct($trans)
    {
        parent::__construct($trans);
        $this->context = new BaseContext();
        $this->reader = new LookaheadReader($this);
    }

    public function reset()
    {
        $this->contextStack = [];
        $this->context = new BaseContext();
        $this->reader = new LookaheadReader($this);
    }

    public function readJSONSyntaxChar($b)
    {
        $ch = $this->reader->read();

        if (substr($ch, 0, 1) != $b) {
            throw new TProtocolException("Unexpected character: " . $ch, TProtocolException::INVALID_DATA);
        }
    }

    private function writeJSONString($b)
    {
        $this->context->write();

        if (is_numeric($b) && $this->context->escapeNum()) {
            $this->trans->write(self::QUOTE . $b . self::QUOTE);
            return;
        }

        $this->trans->write(json_encode($b, JSON_UNESCAPED_UNICODE | JSON_UNESCAPED_SLASHES));
    }

    private function writeJSONInteger(int $num)
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

    private function writeJSONObjectStart()
    {
        $this->context->write();
        $this->trans->write(self::LBRACE);
        $this->pushContext(new PairContext($this));
    }

    private function writeJSONObjectEnd()
    {
        $this->popContext();
        $this->trans->write(self::RBRACE);
    }

    private function writeJSONArrayStart()
    {
        $this->context->write();
        $this->trans->write(self::LBRACKET);
        $this->pushContext(new ListContext($this));
    }

    private function writeJSONArrayEnd()
    {
        $this->popContext();
        $this->trans->write(self::RBRACKET);
    }

    private function readJSONString($skipContext)
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

    private function isJSONNumeric($b)
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

    private function readJSONNumericChars()
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

    private function readJSONInteger()
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
    private function readJSONIntegerAsString()
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

    private function readJSONDouble()
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

    private function readJSONObjectStart()
    {
        $this->context->read();
        $this->readJSONSyntaxChar(self::LBRACE);
        $this->pushContext(new PairContext($this));
    }

    private function readJSONObjectEnd()
    {
        $this->readJSONSyntaxChar(self::RBRACE);
        $this->popContext();
    }

    private function readJSONArrayStart()
    {
        $this->context->read();
        $this->readJSONSyntaxChar(self::LBRACKET);
        $this->pushContext(new ListContext($this));
    }

    private function readJSONArrayEnd()
    {
        $this->readJSONSyntaxChar(self::RBRACKET);
        $this->popContext();
    }

    /**
     * Writes the message header
     *
     * @param string $name Function name
     * @param int $type message type TMessageType::CALL or TMessageType::REPLY
     * @param int $seqid The sequence id of this message
     */
    public function writeMessageBegin($name, $type, $seqid)
    {
        $this->writeJSONArrayStart();
        $this->writeJSONInteger(self::VERSION);
        $this->writeJSONString($name);
        $this->writeJSONInteger($type);
        $this->writeJSONInteger($seqid);
    }

    /**
     * Close the message
     */
    public function writeMessageEnd()
    {
        $this->writeJSONArrayEnd();
    }

    /**
     * Writes a struct header.
     *
     * @param  string $name Struct name
     * @throws TException on write error
     * @return int        How many bytes written
     */
    public function writeStructBegin($name)
    {
        $this->writeJSONObjectStart();
    }

    /**
     * Close a struct.
     *
     * @throws TException on write error
     * @return int        How many bytes written
     */
    public function writeStructEnd()
    {
        $this->writeJSONObjectEnd();
    }

    public function writeFieldBegin($fieldName, $fieldType, $fieldId)
    {
        $this->writeJSONInteger($fieldId);
        $this->writeJSONObjectStart();
        $this->writeJSONString($this->getTypeNameForTypeID($fieldType));
    }

    public function writeFieldEnd()
    {
        $this->writeJsonObjectEnd();
    }

    public function writeFieldStop()
    {
    }

    public function writeMapBegin($keyType, $valType, $size)
    {
        $this->writeJSONArrayStart();
        $this->writeJSONString($this->getTypeNameForTypeID($keyType));
        $this->writeJSONString($this->getTypeNameForTypeID($valType));
        $this->writeJSONInteger($size);
        $this->writeJSONObjectStart();
    }

    public function writeMapEnd()
    {
        $this->writeJSONObjectEnd();
        $this->writeJSONArrayEnd();
    }

    public function writeListBegin($elemType, $size)
    {
        $this->writeJSONArrayStart();
        $this->writeJSONString($this->getTypeNameForTypeID($elemType));
        $this->writeJSONInteger($size);
    }

    public function writeListEnd()
    {
        $this->writeJSONArrayEnd();
    }

    public function writeSetBegin($elemType, $size)
    {
        $this->writeJSONArrayStart();
        $this->writeJSONString($this->getTypeNameForTypeID($elemType));
        $this->writeJSONInteger($size);
    }

    public function writeSetEnd()
    {
        $this->writeJSONArrayEnd();
    }

    public function writeBool($bool)
    {
        $this->writeJSONInteger($bool ? 1 : 0);
    }

    public function writeByte($byte)
    {
        $this->writeJSONInteger($byte);
    }

    public function writeI16($i16)
    {
        $this->writeJSONInteger($i16);
    }

    public function writeI32($i32)
    {
        $this->writeJSONInteger($i32);
    }

    public function writeI64($i64)
    {
        $this->writeJSONInteger($i64);
    }

    public function writeDouble($dub)
    {
        $this->writeJSONDouble($dub);
    }

    public function writeString(string $str)
    {
        $this->writeJSONString($str);
    }

    public function writeUuid($uuid)
    {
        $this->writeJSONString($uuid);
    }

    /**
     * Reads the message header
     *
     * @param string $name Function name
     * @param int $type message type TMessageType::CALL or TMessageType::REPLY
     * @parem int $seqid The sequence id of this message
     */
    public function readMessageBegin(&$name, &$type, &$seqid)
    {
        $this->readJSONArrayStart();

        if ($this->readJSONInteger() != self::VERSION) {
            throw new TProtocolException("Message contained bad version", TProtocolException::BAD_VERSION);
        }

        $name = $this->readJSONString(false);
        $type = $this->readJSONInteger();
        $seqid = $this->readJSONInteger();

        return true;
    }

    /**
     * Read the close of message
     */
    public function readMessageEnd()
    {
        $this->readJSONArrayEnd();
    }

    public function readStructBegin(&$name)
    {
        $this->readJSONObjectStart();

        return 0;
    }

    public function readStructEnd()
    {
        $this->readJSONObjectEnd();
    }

    public function readFieldBegin(&$name, &$fieldType, &$fieldId)
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
    }

    public function readFieldEnd()
    {
        $this->readJSONObjectEnd();
    }

    public function readMapBegin(&$keyType, &$valType, &$size)
    {
        $this->readJSONArrayStart();
        $keyType = $this->getTypeIDForTypeName($this->readJSONString(false));
        $valType = $this->getTypeIDForTypeName($this->readJSONString(false));
        $size = $this->readJSONInteger();
        $this->readJSONObjectStart();
    }

    public function readMapEnd()
    {
        $this->readJSONObjectEnd();
        $this->readJSONArrayEnd();
    }

    public function readListBegin(&$elemType, &$size)
    {
        $this->readJSONArrayStart();
        $elemType = $this->getTypeIDForTypeName($this->readJSONString(false));
        $size = $this->readJSONInteger();

        return true;
    }

    public function readListEnd()
    {
        $this->readJSONArrayEnd();
    }

    public function readSetBegin(&$elemType, &$size)
    {
        $this->readJSONArrayStart();
        $elemType = $this->getTypeIDForTypeName($this->readJSONString(false));
        $size = $this->readJSONInteger();

        return true;
    }

    public function readSetEnd()
    {
        $this->readJSONArrayEnd();
    }

    public function readBool(&$bool)
    {
        $bool = $this->readJSONInteger() == 0 ? false : true;

        return true;
    }

    public function readByte(&$byte)
    {
        $byte = $this->readJSONInteger();

        return true;
    }

    public function readI16(&$i16)
    {
        $i16 = $this->readJSONInteger();

        return true;
    }

    public function readI32(&$i32)
    {
        $i32 = $this->readJSONInteger();

        return true;
    }

    public function readI64(&$i64)
    {
        if (PHP_INT_SIZE === 4) {
            $i64 = $this->readJSONIntegerAsString();
        } else {
            $i64 = $this->readJSONInteger();
        }

        return true;
    }

    public function readDouble(&$dub)
    {
        $dub = $this->readJSONDouble();

        return true;
    }

    public function readString(&$str)
    {
        $str = $this->readJSONString(false);

        return true;
    }

    public function readUuid(&$uuid)
    {
        $uuid = $this->readJSONString(false);

        return true;
    }
}
