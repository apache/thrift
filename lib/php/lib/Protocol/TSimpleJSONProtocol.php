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
use Thrift\Transport\TTransport;
use Thrift\Protocol\SimpleJSON\Context;
use Thrift\Protocol\SimpleJSON\ListContext;
use Thrift\Protocol\SimpleJSON\StructContext;
use Thrift\Protocol\SimpleJSON\MapContext;
use Thrift\Protocol\SimpleJSON\CollectionMapKeyException;

/**
 * SimpleJSON implementation of thrift protocol, ported from Java.
 */
class TSimpleJSONProtocol extends TProtocol
{
    public const COMMA = ',';
    public const COLON = ':';
    public const LBRACE = '{';
    public const RBRACE = '}';
    public const LBRACKET = '[';
    public const RBRACKET = ']';
    public const QUOTE = '"';

    public const NAME_MAP = "map";
    public const NAME_LIST = "lst";
    public const NAME_SET = "set";

    protected ?Context $writeContext = null;
    /** @var list<Context|null> */
    protected array $writeContextStack = [];

    /**
     * Push a new write context onto the stack.
     */
    protected function pushWriteContext(Context $c)
    {
        $this->writeContextStack[] = $this->writeContext;
        $this->writeContext = $c;
    }

    /**
     * Pop the last write context off the stack
     */
    protected function popWriteContext()
    {
        $this->writeContext = array_pop($this->writeContextStack);
    }

    /**
     * Used to make sure that we are not encountering a map whose keys are containers
     */
    protected function assertContextIsNotMapKey($invalidKeyType)
    {
        if ($this->writeContext->isMapKey()) {
            throw new CollectionMapKeyException(
                "Cannot serialize a map with keys that are of type " .
                $invalidKeyType
            );
        }
    }

    private function writeJSONString($b)
    {
        $this->writeContext->write();

        $this->trans->write(json_encode((string)$b, JSON_UNESCAPED_SLASHES));
    }

    private function writeJSONInteger(int $num)
    {
        $isMapKey = $this->writeContext->isMapKey();

        $this->writeContext->write();

        if ($isMapKey) {
            $this->trans->write(self::QUOTE);
        }

        $this->trans->write((string) $num);

        if ($isMapKey) {
            $this->trans->write(self::QUOTE);
        }
    }

    private function writeJSONDouble(float $num): void
    {
        $isMapKey = $this->writeContext->isMapKey();
        $this->writeContext->write();

        if (is_nan($num)) {
            $this->trans->write(self::QUOTE . TJSONProtocol::TOKEN_NAN . self::QUOTE);
            return;
        }

        if (is_infinite($num)) {
            $token = $num > 0 ? TJSONProtocol::TOKEN_POS_INFINITY : TJSONProtocol::TOKEN_NEG_INFINITY;
            $this->trans->write(self::QUOTE . $token . self::QUOTE);
            return;
        }

        if ($isMapKey) {
            $this->trans->write(self::QUOTE);
        }

        $this->trans->write(json_encode($num));

        if ($isMapKey) {
            $this->trans->write(self::QUOTE);
        }
    }

    /**
     * Constructor
     */
    public function __construct(TTransport $trans)
    {
        parent::__construct($trans);
        $this->writeContext = new Context();
    }

    /**
     * TSimpleJSONProtocol does not track precise byte counts; all write
     * methods return 0 and read methods throw, since this protocol is
     * write-only by design (see class docblock below).
     */
    public function writeMessageBegin(string $name, int $type, int $seqid): int
    {
        $this->trans->write(self::LBRACKET);
        $this->pushWriteContext(new ListContext($this));
        $this->writeJSONString($name);
        $this->writeJSONInteger($type);
        $this->writeJSONInteger($seqid);

        return 0;
    }

    public function writeMessageEnd(): int
    {
        $this->popWriteContext();
        $this->trans->write(self::RBRACKET);

        return 0;
    }

    public function writeStructBegin(string $name): int
    {
        $this->writeContext->write();
        $this->trans->write(self::LBRACE);
        $this->pushWriteContext(new StructContext($this));

        return 0;
    }

    public function writeStructEnd(): int
    {
        $this->popWriteContext();
        $this->trans->write(self::RBRACE);

        return 0;
    }

    public function writeFieldBegin(string $fieldName, int $fieldType, int $fieldId): int
    {
        $this->writeJSONString($fieldName);

        return 0;
    }

    public function writeFieldEnd(): int
    {
        return 0;
    }

    public function writeFieldStop(): int
    {
        return 0;
    }

    public function writeMapBegin(int $keyType, int $valType, int $size): int
    {
        $this->assertContextIsNotMapKey(self::NAME_MAP);
        $this->writeContext->write();
        $this->trans->write(self::LBRACE);
        $this->pushWriteContext(new MapContext($this));

        return 0;
    }

    public function writeMapEnd(): int
    {
        $this->popWriteContext();
        $this->trans->write(self::RBRACE);

        return 0;
    }

    public function writeListBegin(int $elemType, int $size): int
    {
        $this->assertContextIsNotMapKey(self::NAME_LIST);
        $this->writeContext->write();
        $this->trans->write(self::LBRACKET);
        $this->pushWriteContext(new ListContext($this));
        // No metadata!

        return 0;
    }

    public function writeListEnd(): int
    {
        $this->popWriteContext();
        $this->trans->write(self::RBRACKET);

        return 0;
    }

    public function writeSetBegin(int $elemType, int $size): int
    {
        $this->assertContextIsNotMapKey(self::NAME_SET);
        $this->writeContext->write();
        $this->trans->write(self::LBRACKET);
        $this->pushWriteContext(new ListContext($this));
        // No metadata!

        return 0;
    }

    public function writeSetEnd(): int
    {
        $this->popWriteContext();
        $this->trans->write(self::RBRACKET);

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

    /**
     * Reading methods.
     *
     * simplejson is not meant to be read back into thrift
     * - see http://wiki.apache.org/thrift/ThriftUsageJava
     * - use JSON instead
     */

    public function readMessageBegin(?string &$name, ?int &$type, ?int &$seqid): int
    {
        throw new TException("Not implemented");
    }

    public function readMessageEnd(): int
    {
        throw new TException("Not implemented");
    }

    public function readStructBegin(?string &$name): int
    {
        throw new TException("Not implemented");
    }

    public function readStructEnd(): int
    {
        throw new TException("Not implemented");
    }

    public function readFieldBegin(?string &$name, ?int &$fieldType, ?int &$fieldId): int
    {
        throw new TException("Not implemented");
    }

    public function readFieldEnd(): int
    {
        throw new TException("Not implemented");
    }

    public function readMapBegin(?int &$keyType, ?int &$valType, ?int &$size): int
    {
        throw new TException("Not implemented");
    }

    public function readMapEnd(): int
    {
        throw new TException("Not implemented");
    }

    public function readListBegin(?int &$elemType, ?int &$size): int
    {
        throw new TException("Not implemented");
    }

    public function readListEnd(): int
    {
        throw new TException("Not implemented");
    }

    public function readSetBegin(?int &$elemType, ?int &$size): int
    {
        throw new TException("Not implemented");
    }

    public function readSetEnd(): int
    {
        throw new TException("Not implemented");
    }

    public function readBool(?bool &$bool): int
    {
        throw new TException("Not implemented");
    }

    public function readByte(?int &$byte): int
    {
        throw new TException("Not implemented");
    }

    public function readI16(?int &$i16): int
    {
        throw new TException("Not implemented");
    }

    public function readI32(?int &$i32): int
    {
        throw new TException("Not implemented");
    }

    public function readI64(?int &$i64): int
    {
        throw new TException("Not implemented");
    }

    public function readDouble(?float &$dub): int
    {
        throw new TException("Not implemented");
    }

    public function readString(?string &$str): int
    {
        throw new TException("Not implemented");
    }

    public function readUuid(?string &$uuid): int
    {
        throw new TException("Not implemented");
    }
}
