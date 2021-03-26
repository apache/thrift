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

package org.apache.thrift.protocol;

import haxe.io.Bytes;
import haxe.io.BytesInput;
import haxe.io.BytesOutput;
import haxe.io.BytesBuffer;
import haxe.Int64;

import org.apache.thrift.TException;
import org.apache.thrift.transport.TTransport;

/**
* Binary protocol implementation for thrift.
*/
class TBinaryProtocol extends TProtocolImplBase implements TProtocol {

    private static var ANONYMOUS_STRUCT:TStruct = new TStruct();

    private static inline var VERSION_MASK : haxe.Int32 = 0xffff0000;
    private static inline var VERSION_1 : haxe.Int32 = 0x80010000;

    private var strictRead_ : Bool = false;
    private var strictWrite_ : Bool = true;

    /**
     * Constructor
     */
    public function new(transport:TTransport, strictRead : Bool = false, strictWrite : Bool = true) {
		super(transport);
		strictRead_ = strictRead;
		strictWrite_ = strictWrite;
    }

    public function writeMessageBegin(message:TMessage) : Void {
        if (strictWrite_) {
          var version : Int = VERSION_1 | message.type;
          writeI32(version);
          writeString(message.name);
          writeI32(message.seqid);
        } else {
          writeString(message.name);
          writeByte(message.type);
          writeI32(message.seqid);
        }
    }

    public function writeMessageEnd() : Void {}

    public function writeStructBegin(struct:TStruct) : Void {}

    public function writeStructEnd() : Void {}

    public function writeFieldBegin(field:TField) : Void {
      writeByte(field.type);
      writeI16(field.id);
    }

    public function writeFieldEnd() : Void {}

    public function writeFieldStop() : Void {
      writeByte(TType.STOP);
    }

    public function writeMapBegin(map:TMap) : Void {
      writeByte(map.keyType);
      writeByte(map.valueType);
      writeI32(map.size);
    }

    public function writeMapEnd() : Void {}

    public function writeListBegin(list:TList) : Void {
        writeByte(list.elemType);
        writeI32(list.size);
    }

    public function writeListEnd() : Void {}

    public function writeSetBegin(set:TSet) : Void {
        writeByte(set.elemType);
        writeI32(set.size);
    }

    public function writeSetEnd() : Void {}

    public function writeBool(b : Bool) : Void {
        writeByte(b ? 1 : 0);
    }


    public function writeByte(b : Int) : Void {
        var out = new BytesOutput();
        out.bigEndian = true;
        out.writeByte(b);
        Transport.write(out.getBytes(), 0, 1);
    }

    public function writeI16(i16 : Int) : Void {
        var out = new BytesOutput();
        out.bigEndian = true;
        out.writeInt16(i16);
        Transport.write(out.getBytes(), 0, 2);
    }

    public function writeI32(i32 : Int) : Void {
        var out = new BytesOutput();
        out.bigEndian = true;
        out.writeInt32(i32);
        Transport.write(out.getBytes(), 0, 4);
    }

    public function writeI64(i64 : haxe.Int64) : Void {
        var out = new BytesOutput();
        out.bigEndian = true;
        #if( haxe_ver < 3.2)
        var hi = Int64.getHigh(i64);
        var lo = Int64.getLow(i64);
        out.writeInt32(hi);
        out.writeInt32(lo);
        #else
        out.writeInt32(i64.high);
        out.writeInt32(i64.low);
        #end
        Transport.write(out.getBytes(), 0, 8);
    }

    public function writeDouble(dub:Float) : Void {
        var out = new BytesOutput();
        out.bigEndian = true;
        out.writeDouble(dub);
        Transport.write(out.getBytes(), 0, 8);
    }

    public function writeString(str : String) : Void {
        var out = new BytesOutput();
        out.bigEndian = true;
        out.writeString(str);
        var bytes = out.getBytes();
        writeI32( bytes.length);
        Transport.write( bytes, 0, bytes.length);
    }

    public function writeBinary(bin:Bytes) : Void {
        writeI32(bin.length);
        Transport.write(bin, 0, bin.length);
    }

    /**
     * Reading methods.
     */

    public function readMessageBegin():TMessage {
        var size : Int = readI32();
        if (size < 0) {
            var version : Int = size & VERSION_MASK;
            if (version != VERSION_1) {
                throw new TProtocolException(TProtocolException.BAD_VERSION, "Bad version in readMessageBegin");
            }
            return new TMessage(readString(), size & 0x000000ff, readI32());
        } else {
            if (strictRead_) {
                throw new TProtocolException(TProtocolException.BAD_VERSION, "Missing version in readMessageBegin, old client?");
            }
            return new TMessage(readStringBody(size), readByte(), readI32());
        }
    }

    public function readMessageEnd() : Void {}

    public function readStructBegin():TStruct {
        return ANONYMOUS_STRUCT;
    }

    public function readStructEnd() : Void {}

    public function readFieldBegin() : TField {
        var type : Int = readByte();
        var id : Int = 0;
        if (type != TType.STOP)
        {
            id = readI16();
        }
        return new TField("", type, id);
    }

    public function readFieldEnd() : Void {}

    public function readMapBegin() : TMap {
        var map = new TMap(readByte(), readByte(), readI32());
		CheckReadBytesAvailableMap(map);
        return map;
    }

    public function readMapEnd() : Void {}

    public function readListBegin():TList {
        var list = new TList(readByte(), readI32());
		CheckReadBytesAvailableList(list);
		return list;
    }

    public function readListEnd() : Void {}

    public function readSetBegin() : TSet {
		var set = new TSet(readByte(), readI32());
		CheckReadBytesAvailableSet(set);
		return set;
    }

    public function readSetEnd() : Void {}

    public function readBool() : Bool {
        return (readByte() == 1);
    }


    public function readByte() : Int {
        var buffer = new BytesBuffer();
        var len = Transport.readAll( buffer, 0, 1);
        var inp = new BytesInput( buffer.getBytes(), 0, 1);
        inp.bigEndian = true;
        return inp.readByte();
    }

    public function readI16() : Int {
        var buffer = new BytesBuffer();
        var len = Transport.readAll( buffer, 0, 2);
        var inp = new BytesInput( buffer.getBytes(), 0, 2);
        inp.bigEndian = true;
        return inp.readInt16();
    }

    public function readI32() : Int {
        var buffer = new BytesBuffer();
        var len = Transport.readAll( buffer, 0, 4);
        var inp = new BytesInput( buffer.getBytes(), 0, 4);
        inp.bigEndian = true;
        return inp.readInt32();
    }

    public function readI64() : haxe.Int64 {
        var buffer = new BytesBuffer();
        var len = Transport.readAll( buffer, 0, 8);
        var inp = new BytesInput( buffer.getBytes(), 0, 8);
        inp.bigEndian = true;
        var hi = inp.readInt32();
        var lo = inp.readInt32();
        return Int64.make(hi,lo);
    }

    public function readDouble():Float {
        var buffer = new BytesBuffer();
        var len = Transport.readAll( buffer, 0, 8);
        var inp = new BytesInput( buffer.getBytes(), 0, 8);
        inp.bigEndian = true;
        return inp.readDouble();
    }

    public function readString() : String {
        return readStringBody( readI32());
    }

    public function readStringBody(len : Int) : String {
		Transport.CheckReadBytesAvailable(len);
        if( len > 0) {
            var buffer = new BytesBuffer();
            Transport.readAll( buffer, 0, len);
            var inp = new BytesInput( buffer.getBytes(), 0, len);
            inp.bigEndian = true;
            return inp.readString(len);
        } else {
            return "";
        }
    }

    public function readBinary() : Bytes {
        var len : Int = readI32();
		Transport.CheckReadBytesAvailable(len);
		var buffer = new BytesBuffer();
        Transport.readAll( buffer, 0, len);
        return buffer.getBytes();
    }

	// Return the minimum number of bytes a type will consume on the wire
	public override function GetMinSerializedSize(type : TType) : Int
	{
		switch (type)
		{
			case TType.STOP: return 0;
			case TType.VOID_: return 0;
			case TType.BOOL: return 1;
			case TType.BYTE: return 1;
			case TType.DOUBLE: return 8;
			case TType.I16: return 2;
			case TType.I32: return 4;
			case TType.I64: return 8;
			case TType.STRING: return 4;  // string length
			case TType.STRUCT: return 0;  // empty struct
			case TType.MAP: return 4;  // element count
			case TType.SET: return 4;  // element count
			case TType.LIST: return 4;  // element count
			default: throw new TProtocolException(TProtocolException.NOT_IMPLEMENTED, "unrecognized type code");
		}
	}

}

