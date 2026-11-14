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

package tests;

#if sys

import haxe.io.Bytes;

import tests.TestBase;

import org.apache.thrift.TConfiguration;
import org.apache.thrift.protocol.*;
import org.apache.thrift.transport.*;


private enum Wire {
    Binary;
    Compact;
    JSON;
}


// An i8 is signed, so readByte() must return a value from -128 to 127. The binary and compact
// protocols carry it as one byte in two's complement, 9c for -100 and ff for -1. JSON carries the
// number.
//
// Every protocol must read every i8 back with readByte(). The binary and compact input is built
// byte by byte, as any library writes it; the JSON input is what writeByte() writes.
//
// The bytes that TCompactProtocol reads for its own use hold no i8 and must still read unsigned:
// the protocol id 0x82, the version and message type, field and container headers, and varints.
// So a message of each type, with negative i8 values in fields, a list, a set and a map, must read
// back. Besides the protocol id, these bytes of it have the top bit set: the version and type of a
// ONEWAY message (81), the first byte of the seqid varint (e8), the field headers 83 and aa, the
// list header f3 and the set header 83.
//
// All cases run before the test fails, so that the output names every case that is not handled
// as expected.
class SignedByteTest extends tests.TestBase {

    // varint e8 07 in the compact protocol
    private static inline var SEQID : Int = 1000;

    // an i8, and the byte that carries it
    private static var ENCODINGS = [
        { value : -128, hex : "80" },
        { value : -100, hex : "9c" },
        { value : -1,   hex : "ff" },
        { value : 0,    hex : "00" },
        { value : 1,    hex : "01" },
        { value : 127,  hex : "7f" }
    ];

    // 15 elements: the compact list header is f3, the size follows as a varint
    private static var LIST : Array<Int> = [-128, -127, -100, -65, -64, -2, -1, 0, 1, 2, 63, 64, 100, 126, 127];

    // 8 elements: the compact set header is 83
    private static var SET : Array<Int> = [-128, -100, -64, -1, 0, 1, 100, 127];

    // key, value, key, value
    private static var MAP : Array<Int> = [-128, 127, -1, -100];

    private static var cases : Int = 0;
    private static var failures : Int = 0;


    private static function wireName( wire : Wire) : String {
        switch( wire) {
            case Binary:   return "TBinaryProtocol";
            case Compact:  return "TCompactProtocol";
            case JSON:     return "TJSONProtocol";
        }
    }

    private static function protocol( wire : Wire, trans : TTransport) : TProtocol {
        switch( wire) {
            case Binary:   return new TBinaryProtocol( trans);
            case Compact:  return new TCompactProtocol( trans);
            case JSON:     return new TJSONProtocol( trans);
        }
    }

    private static function written( wire : Wire, write : TProtocol -> Void) : Bytes {
        var stream = new TMemoryStream();
        var trans = new TStreamTransport( new TMemoryStream(), stream, new TConfiguration());
        write( protocol( wire, trans));
        trans.flush();
        var data = Bytes.alloc( stream.Position);
        stream.Position = 0;
        stream.Read( data, 0, data.length);
        return data;
    }

    private static function reader( wire : Wire, data : Bytes) : TProtocol {
        return protocol( wire, new TStreamTransport( new TMemoryStream( data), new TMemoryStream(), new TConfiguration()));
    }

    private static function Report( title : String, ok : Bool, outcome : String) : Void {
        ++cases;
        if( ! ok) {
            ++failures;
        }
        trace('Test "$title" - ' + (ok ? 'OK' : 'FAILED') + ' ($outcome)');
    }


    // A list of every i8 from -128 to 127
    private static function everyValue( wire : Wire) : Bytes {
        var values = new StringBuf();
        for( i in 0 ... 256) {
            values.add( StringTools.hex( (i - 128) & 0xff, 2));
        }
        switch( wire) {
            case Binary:
                // element type 03, size 256
                return Bytes.ofHex( "0300000100" + values.toString());
            case Compact:
                // header f3: element type 03, size to follow; size 256 as the varint 80 02
                return Bytes.ofHex( "f38002" + values.toString());
            case JSON:
                return written( wire, function( prot : TProtocol) : Void {
                    prot.writeListBegin( new TList( TType.BYTE, 256));
                    for( i in 0 ... 256) {
                        prot.writeByte( i - 128);
                    }
                    prot.writeListEnd();
                });
        }
    }

    private static function testEveryValue( wire : Wire) : Void {
        var outcome = "";
        var ok = false;
        try {
            var prot = reader( wire, everyValue( wire));
            var count = prot.readListBegin().size;
            var wrong = new Array<String>();
            for( i in 0 ... count) {
                var value = prot.readByte();
                if( value != i - 128) {
                    wrong.push( '${i - 128} as $value');
                }
            }
            prot.readListEnd();
            ok = (count == 256) && (wrong.length == 0);
            outcome = (wrong.length == 0)
                ? '$count values read back'
                : '${wrong.length} of $count values read wrong, from ${wrong[0]} to ${wrong[wrong.length - 1]}';
        }
        catch( e : Dynamic) {
            outcome = 'threw $e';
        }
        Report( '${wireName( wire)}: readByte() reads every i8 from -128 to 127', ok, outcome);
    }

    private static function testEncoding( wire : Wire, value : Int, hex : String) : Void {
        var outcome = "";
        var ok = false;
        try {
            var back = reader( wire, Bytes.ofHex( hex)).readByte();
            outcome = 'read $back';
            ok = (back == value);
        }
        catch( e : Dynamic) {
            outcome = 'threw $e';
        }
        Report( '${wireName( wire)}: readByte() reads the byte $hex as $value', ok, outcome);
    }


    // Compact field headers: 13, 83 (delta 8), 19, aa (delta 10), 1b
    private static function writeMessage( prot : TProtocol, type : Int) : Void {
        prot.writeMessageBegin( new TMessage( "i8", type, SEQID));
        prot.writeStructBegin( new TStruct( "Bytes"));

        prot.writeFieldBegin( new TField( "a", TType.BYTE, 1));
        prot.writeByte( -100);
        prot.writeFieldEnd();

        prot.writeFieldBegin( new TField( "b", TType.BYTE, 9));
        prot.writeByte( -1);
        prot.writeFieldEnd();

        prot.writeFieldBegin( new TField( "c", TType.LIST, 10));
        prot.writeListBegin( new TList( TType.BYTE, LIST.length));
        for( value in LIST) {
            prot.writeByte( value);
        }
        prot.writeListEnd();
        prot.writeFieldEnd();

        prot.writeFieldBegin( new TField( "d", TType.SET, 20));
        prot.writeSetBegin( new TSet( TType.BYTE, SET.length));
        for( value in SET) {
            prot.writeByte( value);
        }
        prot.writeSetEnd();
        prot.writeFieldEnd();

        prot.writeFieldBegin( new TField( "e", TType.MAP, 21));
        prot.writeMapBegin( new TMap( TType.BYTE, TType.BYTE, Std.int( MAP.length / 2)));
        for( value in MAP) {
            prot.writeByte( value);
        }
        prot.writeMapEnd();
        prot.writeFieldEnd();

        prot.writeFieldStop();
        prot.writeStructEnd();
        prot.writeMessageEnd();
    }

    // What readMessage() returns for the message that writeMessage() writes
    private static function expected( type : Int) : String {
        return [
            'message i8 type $type seqid $SEQID',
            'field 1 i8 -100',
            'field 9 i8 -1',
            'field 10 list of ${LIST.length}: ' + LIST.join( ' '),
            'field 20 set of ${SET.length}: ' + SET.join( ' '),
            'field 21 map of ${Std.int( MAP.length / 2)}, type ${TType.BYTE} to ${TType.BYTE}: ' + MAP.join( ' ')
        ].join( ', ');
    }

    private static function readValues( prot : TProtocol, count : Int) : String {
        var values = new Array<String>();
        for( i in 0 ... count) {
            values.push( '${prot.readByte()}');
        }
        return values.join( ' ');
    }

    // Reads a message and describes it, all elements taken as i8
    private static function readMessage( prot : TProtocol) : String {
        var parts = new Array<String>();
        var message = prot.readMessageBegin();
        parts.push( 'message ${message.name} type ${message.type} seqid ${message.seqid}');
        prot.readStructBegin();
        var field = prot.readFieldBegin();
        while( field.type != TType.STOP) {
            switch( field.type) {
                case TType.BYTE:
                    parts.push( 'field ${field.id} i8 ${prot.readByte()}');
                case TType.LIST:
                    var list = prot.readListBegin();
                    parts.push( 'field ${field.id} list of ${list.size}: ' + readValues( prot, list.size));
                    prot.readListEnd();
                case TType.SET:
                    var set = prot.readSetBegin();
                    parts.push( 'field ${field.id} set of ${set.size}: ' + readValues( prot, set.size));
                    prot.readSetEnd();
                case TType.MAP:
                    var map = prot.readMapBegin();
                    parts.push( 'field ${field.id} map of ${map.size}, type ${map.keyType} to ${map.valueType}: ' + readValues( prot, 2 * map.size));
                    prot.readMapEnd();
                default:
                    parts.push( 'field ${field.id} of type ${field.type}, not read any further');
                    return parts.join( ', ');
            }
            prot.readFieldEnd();
            field = prot.readFieldBegin();
        }
        prot.readStructEnd();
        prot.readMessageEnd();
        return parts.join( ', ');
    }

    private static function testMessage( type : Int) : Void {
        var want = expected( type);
        var outcome = "";
        var ok = false;
        try {
            var got = readMessage( reader( Compact, written( Compact, function( prot : TProtocol) : Void {
                writeMessage( prot, type);
            })));
            ok = (got == want);
            outcome = ok ? 'read back as written' : 'read back as "$got", expected "$want"';
        }
        catch( e : Dynamic) {
            outcome = 'threw $e';
        }
        Report( '${wireName( Compact)}: a message of type $type with negative i8 values reads back', ok, outcome);
    }


    public static function Run( server : Bool) : Void {
        cases = 0;
        failures = 0;
        for( wire in [Binary, Compact, JSON]) {
            testEveryValue( wire);
        }
        for( wire in [Binary, Compact]) {
            for( entry in ENCODINGS) {
                testEncoding( wire, entry.value, entry.hex);
            }
        }
        for( type in [TMessageType.CALL, TMessageType.REPLY, TMessageType.EXCEPTION, TMessageType.ONEWAY]) {
            testMessage( type);
        }
        TestBase.Expect( failures == 0, 'readByte() returns every i8 signed ($failures of $cases cases failed)');
    }
}

#end
