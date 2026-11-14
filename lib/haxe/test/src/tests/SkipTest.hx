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

private enum Container {
    AsList;
    AsSet;
    AsMap;
}


// TProtocolUtil.skip() is handed the wire type of every field a reader does not
// recognise (generated code calls it for unknown field ids and the default
// field-type arm). It must handle every type that can appear on the wire,
// including UUID (TType.UUID = 16). A UUID field used to fall through skip()'s
// default arm and throw TProtocolException(UNKNOWN), desyncing the read; it is
// now consumed in the protocol-correct form (prot.readUuid()), like Java and Go.
//
// A list, set or map field must be skipped in full on every protocol, so that the
// next field reads. That depends on readListBegin() and readSetBegin() returning
// the element type that was written: TCompactProtocol used to return 0 for every
// list or set with elements, and skip() threw on it. The sizes 0, 1, 14 and 15
// give each form of the compact header: no elements, the size in the header, and
// the size after it.
//
// A type that skip() does not know is refused with TProtocolException(UNKNOWN),
// and the message names the type.
//
// The container and message cases all run before the test fails, so that the
// output names every case that is not handled as expected.
class SkipTest extends tests.TestBase {

    private static inline var SENTINEL : Int = 4711;
    private static var SIZES : Array<Int> = [0, 1, 14, 15];

    private static var cases : Int = 0;
    private static var failures : Int = 0;

    public static function SkipsUuidField() : Void {
        var cfg = new TConfiguration();
        var stream = new TMemoryStream();
        var trans : TTransport = new TStreamTransport( stream, stream, cfg);
        var proto = new TBinaryProtocol( trans);

        // A uuid, then a sentinel i32 standing in for the next field on the wire.
        proto.writeUuid( "00112233-4455-6677-8899-aabbccddeeff");
        proto.writeI32( 4711);

        stream.Position = 0;  // rewind to read back what was written

        TProtocolUtil.skip( proto, TType.UUID);  // must consume exactly the 16 uuid bytes
        var sentinel = proto.readI32();

        tests.TestBase.Expect( sentinel == 4711,
            "skip(UUID) consumes exactly the uuid, leaving the following value readable");
    }


    private static function wireName( wire : Wire) : String {
        switch( wire) {
            case Binary:   return "TBinaryProtocol";
            case Compact:  return "TCompactProtocol";
            case JSON:     return "TJSONProtocol";
        }
    }

    private static function containerName( container : Container) : String {
        switch( container) {
            case AsList:  return "list";
            case AsSet:   return "set";
            case AsMap:   return "map";
        }
    }

    private static function containerType( container : Container) : Int {
        switch( container) {
            case AsList:  return TType.LIST;
            case AsSet:   return TType.SET;
            case AsMap:   return TType.MAP;
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

    // A container of i32 elements (i32 keys and values for a map)
    private static function writeContainer( prot : TProtocol, container : Container, size : Int) : Void {
        switch( container) {
            case AsList:  prot.writeListBegin( new TList( TType.I32, size));
            case AsSet:   prot.writeSetBegin( new TSet( TType.I32, size));
            case AsMap:   prot.writeMapBegin( new TMap( TType.I32, TType.I32, size));
        }
        for( i in 0 ... size) {
            prot.writeI32( 1000 * i + 7);
            if( container == AsMap) {
                prot.writeI32( -i);
            }
        }
        switch( container) {
            case AsList:  prot.writeListEnd();
            case AsSet:   prot.writeSetEnd();
            case AsMap:   prot.writeMapEnd();
        }
    }


    // readListBegin() and readSetBegin() return the element type and size that were written
    private static function testElementType( wire : Wire, container : Container, size : Int) : Void {
        var outcome = "";
        var ok = false;
        try {
            var prot = reader( wire, written( wire, function( prot : TProtocol) : Void {
                writeContainer( prot, container, size);
            }));
            var elemType : Int;
            var count : Int;
            if( container == AsList) {
                var list = prot.readListBegin();
                elemType = list.elemType;
                count = list.size;
            } else {
                var set = prot.readSetBegin();
                elemType = set.elemType;
                count = set.size;
            }
            ok = (elemType == TType.I32) && (count == size);
            outcome = 'element type $elemType, size $count';
        }
        catch( e : Dynamic) {
            outcome = 'threw $e';
        }
        var begin = (container == AsList) ? "readListBegin()" : "readSetBegin()";
        Report( '${wireName( wire)}: $begin returns element type ${TType.I32} for a ${containerName( container)} of size $size',
                ok, outcome);
    }

    // A struct of the container as field 1 and an i32 as field 2: skipping field 1 must leave field 2 readable
    private static function testSkipField( wire : Wire, container : Container, size : Int) : Void {
        var outcome = "";
        var ok = false;
        try {
            var prot = reader( wire, written( wire, function( prot : TProtocol) : Void {
                prot.writeStructBegin( new TStruct( "Fields"));
                prot.writeFieldBegin( new TField( "container", containerType( container), 1));
                writeContainer( prot, container, size);
                prot.writeFieldEnd();
                prot.writeFieldBegin( new TField( "sentinel", TType.I32, 2));
                prot.writeI32( SENTINEL);
                prot.writeFieldEnd();
                prot.writeFieldStop();
                prot.writeStructEnd();
            }));
            prot.readStructBegin();
            var field = prot.readFieldBegin();
            TProtocolUtil.skip( prot, field.type);
            prot.readFieldEnd();
            var next = prot.readFieldBegin();
            var value = prot.readI32();
            prot.readFieldEnd();
            var stop = prot.readFieldBegin();
            prot.readStructEnd();
            ok = (field.id == 1) && (next.id == 2) && (next.type == TType.I32) && (value == SENTINEL) && (stop.type == TType.STOP);
            outcome = 'skipped field ${field.id} of type ${field.type}, then read field ${next.id} as $value';
        }
        catch( e : Dynamic) {
            outcome = 'threw $e';
        }
        Report( '${wireName( wire)}: skip() passes over a ${containerName( container)} field of size $size', ok, outcome);
    }

    // skip() refuses a type it does not know, and says which
    private static function testUnknownType() : Void {
        var type = 99;
        var outcome = "";
        var ok = false;
        try {
            TProtocolUtil.skip( reader( Binary, Bytes.alloc( 0)), type);
            outcome = "no exception";
        }
        catch( e : TProtocolException) {
            ok = (e.errorID == TProtocolException.UNKNOWN) && (e.errorMsg == 'Unknown field type $type');
            outcome = 'TProtocolException ${e.errorID}: ${e.errorMsg}';
        }
        catch( e : Dynamic) {
            outcome = 'threw $e';
        }
        Report( 'skip() refuses the type $type and names it', ok, outcome);
    }


    public static function Run( server : Bool) : Void {
        SkipsUuidField();

        cases = 0;
        failures = 0;
        for( wire in [Binary, Compact, JSON]) {
            for( container in [AsList, AsSet]) {
                for( size in SIZES) {
                    testElementType( wire, container, size);
                }
            }
            for( container in [AsList, AsSet, AsMap]) {
                for( size in SIZES) {
                    testSkipField( wire, container, size);
                }
            }
        }
        testUnknownType();
        TestBase.Expect( failures == 0, 'skip() passes over every container and refuses an unknown type ($failures of $cases cases failed)');
    }
}

#end
