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


// TConfiguration.MaxContainerSize limits the number of elements a list, set or map may declare
// when it is read. Every container here is written complete, with elements of type byte, and is
// read back with MaxMessageSize set to its exact length. So every declared element really is on
// the wire, and the byte budget that CheckReadBytesAvailable() holds the count to accepts it:
// only MaxContainerSize can refuse it.
//
// A count above the limit must be refused by read*Begin(), with a SIZE_LIMIT TProtocolException.
// A count at the limit must read back in full, and so must the count above it when the limit is
// 0, -1 or left at its default, all of which switch it off.
//
// All cases run before the test fails, so that the output names every case that is not handled
// as expected.
class MaxContainerSizeTest extends tests.TestBase {

    private static inline var LIMIT : Int = 20;
    private static inline var MAP_VALUE_BASE : Int = 100;  // map values are 100 + key, all below 128

    private static var cases : Int = 0;
    private static var failures : Int = 0;


    private static function wireName( wire : Wire) : String {
        switch( wire) {
            case Binary:   return "TBinaryProtocol";
            case Compact:  return "TCompactProtocol";
            case JSON:     return "TJSONProtocol";
        }
    }

    private static function containerName( container : Container) : String {
        switch( container) {
            case AsList:  return "List";
            case AsSet:   return "Set";
            case AsMap:   return "Map";
        }
    }

    private static function protocol( wire : Wire, trans : TTransport) : TProtocol {
        switch( wire) {
            case Binary:   return new TBinaryProtocol( trans);
            case Compact:  return new TCompactProtocol( trans);
            case JSON:     return new TJSONProtocol( trans);
        }
    }

    private static function contentOf( stream : TMemoryStream) : Bytes {
        var length = stream.Position;
        var data = Bytes.alloc( length);
        stream.Position = 0;
        stream.Read( data, 0, length);
        return data;
    }

    // A complete container of count byte elements (count byte pairs for a map)
    private static function written( wire : Wire, container : Container, count : Int) : Bytes {
        var stream = new TMemoryStream();
        var trans = new TStreamTransport( new TMemoryStream(), stream, new TConfiguration());
        var prot = protocol( wire, trans);
        switch( container) {
            case AsList:  prot.writeListBegin( new TList( TType.BYTE, count));
            case AsSet:   prot.writeSetBegin( new TSet( TType.BYTE, count));
            case AsMap:   prot.writeMapBegin( new TMap( TType.BYTE, TType.BYTE, count));
        }
        for( i in 0 ... count) {
            prot.writeByte( i);
            if( container == AsMap) {
                prot.writeByte( MAP_VALUE_BASE + i);
            }
        }
        switch( container) {
            case AsList:  prot.writeListEnd();
            case AsSet:   prot.writeSetEnd();
            case AsMap:   prot.writeMapEnd();
        }
        trans.flush();
        return contentOf( stream);
    }

    // limit == null leaves MaxContainerSize at its default
    private static function reader( wire : Wire, data : Bytes, limit : Null<Int>) : TProtocol {
        var config = new TConfiguration();
        config.MaxMessageSize = data.length;
        if( limit != null) {
            config.MaxContainerSize = limit;
        }
        return protocol( wire, new TStreamTransport( new TMemoryStream( data), new TMemoryStream(), config));
    }

    private static function readBegin( prot : TProtocol, container : Container) : Int {
        switch( container) {
            case AsList:  return prot.readListBegin().size;
            case AsSet:   return prot.readSetBegin().size;
            case AsMap:   return prot.readMapBegin().size;
        }
    }

    // Reads the elements and the end of the container; returns whether they are the ones written
    private static function readRest( prot : TProtocol, container : Container, count : Int) : Bool {
        var match = true;
        for( i in 0 ... count) {
            match = (prot.readByte() == i) && match;
            if( container == AsMap) {
                match = (prot.readByte() == MAP_VALUE_BASE + i) && match;
            }
        }
        switch( container) {
            case AsList:  prot.readListEnd();
            case AsSet:   prot.readSetEnd();
            case AsMap:   prot.readMapEnd();
        }
        return match;
    }

    private static function isContainerSizeLimit( e : Dynamic) : Bool {
        return Std.isOfType( e, TProtocolException)
            && (cast( e, TProtocolException).errorID == TProtocolException.SIZE_LIMIT);
    }

    private static function Report( title : String, ok : Bool, outcome : String) : Void {
        ++cases;
        if( ! ok) {
            ++failures;
        }
        trace('Test "$title" - ' + (ok ? 'OK' : 'FAILED') + ' ($outcome)');
    }


    // A container of count elements, read with the given limit, is either read back in full
    // or refused by read*Begin() with SIZE_LIMIT
    private static function testRead( wire : Wire, container : Container, count : Int, limit : Null<Int>, accepted : Bool) : Void {
        var setting = (limit == null) ? "at its default" : '$limit';
        var expected = accepted ? "is read back in full" : 'is refused by read${containerName( container)}Begin';
        var title = '${wireName( wire)}: a ${containerName( container).toLowerCase()} of $count elements with MaxContainerSize $setting $expected';
        var ok = false;
        var outcome = "";
        var begin = 'read${containerName( container)}Begin';
        var stage = 'writing';
        try {
            var prot = reader( wire, written( wire, container, count), limit);
            stage = begin;
            var declared = readBegin( prot, container);
            stage = 'reading the elements';
            var match = readRest( prot, container, declared);
            ok = accepted && (declared == count) && match;
            outcome = 'no exception, $declared elements read back' + (match ? '' : ', not the ones written');
        }
        catch( e : Dynamic) {
            ok = (! accepted) && (stage == begin) && isContainerSizeLimit( e);
            outcome = '$stage threw $e';
        }
        Report( title, ok, outcome);
    }


    public static function Run( server : Bool) : Void {
        cases = 0;
        failures = 0;
        for( wire in [Binary, Compact, JSON]) {
            for( container in [AsList, AsSet, AsMap]) {
                testRead( wire, container, LIMIT, LIMIT, true);
                testRead( wire, container, LIMIT + 1, LIMIT, false);
                for( off in [0, -1, null]) {
                    testRead( wire, container, LIMIT + 1, off, true);
                }
            }
        }
        TestBase.Expect( failures == 0, 'MaxContainerSize limits the element count of a container ($failures of $cases cases failed)');
    }
}

#end
