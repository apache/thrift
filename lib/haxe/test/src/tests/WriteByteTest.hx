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
}


// writeByte() of the binary and compact protocols puts an i8 on the wire as one byte in two's
// complement: 80 for -128, 9c for -100, ff for -1, 7f for 127. Every value from -128 to 127 must
// be written that way on every target. Values from 128 to 255, which callers can pass for the
// same bytes, must still be written as the byte they name.
//
// Only the written bytes are checked; nothing is read back.
//
// Each value is written on its own, and all of them run before the test fails, so that the output
// names every value that is not handled as expected.
class WriteByteTest extends tests.TestBase {

    private static var cases : Int = 0;
    private static var failures : Int = 0;


    private static function wireName( wire : Wire) : String {
        switch( wire) {
            case Binary:   return "TBinaryProtocol";
            case Compact:  return "TCompactProtocol";
        }
    }

    private static function protocol( wire : Wire, trans : TTransport) : TProtocol {
        switch( wire) {
            case Binary:   return new TBinaryProtocol( trans);
            case Compact:  return new TCompactProtocol( trans);
        }
    }

    // The bytes that writeByte() writes for the value, as hex
    private static function written( wire : Wire, value : Int) : String {
        var stream = new TMemoryStream();
        var trans = new TStreamTransport( new TMemoryStream(), stream, new TConfiguration());
        protocol( wire, trans).writeByte( value);
        trans.flush();
        var data = Bytes.alloc( stream.Position);
        stream.Position = 0;
        stream.Read( data, 0, data.length);
        return data.toHex();
    }

    private static function Report( title : String, ok : Bool, outcome : String) : Void {
        ++cases;
        if( ! ok) {
            ++failures;
        }
        trace('Test "$title" - ' + (ok ? 'OK' : 'FAILED') + ' ($outcome)');
    }


    // Each value from first to last must come out as the one byte (value + 256) % 256
    private static function testRange( wire : Wire, first : Int, last : Int) : Void {
        var wrong = new Array<String>();
        for( value in first ... last + 1) {
            var expected = StringTools.hex( (value + 256) % 256, 2).toLowerCase();
            var outcome = "";
            try {
                outcome = 'wrote ' + written( wire, value);
            }
            catch( e : Dynamic) {
                outcome = 'threw $e';
            }
            if( outcome != 'wrote $expected') {
                wrong.push( '$value ($outcome)');
            }
        }
        var count = last - first + 1;
        var outcome = (wrong.length == 0)
            ? 'all $count values written as their byte'
            : '${wrong.length} of $count values not written as their byte, the first ${wrong[0]}, the last ${wrong[wrong.length - 1]}';
        Report( '${wireName( wire)}: writeByte() writes every value from $first to $last as one byte', wrong.length == 0, outcome);
    }


    public static function Run( server : Bool) : Void {
        cases = 0;
        failures = 0;
        for( wire in [Binary, Compact]) {
            testRange( wire, -128, 127);
            testRange( wire, 128, 255);
        }
        TestBase.Expect( failures == 0, 'writeByte() writes every i8 as its byte ($failures of $cases cases failed)');
    }
}

#end
