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

import haxe.Int64;
import haxe.io.Bytes;

import tests.TestBase;

import org.apache.thrift.TConfiguration;
import org.apache.thrift.protocol.*;
import org.apache.thrift.transport.*;


// TCompactProtocol writes an i64 as a varint of its zigzag value: 7-bit groups, the lowest first,
// with the continuation bit 0x80 set in every byte but the last. writeI64() must write the shortest
// varint, which takes 1 to 10 bytes. A longer one ends in a 00 byte. Readers that allow at most
// 10 bytes reject one of 11, the Haxe TCompactProtocol among them.
//
// The expected bytes below are what the Python library writes for these values. They cover every
// length from 1 to 10 bytes with an odd highest group, and every length up to 9 with an even one
// as well; a 10th group can only be 1. A sweep over the powers of two and their neighbours checks
// every bit position for the length alone.
//
// Earlier versions of writeI64() wrote some values one byte too long. Those of at most 10 bytes
// must still read back.
//
// All cases run before the test fails, so that the output names every value that is not handled
// as expected.
class CompactVarintTest extends tests.TestBase {

    private static inline var MAX_VARINT_BYTES : Int = 10;

    private static var cases : Int = 0;
    private static var failures : Int = 0;

    // the value, and the shortest varint for it, by length
    private static var SHORTEST : Array<Array<String>> = [
        ["0",                    "00"],
        ["-1",                   "01"],
        ["1",                    "02"],
        ["63",                   "7e"],
        ["-64",                  "7f"],
        ["64",                   "8001"],
        ["-65",                  "8101"],
        ["-129",                 "8102"],
        ["8191",                 "fe7f"],
        ["-8192",                "ff7f"],
        ["8192",                 "808001"],
        ["-16385",               "818002"],
        ["1048576",              "80808001"],
        ["-2097153",             "81808002"],
        ["2147483647",           "feffffff0f"],
        ["-2147483648",          "ffffffff0f"],
        ["-268435457",           "8180808002"],
        ["17179869184",          "808080808001"],
        ["-34359738369",         "818080808002"],
        ["2199023255552",        "80808080808001"],
        ["-4398046511105",       "81808080808002"],
        ["281474976710656",      "8080808080808001"],
        ["-562949953421313",     "8180808080808002"],
        ["4611686018427387903",  "feffffffffffffff7f"],
        ["-4611686018427387904", "ffffffffffffffff7f"],
        ["-72057594037927937",   "818080808080808002"],
        ["4611686018427387904",  "80808080808080808001"],
        ["-4611686018427387905", "81808080808080808001"],
        ["9223372036854775807",  "feffffffffffffffff01"],
        ["-9223372036854775808", "ffffffffffffffffff01"]
    ];

    // varints one byte too long, as earlier versions wrote them, and the value they hold
    private static var OVERLONG : Array<Array<String>> = [
        ["8100",                 "-1"],
        ["ff00",                 "-64"],
        ["808100",               "64"],
        ["feffffffffffffffff00", "4611686018427387903"]
    ];


    private static function written( value : Int64) : Bytes {
        var stream = new TMemoryStream();
        var trans = new TStreamTransport( new TMemoryStream(), stream, new TConfiguration());
        new TCompactProtocol( trans).writeI64( value);
        trans.flush();
        var data = Bytes.alloc( stream.Position);
        stream.Position = 0;
        stream.Read( data, 0, data.length);
        return data;
    }

    private static function read( data : Bytes) : Int64 {
        var trans = new TStreamTransport( new TMemoryStream( data), new TMemoryStream(), new TConfiguration());
        return new TCompactProtocol( trans).readI64();
    }

    // Reads the varint and describes the outcome; returns whether it holds the expected value
    private static function readsAs( data : Bytes, expected : Int64, outcome : Array<String>) : Bool {
        try {
            var value = read( data);
            outcome.push( 'read back ${Int64.toStr( value)}');
            return Int64.compare( value, expected) == 0;
        }
        catch( e : Dynamic) {
            outcome.push( 'reading it back threw $e');
            return false;
        }
    }

    // The shortest varint is at most 10 bytes long, and it ends in 00 only if 00 is all of it
    private static function isShortest( data : Bytes) : Bool {
        return (data.length <= MAX_VARINT_BYTES)
            && ((data.length == 1) || (data.get( data.length - 1) != 0));
    }

    private static function Report( title : String, ok : Bool, outcome : String) : Void {
        ++cases;
        if( ! ok) {
            ++failures;
        }
        trace('Test "$title" - ' + (ok ? 'OK' : 'FAILED') + ' ($outcome)');
    }


    private static function testShortest( text : String, hex : String) : Void {
        var value = Int64.parseString( text);
        var data = written( value);
        var outcome = ['wrote ${data.length} bytes ${data.toHex()}'];
        var ok = (data.toHex() == hex);
        ok = readsAs( data, value, outcome) && ok;
        Report( 'writeI64($text) writes $hex and reads it back', ok, outcome.join( ', '));

        outcome = new Array<String>();
        ok = readsAs( Bytes.ofHex( hex), value, outcome);
        Report( 'readI64() reads $hex as $text', ok, outcome.join( ', '));
    }

    private static function testOverlong( hex : String, text : String) : Void {
        var outcome = new Array<String>();
        var ok = readsAs( Bytes.ofHex( hex), Int64.parseString( text), outcome);
        Report( 'readI64() reads the over-long $hex as $text', ok, outcome.join( ', '));
    }

    // 2^k, 2^k - 1, -2^k and -2^k - 1 for every k from 0 to 63, which wrap around at k = 63.
    // Only the values that fail are traced.
    private static function testSweep() : Void {
        var one = Int64.make( 0, 1);
        var count = 0;
        var failed = 0;
        for( k in 0 ... 64) {
            var power = Int64.shl( one, k);
            for( value in [power, Int64.sub( power, one), Int64.neg( power), Int64.sub( Int64.neg( power), one)]) {
                ++count;
                var data = written( value);
                var outcome = ['wrote ${data.length} bytes ${data.toHex()}'];
                var ok = isShortest( data);
                ok = readsAs( data, value, outcome) && ok;
                if( ! ok) {
                    ++failed;
                    trace('Test "writeI64(${Int64.toStr( value)}) writes the shortest varint and reads it back" - FAILED (${outcome.join( ', ')})');
                }
            }
        }
        Report( 'writeI64() writes the shortest varint for 2^k, 2^k - 1, -2^k and -2^k - 1, k = 0 to 63',
                failed == 0, '$failed of $count values failed');
    }


    public static function Run( server : Bool) : Void {
        cases = 0;
        failures = 0;
        for( entry in SHORTEST) {
            testShortest( entry[0], entry[1]);
        }
        for( entry in OVERLONG) {
            testOverlong( entry[0], entry[1]);
        }
        testSweep();
        TestBase.Expect( failures == 0, 'TCompactProtocol writes every i64 as the shortest varint ($failures of $cases cases failed)');
    }
}

#end
