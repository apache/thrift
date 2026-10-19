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

import tests.TestBase;

import haxe.Int64;
import haxe.io.Bytes;
import haxe.io.BytesOutput;

import org.apache.thrift.*;
import org.apache.thrift.protocol.*;
import org.apache.thrift.transport.*;

// Exercises the length cap on the JSON numeric reader (ReadJSONNumericChars) and the
// JSON string reader (ReadJSONString). Both accumulate wire characters with no declared
// length, so each is bounded against TConfiguration.MaxMessageSize in the reader itself,
// matching netstd's TJSONProtocol.
//
// The reader cap is read over a TFramedTransport, which is the layering that makes the
// cap load-bearing: readFrame() returns the read budget to its configured maximum before
// each frame, and read() serves buffered bytes without charging that budget, so the
// transport-level budget cannot by itself bound a value that spans frames -- only the
// reader cap can. To make the consumed byte count exactly observable, every wire byte is
// placed in its own one-byte frame (4-byte length 1, then the byte). The protocol pulls
// one byte per frame, so the backing stream's Position advances by exactly 5 for each
// byte the reader consumes -- the test asserts that count, not merely that it threw.
class JSONLimitTest extends tests.TestBase {

    // A small, easy-to-count limit for the whole test.
    private static inline var MAX : Int = 64;

    private static function rep( s : String, n : Int) : String {
        var sb = new StringBuf();
        for ( i in 0 ... n) sb.add( s);
        return sb.toString();
    }

    private static function bytes( s : String) : Bytes {
        return TJSONProtocol.BytesFromString( s);
    }

    // Emit each chunk as its own TFramedTransport frame (4-byte big-endian length, then
    // the chunk bytes).
    private static function frameChunks( chunks : Array<Bytes>) : Bytes {
        var out = new BytesOutput();
        out.bigEndian = true;
        for ( chunk in chunks) {
            out.writeInt32( chunk.length);
            out.writeBytes( chunk, 0, chunk.length);
        }
        return out.getBytes();
    }

    // One one-byte chunk per byte of payload (see the class comment): the protocol pulls
    // one byte per frame, so the inner stream Position advances by exactly 5 per byte read.
    private static function perByte( payload : Bytes) : Array<Bytes> {
        var chunks = new Array<Bytes>();
        for ( i in 0 ... payload.length) chunks.push( payload.sub( i, 1));
        return chunks;
    }

    // Build a TJSONProtocol over TFramedTransport over TStreamTransport whose input is the
    // given frames. MaxMessageSize is set before the transport is constructed. The returned
    // stream is the inner one, so its Position counts the frame bytes the reader pulled.
    private static function framed( chunks : Array<Bytes>, maxMessageSize : Int)
        : { proto : TJSONProtocol, stream : TMemoryStream } {
        var cfg = new TConfiguration();
        cfg.MaxMessageSize = maxMessageSize;
        var input = new TMemoryStream( frameChunks( chunks));
        var output = new TMemoryStream();
        var stream = new TStreamTransport( input, output, cfg);
        return { proto : new TJSONProtocol( new TFramedTransport( stream)), stream : input };
    }

    // Convenience: one JSON value whose wire bytes go one-per-frame.
    private static function perByteJson( wire : String)
        : { proto : TJSONProtocol, stream : TMemoryStream } {
        return framed( perByte( bytes( wire)), MAX);
    }

    private static function isSizeLimit( e : Dynamic) : Bool {
        return Std.isOfType( e, TProtocolException)
            && (cast( e, TProtocolException).errorID == TProtocolException.SIZE_LIMIT);
    }

    public static function Run( server : Bool) : Void {
        // ---- normal values still read back over framed + JSON ----
        {
            var r = perByteJson( '"hello world"');
            TestBase.Expect( r.proto.readString() == "hello world",
                             'string: a normal value reads back unchanged');
        }
        {
            var r = perByteJson( '42 ');  // trailing non-numeric terminator
            TestBase.Expect( Int64.toInt( r.proto.readI64()) == 42,
                             'number: a normal value reads back unchanged');
        }

        // ---- a number longer than the cap is refused, at the cap ----
        {
            var digits = rep( "9", 200);
            var r = perByteJson( digits + " ");
            var threw = false;
            try { r.proto.readDouble(); } catch ( e : Dynamic) { threw = isSizeLimit( e); }
            TestBase.Expect( threw,
                             'number: a ${digits.length}-digit value is refused with SIZE_LIMIT');
            // Refused after pulling exactly MAX+1 digit bytes (5 wire bytes per byte pulled).
            TestBase.Expect( r.stream.Position == 5 * (MAX + 1),
                             'number: reader stopped at the cap (${MAX + 1} bytes pulled, not ${digits.length})');
        }
        // off-by-one guard: exactly MAX digits parse, MAX+1 do not
        {
            var r = perByteJson( rep( "9", MAX) + " ");
            var ok = true;
            try { r.proto.readDouble(); } catch ( e : Dynamic) { ok = false; }
            TestBase.Expect( ok, 'number: exactly $MAX digits are accepted (off-by-one guard)');
        }
        {
            var r = perByteJson( rep( "9", MAX + 1) + " ");
            var threw = false;
            try { r.proto.readDouble(); } catch ( e : Dynamic) { threw = isSizeLimit( e); }
            TestBase.Expect( threw, 'number: ${MAX + 1} digits are refused with SIZE_LIMIT');
        }

        // ---- a string longer than the cap is refused, at the cap ----
        {
            var content = rep( "a", 200);
            var r = perByteJson( '"' + content + '"');
            var threw = false;
            try { r.proto.readString(); } catch ( e : Dynamic) { threw = isSizeLimit( e); }
            TestBase.Expect( threw,
                             'string: a ${content.length}-char value is refused with SIZE_LIMIT');
            // Opening quote (not counted) + MAX+1 characters pulled = MAX+2 bytes.
            TestBase.Expect( r.stream.Position == 5 * (MAX + 2),
                             'string: reader stopped at the cap (${MAX + 2} bytes pulled, not ${content.length + 2})');
        }
        // off-by-one guard: the closing quote is counted before it is tested, so MAX-1
        // content characters parse (closing quote arrives at count = MAX) and MAX do not.
        {
            var r = perByteJson( '"' + rep( "a", MAX - 1) + '"');
            var ok = true;
            try { r.proto.readString(); } catch ( e : Dynamic) { ok = false; }
            TestBase.Expect( ok, 'string: ${MAX - 1} content chars are accepted (off-by-one guard)');
        }
        {
            var r = perByteJson( '"' + rep( "a", MAX) + '"');
            var threw = false;
            try { r.proto.readString(); } catch ( e : Dynamic) { threw = isSizeLimit( e); }
            TestBase.Expect( threw, 'string: $MAX content chars are refused with SIZE_LIMIT');
        }

        // ---- the string cap counts wire bytes, not decoded length ----
        {
            // 20 x A is 120 wire bytes that decode to 20 'A' characters. The decoded
            // length (20) is well under MAX (64); refusing it proves the running count is
            // of wire bytes, as netstd counts, not of the decoded buffer. Each A is
            // framed on its own so its 4-byte \uXXXX read stays within one frame.
            var chunks = new Array<Bytes>();
            chunks.push( bytes( '"'));
            for ( i in 0 ... 20) chunks.push( bytes( "\\u0041"));
            chunks.push( bytes( '"'));
            var r = framed( chunks, MAX);
            var threw = false;
            try { r.proto.readString(); } catch ( e : Dynamic) { threw = isSizeLimit( e); }
            TestBase.Expect( threw,
                             'string: 20 escaped chars (120 wire bytes) are refused on wire-byte count');
        }
    }

}

#end
