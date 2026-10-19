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
import haxe.io.BytesBuffer;
import haxe.io.BytesOutput;
import haxe.io.BytesInput;

import tests.TestBase;

import org.apache.thrift.TConfiguration;
import org.apache.thrift.transport.*;


// A TStream that hands back at most maxChunk bytes per Read and writes them at
// the offset it is asked for -- which is what every real stream does
// (sys.io.FileInput.readBytes among them). A single Read returning fewer bytes
// than requested is normal for pipes, sockets and end-of-buffer, and it is the
// case readAll() is there to paper over: it calls read() again for the rest,
// this time at a non-zero offset.
private class ChunkedStream implements TStream {

    private var data : Bytes;
    private var pos : Int = 0;
    private var maxChunk : Int;

    public function new( data : Bytes, maxChunk : Int = 1) {
        this.data = data;
        this.maxChunk = maxChunk;
    }

    public function Read( buf : Bytes, offset : Int, count : Int) : Int {
        var avail = data.length - pos;
        if( avail <= 0) {
            throw new haxe.io.Eof();
        }
        var n = count;
        if( n > avail) n = avail;
        if( n > maxChunk) n = maxChunk;
        buf.blit( offset, data, pos, n);
        pos += n;
        return n;
    }

    public function Write( buf : Bytes, offset : Int, count : Int) : Void { }
    public function Close() : Void { }
    public function Peek() : Bool { return pos < data.length; }
    public function Flush() : Void { }
}


// An in-memory endpoint used as the inner transport for the layered transports.
// Like TSocket (and the test endpoint in FramedTransportTest) it ignores the
// offset -- buf is an append-only BytesBuffer -- and hands back at most maxChunk
// bytes per read (0 = no limit). Reads are charged against the message budget so
// the framed transport's budget bookkeeping behaves as on a real connection.
private class ChunkedEndpoint extends TEndpointTransport {

    private var data : Bytes;
    private var pos : Int = 0;
    private var maxChunk : Int;

    public function new( data : Bytes, config : TConfiguration, maxChunk : Int = 0) {
        super(config);
        this.data = data;
        this.maxChunk = maxChunk;
    }

    public override function isOpen() : Bool { return true; }
    public override function peek() : Bool { return pos < data.length; }
    public override function open() : Void { }
    public override function close() : Void { }
    public override function write( buf : Bytes, off : Int, len : Int) : Void { }
    public override function flush( callback : Dynamic->Void = null) : Void { }

    public override function read( buf : BytesBuffer, off : Int, len : Int) : Int {
        var avail = data.length - pos;
        var got = (len < avail) ? len : avail;
        if( maxChunk > 0 && got > maxChunk) got = maxChunk;
        buf.addBytes( data, pos, got);
        pos += got;
        CountConsumedMessageBytes(got);
        return got;
    }
}


class TransportPartialReadTest extends tests.TestBase {

    private static inline var BIG_LIMIT : Int = 0x4000000;  // 64 MB, well clear of any test size

    private static function bytesEqual( a : Bytes, b : Bytes) : Bool {
        if( a.length != b.length) {
            return false;
        }
        for( i in 0 ... a.length) {
            if( a.get(i) != b.get(i)) {
                return false;
            }
        }
        return true;
    }

    private static function ramp( n : Int) : Bytes {
        var b = Bytes.alloc(n);
        for( i in 0 ... n) {
            b.set( i, (i * 31 + 7) & 0xFF);
        }
        return b;
    }

    private static function config() : TConfiguration {
        var c = new TConfiguration();
        c.MaxMessageSize = BIG_LIMIT;
        c.MaxFrameSize = BIG_LIMIT;
        return c;
    }

    // A value read off a stream that returns one byte at a time: readAll() asks
    // read() for the remainder at a growing offset. A faithful read must deliver
    // every byte in order regardless of how the stream chunks them.
    public static function StreamPartialRead() : Void {
        var expected = ramp(32);
        var trans = new TStreamTransport( new ChunkedStream( expected, 1), null, config());

        var buf = new BytesBuffer();
        var got = trans.readAll( buf, 0, expected.length);

        tests.TestBase.Expect( got == expected.length, "stream: readAll returns the full length");
        tests.TestBase.Expect( bytesEqual( buf.getBytes(), expected), "stream: bytes arrive in order across one-byte reads");
    }

    // A fixed-width value whose bytes fall in two consecutive frames. This is
    // malformed input -- a well-formed frame holds a whole message -- but the
    // transport must still reassemble it (or fail cleanly), never read one frame
    // into the wrong place. Frame 1 carries the first two bytes, frame 2 the last
    // two; a correct readAll(4) yields all four in order.
    public static function FramedBoundaryRead() : Void {
        var expected = Bytes.ofHex("12345678");

        var out = new BytesOutput();
        out.bigEndian = true;
        out.writeInt32(2); out.writeByte(0x12); out.writeByte(0x34);  // frame 1
        out.writeInt32(2); out.writeByte(0x56); out.writeByte(0x78);  // frame 2
        var wire = out.getBytes();

        var trans = new TFramedTransport( new ChunkedEndpoint( wire, config()));

        var buf = new BytesBuffer();
        var got = trans.readAll( buf, 0, 4);

        tests.TestBase.Expect( got == 4, "framed: readAll returns the full length across a frame boundary");
        tests.TestBase.Expect( bytesEqual( buf.getBytes(), expected), "framed: a value split across two frames reassembles in order");
    }

    // A read at or above the buffer size is served straight from the inner
    // transport. The result must be exactly the bytes read, with nothing appended
    // on top of them.
    public static function BufferedLargeRead() : Void {
        var expected = ramp( 5000);  // > DEFAULT_BUFSIZE (4096)
        var trans = new TBufferedTransport( new ChunkedEndpoint( expected, config()));

        var buf = new BytesBuffer();
        var got = trans.readAll( buf, 0, expected.length);
        var out = buf.getBytes();

        tests.TestBase.Expect( got == expected.length, "buffered: readAll returns the requested length");
        tests.TestBase.Expect( out.length == expected.length, "buffered: a large read yields exactly len bytes");
        tests.TestBase.Expect( bytesEqual( out, expected), "buffered: a large read yields the bytes unchanged");
    }

    // THttpClient carries the same off-misuse, but over a single BytesInput response body that
    // never refills: the off>0 read is only reached once that body is drained, so it hits an
    // empty BytesInput. Whether that throws OutsideBounds or a clean Eof is the target's
    // BytesInput check order (neko checks Eof first -> clean even unfixed; a bounds-first target
    // would throw OutsideBounds unfixed). This guard pins the intended behaviour on every target:
    // the bytes before the end arrive in order, then a clean END_OF_FILE -- which the fix
    // guarantees regardless of check order. responseBuffer_ is private and set only by the HTTP
    // machinery, so it is injected directly (@:access); the constructor opens no connection.
    @:access(org.apache.thrift.transport.THttpClient)
    public static function HttpClientPartialRead() : Void {
        var expected = ramp(3);
        var http = new THttpClient( "http://localhost/", config());
        http.responseBuffer_ = new BytesInput( expected);  // a 3-byte response body

        var buf = new BytesBuffer();
        var clean = false;
        try {
            http.readAll( buf, 0, 4);  // 4 asked, 3 available -> the second read runs at off = 3
        }
        catch ( e : TTransportException ) {
            clean = (e.errorID == TTransportException.END_OF_FILE);
        }
        tests.TestBase.Expect( clean, "http: a short response ends with a clean END_OF_FILE");
        tests.TestBase.Expect( bytesEqual( buf.getBytes(), expected), "http: the bytes before the end arrive in order");
    }

    public static function Run(server : Bool) : Void {
        StreamPartialRead();
        FramedBoundaryRead();
        BufferedLargeRead();
        HttpClientPartialRead();
    }
}

#end
