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
import haxe.io.BytesBuffer;
import haxe.io.BytesOutput;

import tests.TestBase;

import org.apache.thrift.*;
import org.apache.thrift.transport.*;

// An endpoint that accounts for reads the way TSocket does: every read is
// charged against the message budget (TSocket.hx, CountConsumedMessageBytes
// after each read). TStreamTransport cannot stand in for it -- that one does
// not charge reads at all -- and TSocket itself would need a real socket.
class CountingMemoryEndpoint extends TEndpointTransport {

    private var data : Bytes;
    private var pos : Int = 0;

    public function new( data : Bytes, config : TConfiguration) {
        super(config);
        this.data = data;
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
        buf.addBytes( data, pos, got);
        pos += got;
        CountConsumedMessageBytes(got);
        return got;
    }
}


// The read budget must survive one frame following another on the same
// connection with nothing in between.
//
// TFramedTransport binds the budget to each frame (UpdateKnownMessageSize) and
// never returns it to the configured maximum first. TSocket charges every read
// against that budget, so reading a frame leaves it at zero -- and the only
// resets on the socket path are in flush() and on connect. A server handling
// two one-way calls in a row never flushes between them (the generated
// processor returns before the flush for a one-way function), so the second
// call dies on its four-byte frame header, far below any configured limit.
//
// The last test is the other half of the contract: binding must stay tight
// enough that a frame cannot declare a field larger than itself.
class FramedTransportTest extends tests.TestBase {

    private static inline var HEADER_SIZE : Int = 4;

    // One frame: a four-byte big-endian length, then that many bytes.
    private static function appendFrame( out : BytesOutput, payload : Int) : Void {
        out.writeInt32(payload);
        for( i in 0 ... payload) {
            out.writeByte(i & 0xFF);
        }
    }

    private static function framedOver( wire : Bytes) : TFramedTransport {
        var config = new TConfiguration();
        return new TFramedTransport( new CountingMemoryEndpoint( wire, config));
    }

    private static function readExactly( trans : TFramedTransport, len : Int) : Int {
        var buf = new BytesBuffer();
        trans.readAll( buf, 0, len);
        return buf.length;
    }


    // Two frames back to back, the second far larger than the first, with
    // nothing flushed in between.
    private static function testConsecutiveFrames() : Void {
        var out = new BytesOutput();
        out.bigEndian = true;
        appendFrame( out, 16);
        appendFrame( out, 4096);

        var trans = framedOver( out.getBytes());

        TestBase.Expect( readExactly( trans, 16) == 16, 'first frame reads');
        TestBase.Expect( readExactly( trans, 4096) == 4096, 'second, larger frame reads');
    }


    // A longer run, alternating size, whose total far exceeds any single frame
    // but stays well inside the configured maximum.
    private static function testManyConsecutiveFrames() : Void {
        var sizes = [64, 512, 64, 2048, 128, 4096, 32, 1024];

        var out = new BytesOutput();
        out.bigEndian = true;
        for( sz in sizes) {
            appendFrame( out, sz);
        }

        var trans = framedOver( out.getBytes());
        for( sz in sizes) {
            TestBase.Expect( readExactly( trans, sz) == sz, 'frame of $sz bytes reads');
        }
    }


    // The bound must stay tight: a small frame may not declare a field larger
    // than the frame that carries it.
    private static function testFieldLargerThanFrameIsRejected() : Void {
        var out = new BytesOutput();
        out.bigEndian = true;
        appendFrame( out, 64);

        var trans = framedOver( out.getBytes());
        TestBase.Expect( readExactly( trans, 8) == 8, 'the frame itself reads');

        var rejected = false;
        try {
            trans.CheckReadBytesAvailable( Int64.ofInt( 64 * 1024 * 1024));
        }
        catch( e : TTransportException) {
            rejected = (e.errorID == TTransportException.MESSAGE_SIZE_LIMIT);
        }
        TestBase.Expect( rejected, 'a 64 MB field in a 64-byte frame is refused');
    }


    // A negative size means "back to the configured maximum", which is how netstd and
    // Delphi spell it. Haxe recognised only the omitted argument, so a negative one fell
    // through to the shrink path and left the budget at -1, refusing every later read.
    private static function testNegativeSizeIsAFullReset() : Void {
        var out = new BytesOutput();
        out.bigEndian = true;
        appendFrame( out, 64);

        var trans = framedOver( out.getBytes());
        TestBase.Expect( readExactly( trans, 8) == 8, 'the frame reads');

        trans.ResetMessageSizeAndConsumedBytes( Int64.ofInt(-1));

        var restored = true;
        try {
            trans.CheckReadBytesAvailable( Int64.ofInt(1024));
        }
        catch( e : TTransportException) {
            restored = false;
        }
        TestBase.Expect( restored, 'a negative size restores the configured maximum');
    }


    public static function Run( server : Bool) : Void {
        testConsecutiveFrames();
        testManyConsecutiveFrames();
        testFieldLargerThanFrameIsRejected();
        testNegativeSizeIsAFullReset();
    }
}

#end
