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
import haxe.io.BytesOutput;

import tests.TestBase;

import org.apache.thrift.TConfiguration;
import org.apache.thrift.protocol.*;
import org.apache.thrift.transport.*;


// The transport compositions, all of them reading from an in-memory stream
private enum Stack {
    Plain;      // TMemoryStream -> TStreamTransport(config)
    Buffered;   // TMemoryStream -> TStreamTransport(config) -> TBufferedTransport
    Framed;     // TMemoryStream -> TStreamTransport(config) -> TFramedTransport
}

// The reads, each with a size of -1 on the wire
private enum Read {
    CompactString;  // varint FF FF FF FF 0F, cast to Int
    CompactBinary;  // varint FF FF FF FF 0F, cast to Int
    BinaryString;   // i32 length FF FF FF FF
    BinaryBinary;   // i32 length FF FF FF FF
    BinaryList;     // element type i32 (08), then count FF FF FF FF
}


// A negative string, binary or container size read off the wire must be rejected by
// CheckReadBytesAvailable() on every transport composition. The endpoint transport
// rejects it with MESSAGE_SIZE_LIMIT. The layered transports must pass such a size on
// to it instead of treating it as already satisfied by the bytes they hold.
//
// Each read runs with and without trailing bytes after the value, so that the layered
// read buffer is empty in one run and not empty in the other. All cases run before the
// test fails, so that the output names every composition that accepts the size. A valid
// size runs first on each composition, and must still be accepted.
class NegativeSizeTest extends tests.TestBase {

    private static inline var TRAILING_BYTES : Int = 8;
    private static inline var MAX_MESSAGE_SIZE : Int = 4096;  // well above any frame used here

    private static function stackName( stack : Stack) : String {
        switch( stack) {
            case Plain:     return "TStreamTransport";
            case Buffered:  return "TBufferedTransport";
            case Framed:    return "TFramedTransport";
        }
    }

    private static function readName( read : Read) : String {
        switch( read) {
            case CompactString:  return "compact readString";
            case CompactBinary:  return "compact readBinary";
            case BinaryString:   return "binary readString";
            case BinaryBinary:   return "binary readBinary";
            case BinaryList:     return "binary readListBegin";
        }
    }

    private static function payload( read : Read) : Bytes {
        switch( read) {
            case CompactString, CompactBinary:  return Bytes.ofHex("ffffffff0f");
            case BinaryString, BinaryBinary:    return Bytes.ofHex("ffffffff");
            case BinaryList:                    return Bytes.ofHex("08ffffffff");
        }
    }

    // The value, followed by the given number of zero bytes. The framed composition gets
    // all of it in one frame (a four-byte big-endian length, then the bytes), so that the
    // trailing bytes are part of the frame and remain in the read buffer.
    private static function wire( stack : Stack, data : Bytes, trailing : Int) : Bytes {
        var out = new BytesOutput();
        out.bigEndian = true;
        if( stack == Framed) {
            out.writeInt32( data.length + trailing);
        }
        out.writeBytes( data, 0, data.length);
        for( i in 0 ... trailing) {
            out.writeByte( 0);
        }
        return out.getBytes();
    }

    private static function transport( stack : Stack, data : Bytes) : TTransport {
        var config = new TConfiguration();
        config.MaxMessageSize = MAX_MESSAGE_SIZE;
        var endpoint = new TStreamTransport( new TMemoryStream( data), null, config);
        switch( stack) {
            case Plain:     return endpoint;
            case Buffered:  return new TBufferedTransport( endpoint);
            case Framed:    return new TFramedTransport( endpoint);
        }
    }

    // Performs the read and describes what it returned, should it return at all
    private static function perform( read : Read, trans : TTransport) : String {
        switch( read) {
            case CompactString:
                var value = new TCompactProtocol( trans).readString();
                return 'returned a string of length ${value.length}';
            case CompactBinary:
                var value = new TCompactProtocol( trans).readBinary();
                return 'returned ${value.length} bytes';
            case BinaryString:
                var value = new TBinaryProtocol( trans).readString();
                return 'returned a string of length ${value.length}';
            case BinaryBinary:
                var value = new TBinaryProtocol( trans).readBinary();
                return 'returned ${value.length} bytes';
            case BinaryList:
                var list = new TBinaryProtocol( trans).readListBegin();
                return 'returned a list of count ${list.size}';
        }
    }

    private static function isSizeLimit( e : Dynamic) : Bool {
        return Std.isOfType( e, TTransportException)
            && (cast( e, TTransportException).errorID == TTransportException.MESSAGE_SIZE_LIMIT);
    }

    // Runs one case and traces its outcome; returns whether the size was rejected
    private static function ExpectRejected( stack : Stack, read : Read, trailing : Int) : Bool {
        var title = '${readName( read)}: a negative size is rejected on ${stackName( stack)} ($trailing trailing bytes)';
        var outcome = "";
        var rejected = false;
        try {
            outcome = 'no exception, ' + perform( read, transport( stack, wire( stack, payload( read), trailing)));
        }
        catch( e : Dynamic) {
            rejected = isSizeLimit( e);
            outcome = rejected
                ? cast( e, TTransportException).errorMsg
                : 'expected TTransportException MESSAGE_SIZE_LIMIT, got $e';
        }
        trace('Test "$title" - ' + (rejected ? 'OK' : 'FAILED') + ' ($outcome)');
        return rejected;
    }

    // A size that the bytes at hand can satisfy is still accepted, and the value reads back
    private static function ValidSizeIsAccepted() : Void {
        for( stack in [Plain, Buffered, Framed]) {
            for( trailing in [0, TRAILING_BYTES]) {
                var trans = transport( stack, wire( stack, Bytes.ofHex("00000003616263"), trailing));
                TestBase.Expect( new TBinaryProtocol( trans).readString() == "abc",
                                 'binary readString: a valid size is accepted on ${stackName( stack)} ($trailing trailing bytes)');
            }
        }
    }

    private static function NegativeSizeIsRejected() : Void {
        var cases = 0;
        var failures = 0;
        for( stack in [Plain, Buffered, Framed]) {
            for( read in [CompactString, CompactBinary, BinaryString, BinaryBinary, BinaryList]) {
                for( trailing in [0, TRAILING_BYTES]) {
                    ++cases;
                    if( ! ExpectRejected( stack, read, trailing)) {
                        ++failures;
                    }
                }
            }
        }
        TestBase.Expect( failures == 0, 'a negative size is rejected on every transport composition ($failures of $cases cases failed)');
    }

    public static function Run( server : Bool) : Void {
        ValidSizeIsAccepted();
        NegativeSizeIsRejected();
    }
}

#end
