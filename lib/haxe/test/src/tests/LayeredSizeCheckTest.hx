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


// The layered transports, each over a TStreamTransport on in-memory streams
private enum Layer {
    Buffered;
    Framed;
}

// What happened on the layered transport before the size is checked
private enum State {
    Fresh;       // nothing at all
    AfterFlush;  // one value read, then a write and a flush
}

// The size to check, and the outcome the endpoint transport gives it
private enum Size {
    Small;     // fits the message size limit, accepted
    TooLarge;  // above the message size limit, MESSAGE_SIZE_LIMIT
    Negative;  // MESSAGE_SIZE_LIMIT
}


// CheckReadBytesAvailable() on a layered transport subtracts the bytes it holds in its read
// buffer and passes the rest on to the inner transport. A layered transport has no read buffer
// before its first read, and TFramedTransport drops its read buffer in flush(). In either state
// the check must count no buffered bytes, so that the inner transport decides on every size.
//
// All cases run before the test fails, so that the output names every case that is not
// handled as expected.
class LayeredSizeCheckTest extends tests.TestBase {

    private static inline var MAX_MESSAGE_SIZE : Int = 4096;

    private static function layerName( layer : Layer) : String {
        switch( layer) {
            case Buffered:  return "TBufferedTransport";
            case Framed:    return "TFramedTransport";
        }
    }

    private static function stateName( state : State) : String {
        switch( state) {
            case Fresh:       return "before the first read";
            case AfterFlush:  return "after a flush";
        }
    }

    private static function numBytes( size : Size) : Int {
        switch( size) {
            case Small:     return 16;
            case TooLarge:  return 2 * MAX_MESSAGE_SIZE;
            case Negative:  return -1;
        }
    }

    // One i32 with the value 1; the framed transport gets it as a frame of four bytes
    private static function wire( layer : Layer) : Bytes {
        switch( layer) {
            case Buffered:  return Bytes.ofHex("00000001");
            case Framed:    return Bytes.ofHex("0000000400000001");
        }
    }

    private static function transport( layer : Layer, state : State) : TTransport {
        var config = new TConfiguration();
        config.MaxMessageSize = MAX_MESSAGE_SIZE;
        var endpoint = new TStreamTransport( new TMemoryStream( wire( layer)), new TMemoryStream(), config);
        var trans : TTransport;
        switch( layer) {
            case Buffered:  trans = new TBufferedTransport( endpoint);
            case Framed:    trans = new TFramedTransport( endpoint);
        }
        switch( state) {
            case Fresh:
            case AfterFlush:
                new TBinaryProtocol( trans).readI32();
                trans.write( Bytes.ofHex("00"), 0, 1);
                trans.flush();
        }
        return trans;
    }

    private static function isSizeLimit( e : Dynamic) : Bool {
        return Std.isOfType( e, TTransportException)
            && (cast( e, TTransportException).errorID == TTransportException.MESSAGE_SIZE_LIMIT);
    }

    // Runs one case and traces its outcome; returns whether it came out as expected
    private static function Check( layer : Layer, state : State, size : Size) : Bool {
        var n = numBytes( size);
        var expectAccepted = (size == Small);
        var title = '${layerName( layer)} ${stateName( state)}: a size of $n is '
                  + (expectAccepted ? 'accepted' : 'rejected');
        var outcome = "";
        var ok = false;
        try {
            var trans = transport( layer, state);
            try {
                trans.CheckReadBytesAvailable( n);
                ok = expectAccepted;
                outcome = 'no exception';
            }
            catch( e : Dynamic) {
                ok = (! expectAccepted) && isSizeLimit( e);
                outcome = ok
                    ? cast( e, TTransportException).errorMsg
                    : 'expected ' + (expectAccepted ? 'no exception' : 'TTransportException MESSAGE_SIZE_LIMIT') + ', got $e';
            }
        }
        catch( e : Dynamic) {
            outcome = 'setting up the transport failed: $e';
        }
        trace('Test "$title" - ' + (ok ? 'OK' : 'FAILED') + ' ($outcome)');
        return ok;
    }

    public static function Run( server : Bool) : Void {
        var cases = 0;
        var failures = 0;
        for( layer in [Buffered, Framed]) {
            for( state in [Fresh, AfterFlush]) {
                for( size in [Small, TooLarge, Negative]) {
                    ++cases;
                    if( ! Check( layer, state, size)) {
                        ++failures;
                    }
                }
            }
        }
        TestBase.Expect( failures == 0, 'the layered transports check sizes with no read buffer ($failures of $cases cases failed)');
    }
}

#end
