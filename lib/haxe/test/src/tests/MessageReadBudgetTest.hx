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

// What the protocol reads from, over a TStreamTransport on an in-memory stream
private enum Layer {
    Endpoint;
    Buffered;
    Framed;
}


// The read budget belongs to one message. A run of one-way calls is the case that shows it:
// nothing flushes between them, so only the end of each message can return the budget. Every
// message of the run fits MaxMessageSize, and all of them together do not.
//
// The budget of a frame covers all that is left of the frame, which can hold another message,
// so the end of a message does not return it.
//
// All cases run before the test fails, so that the output names every case that is not
// handled as expected.
class MessageReadBudgetTest extends tests.TestBase {

    private static inline var MAX_MESSAGE_SIZE : Int = 1024;
    private static inline var MESSAGES_IN_A_RUN : Int = 5;
    private static inline var FITS : Int = 600;
    private static inline var TOO_LARGE : Int = 2 * MAX_MESSAGE_SIZE;
    private static inline var BUFFER_SIZE : Int = TBufferedTransport.MIN_BUFSIZE;

    private static var cases : Int = 0;
    private static var failures : Int = 0;


    private static function wireName( wire : Wire) : String {
        switch( wire) {
            case Binary:   return "TBinaryProtocol";
            case Compact:  return "TCompactProtocol";
            case JSON:     return "TJSONProtocol";
        }
    }

    private static function layerName( layer : Layer) : String {
        switch( layer) {
            case Endpoint:  return "TStreamTransport";
            case Buffered:  return "TBufferedTransport";
            case Framed:    return "TFramedTransport";
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

    // count one-way messages, each with a string of size bytes
    private static function oneWayCalls( wire : Wire, layer : Layer, count : Int, size : Int) : Bytes {
        var stream = new TMemoryStream();
        var endpoint = new TStreamTransport( new TMemoryStream(), stream, new TConfiguration());
        var trans : TTransport = (layer == Framed) ? new TFramedTransport( endpoint) : endpoint;
        var prot = protocol( wire, trans);
        var text = StringTools.rpad( "", "x", size);
        for( seqid in 0 ... count) {
            prot.writeMessageBegin( new TMessage( "oneway", TMessageType.ONEWAY, seqid));
            prot.writeString( text);
            prot.writeMessageEnd();
            trans.flush();
        }
        return contentOf( stream);
    }

    private static function reader( wire : Wire, layer : Layer, data : Bytes) : TProtocol {
        var config = new TConfiguration();
        config.MaxMessageSize = MAX_MESSAGE_SIZE;
        var endpoint = new TStreamTransport( new TMemoryStream( data), new TMemoryStream(), config);
        var trans : TTransport = endpoint;
        switch( layer) {
            case Endpoint:
            case Buffered:  trans = new TBufferedTransport( endpoint, BUFFER_SIZE);
            case Framed:    trans = new TFramedTransport( endpoint);
        }
        return protocol( wire, trans);
    }

    private static function readMessage( prot : TProtocol, seqid : Int, size : Int) : Bool {
        var msg = prot.readMessageBegin();
        var text = prot.readString();
        prot.readMessageEnd();
        return (msg.seqid == seqid) && (text.length == size);
    }

    private static function isSizeLimit( e : Dynamic) : Bool {
        return (Std.isOfType( e, TTransportException)
                && (cast( e, TTransportException).errorID == TTransportException.MESSAGE_SIZE_LIMIT))
            || (Std.isOfType( e, TProtocolException)
                && (cast( e, TProtocolException).errorID == TProtocolException.SIZE_LIMIT));
    }

    private static function Report( title : String, ok : Bool, outcome : String) : Void {
        ++cases;
        if( ! ok) {
            ++failures;
        }
        trace('Test "$title" - ' + (ok ? 'OK' : 'FAILED') + ' ($outcome)');
    }


    // Every message of the run is read, though the run as a whole exceeds the limit
    private static function testRunOfOneWayCalls( wire : Wire, layer : Layer) : Void {
        var title = '${wireName( wire)} on ${layerName( layer)} reads $MESSAGES_IN_A_RUN one-way calls in a row';
        var read = 0;
        var outcome = "";
        try {
            var prot = reader( wire, layer, oneWayCalls( wire, layer, MESSAGES_IN_A_RUN, FITS));
            while( (read < MESSAGES_IN_A_RUN) && readMessage( prot, read, FITS)) {
                ++read;
            }
            outcome = '$read messages read';
        }
        catch( e : Dynamic) {
            outcome = '$read messages read, then $e';
        }
        Report( title, read == MESSAGES_IN_A_RUN, outcome);
    }

    // The limit still applies to each message
    private static function testMessageAboveTheLimit( wire : Wire, layer : Layer) : Void {
        var title = '${wireName( wire)} on ${layerName( layer)} refuses a message above the limit';
        var ok = false;
        var outcome = "";
        try {
            var prot = reader( wire, layer, oneWayCalls( wire, layer, 1, TOO_LARGE));
            readMessage( prot, 0, TOO_LARGE);
            outcome = 'no exception';
        }
        catch( e : Dynamic) {
            ok = isSizeLimit( e);
            outcome = '$e';
        }
        Report( title, ok, outcome);
    }

    // Two messages in one frame: the second one declares a string that is larger than the rest
    // of the frame, though it fits the limit.
    private static function testFrameKeepsItsBudget() : Void {
        var title = 'a message that follows another in a frame is held to the rest of the frame';
        var ok = false;
        var outcome = "";
        try {
            var stream = new TMemoryStream();
            var endpoint = new TStreamTransport( new TMemoryStream(), stream, new TConfiguration());
            var trans = new TFramedTransport( endpoint);
            var prot = new TBinaryProtocol( trans);
            prot.writeMessageBegin( new TMessage( "oneway", TMessageType.ONEWAY, 0));
            prot.writeString( "first");
            prot.writeMessageEnd();
            prot.writeMessageBegin( new TMessage( "oneway", TMessageType.ONEWAY, 1));
            prot.writeI32( FITS);
            trans.flush();

            var input = reader( Binary, Framed, contentOf( stream));
            readMessage( input, 0, "first".length);
            input.readMessageBegin();
            input.readString();
            outcome = 'no exception';
        }
        catch( e : Dynamic) {
            ok = isSizeLimit( e);
            outcome = '$e';
        }
        Report( title, ok, outcome);
    }


    public static function Run( server : Bool) : Void {
        cases = 0;
        failures = 0;
        for( wire in [Binary, Compact, JSON]) {
            for( layer in [Endpoint, Buffered, Framed]) {
                testRunOfOneWayCalls( wire, layer);
                testMessageAboveTheLimit( wire, layer);
            }
        }
        testFrameKeepsItsBudget();
        TestBase.Expect( failures == 0, 'the read budget belongs to one message ($failures of $cases cases failed)');
    }
}

#end
