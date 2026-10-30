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

import tests.TestBase;

import org.apache.thrift.TConfiguration;
import org.apache.thrift.protocol.*;
import org.apache.thrift.transport.*;


// A server socket with a client connected to it, and the connection it accepted
private typedef Connection = {
    server : TServerSocket,
    client : TSocket,
    accepted : TTransport
};


// A connection accepted by TServerSocket has the configuration of the server socket, so
// the limits in it hold for the transports and protocols layered on top of that connection.
//
// All cases run on real sockets over the loopback interface. The client writes what the
// server is going to read before the connection is accepted, so that one thread can do both.
// All cases run before the test fails, so that the output names every case that is not
// handled as expected.
class ServerSocketConfigTest extends tests.TestBase {

    private static inline var HOST : String = "127.0.0.1";
    private static inline var FIRST_PORT : Int = 19190;
    private static inline var PORTS_TO_TRY : Int = 50;

    private static inline var MAX_MESSAGE_SIZE : Int = 4096;
    private static inline var MAX_FRAME_SIZE : Int = 1000;
    private static inline var RECURSION_LIMIT : Int = 3;

    private static inline var I32_SIZE : Int = 4;
    private static inline var FIELD_ID : Int = 1;

    private static var cases : Int = 0;
    private static var failures : Int = 0;


    private static function limits() : TConfiguration {
        var config = new TConfiguration();
        config.MaxMessageSize = MAX_MESSAGE_SIZE;
        config.MaxFrameSize = MAX_FRAME_SIZE;
        config.RecursionLimit = RECURSION_LIMIT;
        return config;
    }

    private static function connect( config : TConfiguration, wire : Bytes) : Connection {
        for( port in FIRST_PORT ... FIRST_PORT + PORTS_TO_TRY) {
            var server : TServerSocket = null;
            try {
                server = new TServerSocket( HOST, port, 5, false, config);
            }
            catch( e : TTransportException) {
                continue;  // this port is taken
            }
            server.Listen();
            var client = new TSocket( HOST, port);
            client.open();
            client.write( wire, 0, wire.length);
            client.flush();
            return { server : server, client : client, accepted : server.Accept() };
        }
        throw 'no free port from $FIRST_PORT on';
    }

    private static function disconnect( connection : Connection) : Void {
        connection.accepted.close();
        connection.client.close();
        connection.server.Close();
    }

    private static function bigEndian() : BytesOutput {
        var out = new BytesOutput();
        out.bigEndian = true;
        return out;
    }

    private static function payload( out : BytesOutput, size : Int) : Void {
        for( i in 0 ... size) {
            out.writeByte( i & 0xFF);
        }
    }

    private static function isSizeLimit( e : Dynamic) : Bool {
        return Std.isOfType( e, TTransportException)
            && (cast( e, TTransportException).errorID == TTransportException.MESSAGE_SIZE_LIMIT);
    }

    private static function isFrameLimit( e : Dynamic) : Bool {
        return Std.isOfType( e, TTransportException)
            && (cast( e, TTransportException).errorMsg.indexOf( "Frame size") == 0);
    }

    private static function isDepthLimit( e : Dynamic) : Bool {
        return Std.isOfType( e, TProtocolException)
            && (cast( e, TProtocolException).errorID == TProtocolException.DEPTH_LIMIT);
    }

    // Traces the outcome of one case
    private static function Report( title : String, ok : Bool, outcome : String) : Void {
        ++cases;
        if( ! ok) {
            ++failures;
        }
        trace('Test "$title" - ' + (ok ? 'OK' : 'FAILED') + ' ($outcome)');
    }

    // Runs one case on a connection of its own: read() either returns, or throws what isLimit expects.
    private static function Check( title : String, expectAccepted : Bool, wire : Bytes,
                                   isLimit : Dynamic->Bool, read : TTransport->Void) : Void {
        var outcome = "";
        var ok = false;
        try {
            var connection = connect( limits(), wire);
            try {
                read( connection.accepted);
                ok = expectAccepted;
                outcome = 'no exception';
            }
            catch( e : Dynamic) {
                ok = (! expectAccepted) && isLimit( e);
                outcome = '$e';
            }
            disconnect( connection);
        }
        catch( e : Dynamic) {
            outcome = 'setting up the connection failed: $e';
        }
        Report( title, ok, outcome);
    }


    // The very object the server socket was given, not one with the same values
    private static function testConfigurationOfAcceptedConnection() : Void {
        var config = limits();
        var connection = connect( config, Bytes.alloc( 1));
        var found = connection.accepted.Configuration;
        disconnect( connection);
        Report( 'the accepted connection has the configuration of the server socket', found == config, 'identity');
    }

    // A server socket with no configuration has the defaults, and so has what it accepts
    private static function testDefaultsWithoutConfiguration() : Void {
        var connection = connect( null, Bytes.alloc( 1));
        var found = connection.accepted.Configuration;
        var ok = (found.MaxMessageSize == TConfiguration.DEFAULT_MAX_MESSAGE_SIZE)
              && (found.MaxFrameSize == TConfiguration.DEFAULT_MAX_FRAME_SIZE)
              && (found.RecursionLimit == TConfiguration.DEFAULT_RECURSION_DEPTH);
        disconnect( connection);
        Report( 'with no configuration the accepted connection has the defaults', ok,
                '${found.MaxMessageSize}, ${found.MaxFrameSize}, ${found.RecursionLimit}');
    }

    // The accepted socket counts what it reads, up to and including the message size
    private static function testMessageSize( size : Int, expectAccepted : Bool) : Void {
        var out = bigEndian();
        payload( out, size);
        Check( '$size bytes are ' + (expectAccepted ? 'read' : 'refused') + ' on the accepted connection',
               expectAccepted, out.getBytes(), isSizeLimit,
               function( accepted : TTransport) {
                   accepted.readAll( new BytesBuffer(), 0, size);
               });
    }

    // The protocol checks the size a string declares against the message size
    private static function testDeclaredString( size : Int, expectAccepted : Bool) : Void {
        var out = bigEndian();
        out.writeInt32( size);
        payload( out, size);
        Check( 'a string of $size bytes is ' + (expectAccepted ? 'read' : 'refused') + ' by the protocol',
               expectAccepted, out.getBytes(), isSizeLimit,
               function( accepted : TTransport) {
                   new TBinaryProtocol( accepted).readBinary();
               });
    }

    // TFramedTransport finds the frame size in the transport it wraps
    private static function testFrameSize( size : Int, expectAccepted : Bool) : Void {
        var out = bigEndian();
        out.writeInt32( size);
        payload( out, size);
        Check( 'a frame of $size bytes is ' + (expectAccepted ? 'read' : 'refused'),
               expectAccepted, out.getBytes(), isFrameLimit,
               function( accepted : TTransport) {
                   new TFramedTransport( accepted).readAll( new BytesBuffer(), 0, size);
               });
    }

    // The protocol finds the recursion limit in the transport it reads from
    private static function testRecursionLimit( depth : Int, expectAccepted : Bool) : Void {
        var out = bigEndian();
        for( level in 1 ... depth) {
            out.writeByte( TType.STRUCT);
            out.writeInt16( FIELD_ID);
        }
        for( level in 0 ... depth) {
            out.writeByte( TType.STOP);
        }
        Check( 'structs nested $depth deep are ' + (expectAccepted ? 'skipped' : 'refused'),
               expectAccepted, out.getBytes(), isDepthLimit,
               function( accepted : TTransport) {
                   TProtocolUtil.skip( new TBinaryProtocol( accepted), TType.STRUCT);
               });
    }


    public static function Run( server : Bool) : Void {
        cases = 0;
        failures = 0;
        testConfigurationOfAcceptedConnection();
        testDefaultsWithoutConfiguration();
        testMessageSize( MAX_MESSAGE_SIZE, true);
        testMessageSize( MAX_MESSAGE_SIZE + 1, false);
        testDeclaredString( MAX_MESSAGE_SIZE - I32_SIZE, true);
        testDeclaredString( MAX_MESSAGE_SIZE - I32_SIZE + 1, false);
        testFrameSize( MAX_FRAME_SIZE, true);
        testFrameSize( MAX_FRAME_SIZE + 1, false);
        testRecursionLimit( RECURSION_LIMIT, true);
        testRecursionLimit( RECURSION_LIMIT + 1, false);
        TestBase.Expect( failures == 0, 'accepted connections have the configuration of the server socket ($failures of $cases cases failed)');
    }
}

#end
