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
import tests.TestBase;
#if sys

import haxe.Int64;
import haxe.io.Bytes;
import haxe.io.BytesBuffer;
import sys.FileSystem;

import org.apache.thrift.*;
import org.apache.thrift.protocol.*;
import org.apache.thrift.transport.*;
import org.apache.thrift.server.*;
import org.apache.thrift.meta_data.*;

import thrift.test.*;  // generated code


class StreamTest extends tests.TestBase {


    private inline static var tmpfile : String = "data.tmp";


    private static function MakeTestData() : Xtruct {
        var data : Xtruct = new Xtruct();
        data.string_thing = "Streamtest";
        data.byte_thing = -128;
        data.i32_thing = 4711;
        data.i64_thing = Int64.make(0x12345678,0x9ABCDEF0);
        return data;
    }

    public static function WriteData() : Xtruct
    {
        var config : TConfiguration = new TConfiguration();
        var stream : TStream = new TFileStream( tmpfile, CreateNew);
        var trans : TTransport = new TStreamTransport( null, stream, config);
        var prot = new TJSONProtocol( trans);

        var data = MakeTestData();
        data.write(prot);
        trans.close();

        return data;
    }

    public static function ReadData() : Xtruct
    {
        var config : TConfiguration = new TConfiguration();
        var stream : TStream = new TFileStream( tmpfile, Read);
        var trans : TTransport = new TStreamTransport( stream, null, config);
        var prot = new TJSONProtocol( trans);

        var data : Xtruct = new Xtruct();
        data.read(prot);
        trans.close();

        return data;
    }

    // Round-trip the same data through a single in-memory TMemoryStream (no temp
    // file): the stream is written, rewound (Position = 0) and read back, which
    // only works if TMemoryStream grows on write.
    public static function WriteReadViaMemory() : { written : Xtruct, read : Xtruct }
    {
        var config : TConfiguration = new TConfiguration();
        var stream = new TMemoryStream();
        var trans : TTransport = new TStreamTransport( stream, stream, config);
        var prot = new TJSONProtocol( trans);

        var written = MakeTestData();
        written.write(prot);

        stream.Position = 0;  // rewind to read back what was just written
        var read : Xtruct = new Xtruct();
        read.read(prot);

        return { written : written, read : read };
    }

    // A fresh TMemoryStream must grow as bytes are appended and return them
    // intact after rewinding (directly exercises the grow-on-write path).
    private static function MemoryStreamGrows() : Bool
    {
        var stream = new TMemoryStream();
        var n = 1000;
        for ( i in 0...n) {
            var one = haxe.io.Bytes.alloc(1);
            one.set( 0, i & 0xFF);
            stream.Write( one, 0, 1);
        }
        if ( stream.Position != n) {
            return false;
        }
        stream.Position = 0;
        var back = haxe.io.Bytes.alloc(n);
        if ( stream.Read( back, 0, n) != n) {
            return false;
        }
        for ( i in 0...n) {
            if ( back.get(i) != (i & 0xFF)) {
                return false;
            }
        }
        return true;
    }

    // MaxMessageSize is meant as "a general device to be used with any transport or
    // protocol" (doc/specs/thrift-tconfiguration.md), and it is expressed as the number of
    // bytes *remaining* to be read -- which only means something if reads draw it down.
    // Every other Haxe endpoint charges reads against it; TStreamTransport did not, so the
    // limit had no effect on a stream-backed connection however much was read.
    private static function streamTransportWithLimit( maxMessageSize : Int) : TStreamTransport {
        var config = new TConfiguration();
        config.MaxMessageSize = maxMessageSize;
        var stream = new TMemoryStream( Bytes.alloc( 4 * maxMessageSize));
        stream.Position = 0;
        return new TStreamTransport( stream, stream, config);
    }

    // Reading past the limit, a chunk at a time, must be refused. No single read here
    // exceeds it, so nothing but cumulative accounting can catch this.
    public static function ReadsAreChargedAgainstTheBudget() : Void {
        var limit = 256;
        var trans = streamTransportWithLimit( limit);

        var refused = false;
        try {
            var read = 0;
            while( read < 4 * limit) {
                var buf = new BytesBuffer();
                trans.read( buf, 0, 32);
                read += 32;
            }
        }
        catch( e : TTransportException) {
            refused = (e.errorID == TTransportException.MESSAGE_SIZE_LIMIT);
        }
        tests.TestBase.Expect( refused, "stream: reading past MaxMessageSize is refused");
    }

    // ... and the allowance has to come back, or one long-lived connection would run itself
    // out of budget over successive messages. flush() is where the other endpoints do it.
    public static function FlushRestoresTheBudget() : Void {
        var limit = 256;
        var trans = streamTransportWithLimit( limit);

        var buf = new BytesBuffer();
        trans.read( buf, 0, 200);
        trans.flush();

        var ok = true;
        try {
            var again = new BytesBuffer();
            trans.read( again, 0, 200);
        }
        catch( e : TTransportException) {
            ok = false;
        }
        tests.TestBase.Expect( ok, "stream: flush restores the allowance for the next message");
    }


    public static function Run(server : Bool) : Void
    {
        try {
            var written = WriteData();
            var read = ReadData();
            FileSystem.deleteFile(tmpfile);

            tests.TestBase.Expect( read.string_thing == written.string_thing, "string data");
            tests.TestBase.Expect( read.byte_thing == written.byte_thing, "byte data");
            tests.TestBase.Expect( read.i32_thing == written.i32_thing, "i32 data");
            tests.TestBase.Expect( Int64.compare( read.i64_thing, written.i64_thing) == 0, "i64 data");

            var mem = WriteReadViaMemory();
            tests.TestBase.Expect( mem.read.string_thing == mem.written.string_thing, "memory: string data");
            tests.TestBase.Expect( mem.read.byte_thing == mem.written.byte_thing, "memory: byte data");
            tests.TestBase.Expect( mem.read.i32_thing == mem.written.i32_thing, "memory: i32 data");
            tests.TestBase.Expect( Int64.compare( mem.read.i64_thing, mem.written.i64_thing) == 0, "memory: i64 data");
            tests.TestBase.Expect( MemoryStreamGrows(), "memory: grows across many writes");

        } catch(e:Dynamic) {
            // Only the two calls above leave the temp file behind, and everything after them
            // has already deleted it -- so deleting unconditionally here throws
            // std@file_delete over the top of whatever actually failed.
            if( FileSystem.exists(tmpfile)) {
                FileSystem.deleteFile(tmpfile);
            }
            throw e;
        }

        // Outside the block above: neither of these uses the temp file.
        ReadsAreChargedAgainstTheBudget();
        FlushRestoresTheBudget();
    }

}


#end
