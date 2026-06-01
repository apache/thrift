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

import org.apache.thrift.*;
import org.apache.thrift.protocol.*;
import org.apache.thrift.transport.*;

// recursive types from test/Recursive.thrift (no namespace -> root package)
import CoRec;
import CoRec2;
import CoUnion;
import CoUnion2;
import CoError;
import CoError2;

// Exercises the struct read/write recursion-depth guard (TConfiguration.RecursionLimit,
// default 64) through full round-trips over the recursive types in test/Recursive.thrift,
// for a struct (CoRec), a union (CoUnion) and an exception (CoError).
//
// A chain exactly at the limit must round-trip (this is what would fail if the guard
// double-counted, e.g. also incremented in writeStructBegin), while one level past the
// limit must be rejected with DEPTH_LIMIT on both write and read.
class RecursionLimitTest extends tests.TestBase {

    private static inline var LIMIT : Int = TConfiguration.DEFAULT_RECURSION_DEPTH; // 64

    // ---- builders (alternating Co* / Co*2, null leaf) ----
    private static function buildCoRec(d : Int) : CoRec {
        var n = new CoRec();
        if (d > 1) n.other = buildCoRec2(d - 1);
        return n;
    }
    private static function buildCoRec2(d : Int) : CoRec2 {
        var n = new CoRec2();
        if (d > 1) n.other = buildCoRec(d - 1);
        return n;
    }
    private static function depthCoRec(n : CoRec) : Int {
        return (n.other == null) ? 1 : 1 + depthCoRec2(n.other);
    }
    private static function depthCoRec2(n : CoRec2) : Int {
        return (n.other == null) ? 1 : 1 + depthCoRec(n.other);
    }

    private static function buildCoUnion(d : Int) : CoUnion {
        var n = new CoUnion();
        if (d > 1) n.other = buildCoUnion2(d - 1);
        return n;
    }
    private static function buildCoUnion2(d : Int) : CoUnion2 {
        var n = new CoUnion2();
        if (d > 1) n.other = buildCoUnion(d - 1);
        return n;
    }

    private static function buildCoError(d : Int) : CoError {
        var n = new CoError();
        if (d > 1) n.other = buildCoError2(d - 1);
        return n;
    }
    private static function buildCoError2(d : Int) : CoError2 {
        var n = new CoError2();
        if (d > 1) n.other = buildCoError(d - 1);
        return n;
    }

    // A single growable in-memory stream backs both directions: write into it,
    // rewind (Position = 0), then read back -- no temporary file required.
    private static function newStream() : TMemoryStream {
        return new TMemoryStream();
    }
    private static function protOf(s : TMemoryStream) : TProtocol {
        return new TBinaryProtocol(new TStreamTransport(s, s, new TConfiguration()));
    }

    // Raw nested struct (field id 1, type STRUCT) written with the unguarded protocol
    // primitives (writeStructBegin does not increment the depth -- single-count), used to
    // craft an over-limit payload that trips the guard only on read.
    private static function craftDeep(p : TProtocol, d : Int) : Void {
        p.writeStructBegin(new TStruct("Rec"));
        if (d > 1) {
            p.writeFieldBegin(new TField("other", TType.STRUCT, 1));
            craftDeep(p, d - 1);
            p.writeFieldEnd();
        }
        p.writeFieldStop();
        p.writeStructEnd();
    }

    private static function isDepthLimit(e : Dynamic) : Bool {
        return Std.isOfType(e, TProtocolException)
            && (cast(e, TProtocolException).errorID == TProtocolException.DEPTH_LIMIT);
    }

    public static function Run(server : Bool) : Void {
        // ---- struct (CoRec) ----
        var s = newStream();
        var p = protOf(s);
        buildCoRec(LIMIT).write(p);
        s.Position = 0;
        var back = new CoRec();
        back.read(p);
        TestBase.Expect(depthCoRec(back) == LIMIT, 'struct: $LIMIT-deep round-trip preserves depth');

        ExpectDepthLimitOnWrite(buildCoRec(LIMIT + 1), 'struct');
        ExpectDepthLimitOnRead(function(pr) return new CoRec().read(pr), 'struct');

        // ---- union (CoUnion) ----
        var s2 = newStream();
        var p2 = protOf(s2);
        buildCoUnion(LIMIT).write(p2);
        s2.Position = 0;
        new CoUnion().read(p2);
        TestBase.Expect(true, 'union: $LIMIT-deep round-trip succeeds');

        ExpectDepthLimitOnWrite(buildCoUnion(LIMIT + 1), 'union');
        ExpectDepthLimitOnRead(function(pr) return new CoUnion().read(pr), 'union');

        // ---- exception (CoError) ----
        var s3 = newStream();
        var p3 = protOf(s3);
        buildCoError(LIMIT).write(p3);
        s3.Position = 0;
        new CoError().read(p3);
        TestBase.Expect(true, 'exception: $LIMIT-deep round-trip succeeds');

        ExpectDepthLimitOnWrite(buildCoError(LIMIT + 1), 'exception');
        ExpectDepthLimitOnRead(function(pr) return new CoError().read(pr), 'exception');
    }

    private static function ExpectDepthLimitOnWrite(obj : TBase, what : String) : Void {
        var threw = false;
        var p = protOf(newStream());
        try {
            obj.write(p);
        } catch (e : Dynamic) {
            threw = isDepthLimit(e);
        }
        TestBase.Expect(threw, '$what: write ${LIMIT + 1}-deep throws DEPTH_LIMIT');
    }

    private static function ExpectDepthLimitOnRead(readInto : TProtocol -> Void, what : String) : Void {
        // craftDeep uses raw primitives (no depth guard on write), so the write side
        // does not throw; only the guarded read below trips the limit.
        var s = newStream();
        var p = protOf(s);
        craftDeep(p, LIMIT + 1);
        s.Position = 0;
        var threw = false;
        try {
            readInto(p);
        } catch (e : Dynamic) {
            threw = isDepthLimit(e);
        }
        TestBase.Expect(threw, '$what: read ${LIMIT + 1}-deep throws DEPTH_LIMIT');
    }

}

#end
