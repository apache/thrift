/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.apache.thrift

import kotlin.test.assertEquals
import kotlin.test.assertFailsWith
import org.apache.thrift.protocol.TBinaryProtocol
import org.apache.thrift.protocol.TProtocolException
import org.apache.thrift.recursion.CoRec
import org.apache.thrift.recursion.CoRec2
import org.apache.thrift.recursion.RecError
import org.apache.thrift.recursion.RecTree
import org.apache.thrift.recursion.RecUnion
import org.apache.thrift.transport.TMemoryBuffer
import org.apache.thrift.transport.TMemoryInputTransport
import org.junit.jupiter.api.Test

/**
 * Exercises the recursion-depth limit through the *generated* struct read/write path (TBase.read /
 * TBase.write) -- the real code path that deeply nested input exercises -- rather than calling
 * TProtocol.incrementRecursionDepth / decrementRecursionDepth in isolation.
 *
 * The recursive types CoRec / CoRec2 / RecTree / RecUnion / RecError are generated from
 * src/test/resources/RecursionDepthTest.thrift (mirroring test/Recursive.thrift): CoRec <-> CoRec2
 * form a mutually recursive chain, RecTree is a wide tree, and RecUnion / RecError are
 * self-recursive union and exception types.
 *
 * Struct and exception serialization is routed through TProtocol's readStruct {} / writeStruct {}
 * helpers, which is where the bound lives; recursive unions are read/written by TUnion, which
 * bounds them the same way. The limit is taken from TConfiguration.getRecursionLimit(), so each
 * test uses a small custom limit for clarity.
 */
internal class RecursionDepthTest {

    private val limit = 8

    private fun config(recursionLimit: Int) =
        TConfiguration.custom().setRecursionLimit(recursionLimit).build()

    // Build a CoRec/CoRec2 chain that is exactly 'depth' structs deep.
    private fun makeNestedRecs(depth: Int): CoRec? =
        if (depth <= 0) null else CoRec(makeNestedCoRec2(depth - 1))

    private fun makeNestedCoRec2(depth: Int): CoRec2? =
        if (depth <= 0) null else CoRec2(makeNestedRecs(depth - 1))

    // Build a RecUnion chain that is exactly 'depth' unions deep; the innermost union holds the
    // non-recursive 'leaf' so the value is finite and writable.
    private fun makeNestedUnion(depth: Int): RecUnion =
        if (depth <= 1) RecUnion(RecUnion._Fields.LEAF, 0.toShort())
        else RecUnion(RecUnion._Fields.CHILD, makeNestedUnion(depth - 1))

    // Build a RecError chain that is exactly 'depth' exceptions deep.
    private fun makeNestedError(depth: Int): RecError =
        if (depth <= 1) RecError(leaf = 0) else RecError(child = makeNestedError(depth - 1))

    // Serialize via the generated write() over a protocol with the given limit.
    private fun serialize(data: TBase<*, *>, recursionLimit: Int): ByteArray {
        val buf = TMemoryBuffer(config(recursionLimit), 1024)
        data.write(TBinaryProtocol(buf))
        return buf.array.copyOf(buf.length())
    }

    // Deserialize via the generated read() over a protocol with the given limit.
    private fun <T : TBase<*, *>> deserialize(into: T, bytes: ByteArray, recursionLimit: Int): T {
        into.read(TBinaryProtocol(TMemoryInputTransport(config(recursionLimit), bytes)))
        return into
    }

    @Test
    fun roundTripOneBelowLimitSucceeds() {
        val bytes = serialize(makeNestedRecs(limit - 1)!!, limit)
        deserialize(CoRec(), bytes, limit)
    }

    @Test
    fun roundTripAtLimitSucceeds() {
        val bytes = serialize(makeNestedRecs(limit)!!, limit)
        deserialize(CoRec(), bytes, limit)
    }

    @Test
    fun writeOneOverLimitThrows() {
        val ex =
            assertFailsWith<TProtocolException> { serialize(makeNestedRecs(limit + 1)!!, limit) }
        assertEquals(TProtocolException.DEPTH_LIMIT, ex.type)
    }

    @Test
    fun readOneOverLimitThrows() {
        // Produce a valid over-limit payload with a higher write limit, then read it
        // back with the real limit -- mimicking a message arriving from the network.
        val bytes = serialize(makeNestedRecs(limit + 1)!!, limit + 1)
        val ex = assertFailsWith<TProtocolException> { deserialize(CoRec(), bytes, limit) }
        assertEquals(TProtocolException.DEPTH_LIMIT, ex.type)
    }

    @Test
    fun wideStructureRoundTrips() {
        // Many siblings (>> limit) must still round-trip: this only holds if the
        // counter is decremented for each sibling back to depth 1.
        val children =
            (0 until limit * 3).map { RecTree(children = emptyList(), item = it.toShort()) }
        val bytes = serialize(RecTree(children = children, item = 0.toShort()), limit)
        deserialize(RecTree(), bytes, limit)
    }

    @Test
    fun cyclicGraphThrows() {
        val data = makeNestedRecs(2)!! // CoRec -> CoRec2 -> null
        data.other!!.other = data // close the loop: CoRec2.other -> CoRec
        val ex = assertFailsWith<TProtocolException> { serialize(data, limit) }
        assertEquals(TProtocolException.DEPTH_LIMIT, ex.type)
    }

    @Test
    fun unionRoundTripAtLimitSucceeds() {
        val bytes = serialize(makeNestedUnion(limit), limit)
        deserialize(RecUnion(), bytes, limit)
    }

    @Test
    fun unionWriteOneOverLimitThrows() {
        val ex =
            assertFailsWith<TProtocolException> { serialize(makeNestedUnion(limit + 1), limit) }
        assertEquals(TProtocolException.DEPTH_LIMIT, ex.type)
    }

    @Test
    fun unionReadOneOverLimitThrows() {
        val bytes = serialize(makeNestedUnion(limit + 1), limit + 1)
        val ex = assertFailsWith<TProtocolException> { deserialize(RecUnion(), bytes, limit) }
        assertEquals(TProtocolException.DEPTH_LIMIT, ex.type)
    }

    @Test
    fun exceptionRoundTripAtLimitSucceeds() {
        val bytes = serialize(makeNestedError(limit), limit)
        deserialize(RecError(), bytes, limit)
    }

    @Test
    fun exceptionWriteOneOverLimitThrows() {
        val ex =
            assertFailsWith<TProtocolException> { serialize(makeNestedError(limit + 1), limit) }
        assertEquals(TProtocolException.DEPTH_LIMIT, ex.type)
    }

    @Test
    fun exceptionReadOneOverLimitThrows() {
        val bytes = serialize(makeNestedError(limit + 1), limit + 1)
        val ex = assertFailsWith<TProtocolException> { deserialize(RecError(), bytes, limit) }
        assertEquals(TProtocolException.DEPTH_LIMIT, ex.type)
    }
}
