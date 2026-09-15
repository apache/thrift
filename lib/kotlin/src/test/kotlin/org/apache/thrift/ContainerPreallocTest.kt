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

package org.apache.thrift

import kotlin.test.assertEquals
import org.apache.thrift.protocol.TBinaryProtocol
import org.apache.thrift.recursion.RecTree
import org.apache.thrift.transport.TMemoryBuffer
import org.apache.thrift.transport.TMemoryInputTransport
import org.junit.jupiter.api.Test

/**
 * The generated Kotlin decoder reserves an initial container capacity capped at 1024
 * (buildList/buildSet/buildMap with coerceAtMost) from the wire element count, then reads every
 * element. A container larger than that cap must therefore still round-trip with all of its
 * elements -- the cap bounds only the up-front reservation, not the number of elements read.
 */
internal class ContainerPreallocTest {

    private fun config() = TConfiguration.custom().build()

    @Test
    fun listLargerThanPreallocCapRoundTripsInFull() {
        val n = 3000 // well above the 1024 initial-capacity cap
        val leaves = ArrayList<RecTree>(n).apply { repeat(n) { add(RecTree(emptyList())) } }
        val tree = RecTree(leaves)

        val buf = TMemoryBuffer(config(), 1024)
        tree.write(TBinaryProtocol(buf))
        val bytes = buf.array.copyOf(buf.length())

        val read = RecTree()
        read.read(TBinaryProtocol(TMemoryInputTransport(config(), bytes)))

        assertEquals(n, read.children?.size)
    }
}
