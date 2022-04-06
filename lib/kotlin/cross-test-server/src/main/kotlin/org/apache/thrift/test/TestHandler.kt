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
package org.apache.thrift.test

import java.nio.ByteBuffer
import kotlinx.coroutines.delay
import org.apache.thrift.TException
import org.slf4j.Logger
import org.slf4j.LoggerFactory
import thrift.test.Insanity
import thrift.test.Numberz
import thrift.test.ThriftTest
import thrift.test.Xception
import thrift.test.Xception2
import thrift.test.Xtruct
import thrift.test.Xtruct2

class TestHandler : ThriftTest {

    companion object {
        private val logger: Logger = LoggerFactory.getLogger(TestHandler::class.java)
    }

    override suspend fun testVoid() {
        logger.info("testVoid()\n")
    }

    override suspend fun testString(thing: String): String {
        logger.info("testString(\"$thing\")\n")
        return thing
    }

    override suspend fun testBool(thing: Boolean): Boolean {
        logger.info("testBool($thing)\n")
        return thing
    }

    override suspend fun testByte(thing: Byte): Byte {
        logger.info("testByte($thing)\n")
        return thing
    }

    override suspend fun testI32(thing: Int): Int {
        logger.info("testI32($thing)\n")
        return thing
    }

    override suspend fun testI64(thing: Long): Long {
        logger.info("testI64($thing)\n")
        return thing
    }

    override suspend fun testDouble(thing: Double): Double {
        logger.info("testDouble($thing)\n")
        return thing
    }

    override suspend fun testBinary(thing: ByteArray): ByteArray {
        val buffer = ByteBuffer.wrap(thing)
        val sb = StringBuilder(buffer.remaining() * 3)
        buffer.mark()
        var limit = 0 // limit output to keep the log size sane
        while (buffer.remaining() > 0 && ++limit < 1024) {
            sb.append(String.format("%02X ", buffer.get()))
        }
        if (buffer.remaining() > 0) {
            sb.append("...") // indicate we have more date
        }
        logger.info("testBinary($sb)\n")
        buffer.reset()
        return buffer.array()
    }

    override suspend fun testStruct(thing: Xtruct): Xtruct {
        logger.info(
            """
testStruct({"${thing.string_thing}", ${thing.byte_thing}, ${thing.i32_thing}, ${thing.i64_thing}})

""".trimIndent()
        )
        return thing
    }

    override suspend fun testNest(thing: Xtruct2): Xtruct2 {
        val thing2: Xtruct = thing.struct_thing!!
        logger.info(
            """
testNest({${thing.byte_thing}, {"${thing2.string_thing}", ${thing2.byte_thing}, ${thing2.i32_thing}, ${thing2.i64_thing}}, ${thing.i32_thing}})

""".trimIndent()
        )
        return thing
    }

    override suspend fun testMap(thing: Map<Int, Int>): Map<Int, Int> {
        logger.info("testMap({")
        logger.info("{}", thing)
        logger.info("})\n")
        return thing
    }

    override suspend fun testStringMap(thing: Map<String, String>): Map<String, String> {
        logger.info("testStringMap({")
        logger.info("{}", thing)
        logger.info("})\n")
        return thing
    }

    override suspend fun testSet(thing: Set<Int>): Set<Int> {
        logger.info("testSet({")
        var first = true
        for (elem in thing) {
            if (first) {
                first = false
            } else {
                logger.info(", ")
            }
            logger.info("{}", elem)
        }
        logger.info("})\n")
        return thing
    }

    override suspend fun testList(thing: List<Int>): List<Int> {
        logger.info("testList({")
        var first = true
        for (elem in thing) {
            if (first) {
                first = false
            } else {
                logger.info(", ")
            }
            logger.info("{}", elem)
        }
        logger.info("})\n")
        return thing
    }

    override suspend fun testEnum(thing: Numberz): Numberz {
        logger.info("testEnum($thing)\n")
        return thing
    }

    override suspend fun testTypedef(thing: Long): Long {
        logger.info("testTypedef($thing)\n")
        return thing
    }

    override suspend fun testMapMap(hello: Int): Map<Int, Map<Int, Int>> {
        logger.info("testMapMap($hello)\n")
        val mapmap: MutableMap<Int, Map<Int, Int>> = HashMap()
        val pos = HashMap<Int, Int>()
        val neg = HashMap<Int, Int>()
        for (i in 1..4) {
            pos[i] = i
            neg[-i] = -i
        }
        mapmap[4] = pos
        mapmap[-4] = neg
        return mapmap
    }

    override suspend fun testInsanity(argument: Insanity): Map<Long, Map<Numberz, Insanity>> {
        logger.info("testInsanity()\n")
        val firstMap = mutableMapOf<Numberz, Insanity>()
        val secondMap = mutableMapOf<Numberz, Insanity>()
        firstMap[Numberz.TWO] = argument
        firstMap[Numberz.THREE] = argument
        val looney = Insanity()
        secondMap[Numberz.SIX] = looney
        val insane: MutableMap<Long, Map<Numberz, Insanity>> = HashMap()
        insane[1L] = firstMap
        insane[2L] = secondMap
        return insane
    }

    override suspend fun testMulti(
        arg0: Byte,
        arg1: Int,
        arg2: Long,
        arg3: Map<Short, String>,
        arg4: Numberz,
        arg5: Long
    ): Xtruct {
        logger.info("testMulti()\n")
        val hello = Xtruct()
        hello.string_thing = "Hello2"
        hello.byte_thing = arg0
        hello.i32_thing = arg1
        hello.i64_thing = arg2
        return hello
    }

    @Throws(Xception::class, TException::class)
    override suspend fun testException(arg: String) {
        logger.info("testException($arg)\n")
        when (arg) {
            "Xception" -> {
                val x = Xception()
                x.errorCode = 1001
                x.setFieldValue(Xception._Fields.MESSAGE, arg)
                throw x
            }
            "TException" -> {
                // Unspecified exception should yield a TApplicationException on client side
                throw RuntimeException(arg)
            }
            else -> {
                val result = Xtruct()
                result.string_thing = arg
            }
        }
        return
    }

    @Throws(Xception::class, Xception2::class)
    override suspend fun testMultiException(arg0: String, arg1: String): Xtruct {
        logger.info("testMultiException($arg0, $arg1)\n")
        if (arg0 == "Xception") {
            val x = Xception()
            x.errorCode = 1001
            x.setFieldValue(Xception._Fields.MESSAGE, "This is an Xception")
            throw x
        } else if (arg0 == "Xception2") {
            val x = Xception2()
            x.errorCode = 2002
            x.struct_thing = Xtruct().apply { string_thing = "This is an Xception2" }
            throw x
        }
        val result = Xtruct()
        result.string_thing = arg1
        return result
    }

    override suspend fun testOneway(secondsToSleep: Int) {
        logger.info("testOneway($secondsToSleep) => sleeping...")
        try {
            delay(secondsToSleep * 1000L)
            logger.info("Done sleeping!")
        } catch (ie: InterruptedException) {
            throw RuntimeException(ie)
        }
    }
}
