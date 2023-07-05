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

import com.github.ajalt.clikt.core.CliktCommand
import com.github.ajalt.clikt.parameters.options.default
import com.github.ajalt.clikt.parameters.options.flag
import com.github.ajalt.clikt.parameters.options.option
import com.github.ajalt.clikt.parameters.types.enum
import com.github.ajalt.clikt.parameters.types.int
import java.nio.ByteBuffer
import kotlin.math.abs
import kotlin.system.exitProcess
import kotlinx.coroutines.runBlocking
import org.apache.thrift.TApplicationException
import org.apache.thrift.TException
import org.apache.thrift.TSerializer
import org.apache.thrift.async.TAsyncClientManager
import org.apache.thrift.protocol.TBinaryProtocol
import org.apache.thrift.protocol.TCompactProtocol
import org.apache.thrift.protocol.TJSONProtocol
import org.apache.thrift.protocol.TProtocol
import org.apache.thrift.protocol.TSimpleJSONProtocol
import org.apache.thrift.transport.TNonblockingSocket
import org.apache.thrift.transport.TNonblockingTransport
import org.apache.thrift.transport.TTransport
import thrift.test.Insanity
import thrift.test.Numberz
import thrift.test.SecondServiceClient
import thrift.test.ThriftTestClient
import thrift.test.Xception
import thrift.test.Xception2
import thrift.test.Xtruct
import thrift.test.Xtruct2

/**
 * Test Java client for thrift. Essentially just a copy of the C++ version, this makes a variety of
 * requests to enable testing for both performance and correctness of the output.
 */
const val ERR_BASETYPES = 1
const val ERR_STRUCTS = 2
const val ERR_CONTAINERS = 4
const val ERR_EXCEPTIONS = 8
const val ERR_PROTOCOLS = 16
const val ERR_UNKNOWN = 64

enum class ProtocolType(val key: String) {
    Binary("binary"),
    Multi("multi"),
    Json("json"),
    MultiJson("multij"),
    Compact("compact"),
    MultiCompact("multic")
}

enum class TransportType(val key: String) {
    Buffered("buffered"),
    Framed("framed"),
    FastFramed("fastframed"),
    Http("http")
}

class TestClient : CliktCommand() {
    private val host: String by
        option(help = "The cross test host to connect to").default("localhost")
    private val port: Int by option(help = "The cross test port to connect to").int().default(9090)
    private val numTests: Int by
        option("--testloops", "--n", help = "Number of runs in this test").int().default(1)
    private val protocolType: ProtocolType by
        option("--protocol", help = "Protocol type")
            .enum<ProtocolType> { it.key }
            .default(ProtocolType.Binary)
    private val transportType: TransportType by
        option("--transport", help = "Transport type")
            .enum<TransportType> { it.key }
            .default(TransportType.Buffered)
    private val useHttpClient: Boolean by
        option("--client", help = "Use http client").flag(default = false)
    private val useSSL: Boolean by
        option("--ssl", help = "Use SSL for encrypted transport").flag(default = false)
    private val useZlib: Boolean by
        option("--zlib", help = "Use zlib wrapper for compressed transport").flag(default = false)
    private val socketTimeout: Int by
        option("--timeout", help = "Socket timeout").int().default(1000)

    private fun createProtocol(transport: TTransport): TProtocol =
        when (protocolType) {
            ProtocolType.Binary,
            ProtocolType.Multi -> TBinaryProtocol(transport)
            ProtocolType.Compact,
            ProtocolType.MultiCompact -> TCompactProtocol(transport)
            ProtocolType.Json,
            ProtocolType.MultiJson -> TJSONProtocol(transport)
        }

    private fun createTransport(): TNonblockingTransport =
        when (transportType) {
            TransportType.Framed -> TNonblockingSocket(host, port, socketTimeout)
            else ->
                throw UnsupportedOperationException(
                    "only frame transport type is supported for now, got $transportType"
                )
        }

    private val clientManager = TAsyncClientManager()

    private fun createClient(): ThriftTestClient =
        ThriftTestClient({ createProtocol(it) }, clientManager, createTransport())

    private fun createSecondServiceClient(): SecondServiceClient =
        SecondServiceClient({ createProtocol(it) }, clientManager, createTransport())

    override fun run() = runBlocking {
        var testClient = createClient()
        val insane = Insanity()
        var timeMin: Long = 0
        var timeMax: Long = 0
        var timeTot: Long = 0
        var returnCode = 0
        for (test in 0 until numTests) {
            try {
                /** CONNECT TEST */
                /** CONNECT TEST */
                //  if (!transport.isOpen) {
                //      try {
                //          transport.open()
                //      } catch (ttx: TTransportException) {
                //          ttx.printStackTrace()
                //          println("Connect failed: " + ttx.message)
                //          exitProcess(ERR_UNKNOWN)
                //      }
                //  }
                println("Test #${test + 1}, connect $host:$port")

                val start = System.nanoTime()
                /** VOID TEST */
                /** VOID TEST */
                returnCode = testClient.voidTest(returnCode)
                /** STRING TEST */
                /** STRING TEST */
                returnCode = testClient.stringTest(returnCode)
                /** Multiplexed test */
                /** Multiplexed test */
                returnCode = multiplexTest(returnCode)
                /** BYTE TEST */
                /** BYTE TEST */
                returnCode = testClient.byteTest(returnCode)
                /** I32 TEST */
                /** I32 TEST */
                returnCode = testClient.i32Test(returnCode)
                /** I64 TEST */
                /** I64 TEST */
                returnCode = testClient.i64Test(returnCode)
                /** DOUBLE TEST */
                /** DOUBLE TEST */
                returnCode = testClient.doubleTest(returnCode)
                /** BINARY TEST */
                /** BINARY TEST */
                returnCode = testClient.binaryTest(returnCode)
                /** STRUCT TEST */
                /** STRUCT TEST */
                val pair = testClient.structTest(returnCode)
                val out = pair.first
                returnCode = pair.second
                /** NESTED STRUCT TEST */
                /** NESTED STRUCT TEST */
                returnCode = testClient.nestedStructTest(out, returnCode)
                /** MAP TEST */
                /** MAP TEST */
                val testMapParam = (0..4).associateBy { 10 - it }
                printMap(testMapParam)
                val testMapResult: Map<Int, Int> = testClient.testMap(testMapParam)
                printMap(testMapResult)
                if (testMapParam != testMapResult) {
                    returnCode = returnCode or ERR_CONTAINERS
                    println("*** FAILURE ***\n")
                }
                /** STRING MAP TEST */
                /** STRING MAP TEST */
                returnCode = testClient.stringMapTest(returnCode)
                /** SET TEST */
                /** SET TEST */
                val setout: MutableSet<Int> = HashSet()
                for (i in -2..2) {
                    setout.add(i)
                }
                val setin: Set<Int> = testClient.testSet(setout)
                if (setout != setin) {
                    returnCode = returnCode or ERR_CONTAINERS
                    println("*** FAILURE ***\n")
                }
                /** LIST TEST */
                /** LIST TEST */
                val listout: MutableList<Int> = ArrayList()
                for (i in -2..2) {
                    listout.add(i)
                }
                val listin: List<Int> = testClient.testList(listout)
                if (listout != listin) {
                    returnCode = returnCode or ERR_CONTAINERS
                    println("*** FAILURE ***\n")
                }
                /** ENUM TEST */
                /** ENUM TEST */
                returnCode = testClient.enumTest(returnCode)
                /** TYPEDEF TEST */
                /** TYPEDEF TEST */
                returnCode = testClient.typedefTest(returnCode)
                /** NESTED MAP TEST */
                /** NESTED MAP TEST */
                returnCode = testClient.nestedMapTest(returnCode)
                /** INSANITY TEST */
                /** INSANITY TEST */
                var insanityFailed = true
                try {
                    val hello = Xtruct()
                    hello.string_thing = "Hello2"
                    hello.byte_thing = 2
                    hello.i32_thing = 2
                    hello.i64_thing = 2
                    val goodbye = Xtruct()
                    goodbye.string_thing = "Goodbye4"
                    goodbye.byte_thing = 4.toByte()
                    goodbye.i32_thing = 4
                    goodbye.i64_thing = 4L
                    insane.userMap =
                        HashMap<Numberz, Long>().apply {
                            put(Numberz.EIGHT, 8L)
                            put(Numberz.FIVE, 5L)
                        }

                    insane.xtructs =
                        ArrayList<Xtruct>().apply {
                            add(goodbye)
                            add(hello)
                        }

                    print("testInsanity()")
                    val whoa: Map<Long, Map<Numberz, Insanity>> = testClient.testInsanity(insane)
                    print(" = {")
                    for (key in whoa.keys) {
                        val `val` = whoa[key]!!
                        print("$key => {")
                        for (k2 in `val`.keys) {
                            val v2 = `val`[k2]
                            print("$k2 => {")
                            val userMap = v2!!.userMap
                            print("{")
                            if (userMap != null) {
                                for (k3 in userMap.keys) {
                                    print(k3.toString() + " => " + userMap[k3] + ", ")
                                }
                            }
                            print("}, ")
                            val xtructs = v2.xtructs
                            print("{")
                            if (xtructs != null) {
                                for ((string_thing, byte_thing, i32_thing, i64_thing) in xtructs) {
                                    print(
                                        "{\"$string_thing\", $byte_thing, $i32_thing, $i64_thing}, "
                                    )
                                }
                            }
                            print("}")
                            print("}, ")
                        }
                        print("}, ")
                    }
                    print("}\n")
                    if (whoa.size == 2 && whoa.containsKey(1L) && whoa.containsKey(2L)) {
                        val firstMap = whoa[1L]!!
                        val secondMap = whoa[2L]!!
                        if (
                            firstMap.size == 2 &&
                                firstMap.containsKey(Numberz.TWO) &&
                                firstMap.containsKey(Numberz.THREE) &&
                                secondMap.size == 1 &&
                                secondMap.containsKey(Numberz.SIX) &&
                                insane == firstMap[Numberz.TWO] &&
                                insane == firstMap[Numberz.THREE]
                        ) {
                            val six = secondMap[Numberz.SIX]!!
                            // Cannot use "new Insanity().equals(six)" because as of now,
                            // struct/container
                            // fields with default requiredness have isset=false for local instances
                            // and
                            // yet
                            // received empty values from other languages like C++ have isset=true .
                            if ((six.userMap?.size ?: 0) == 0 && (six.xtructs?.size ?: 0) == 0) {
                                // OK
                                insanityFailed = false
                            }
                        }
                    }
                } catch (ex: Exception) {
                    returnCode = returnCode or ERR_STRUCTS
                    println("*** FAILURE ***\n")
                    ex.printStackTrace(System.out)
                    insanityFailed = false
                }
                if (insanityFailed) {
                    returnCode = returnCode or ERR_STRUCTS
                    println("*** FAILURE ***\n")
                }
                /** EXECPTION TEST */
                /** EXECPTION TEST */
                val pair2 = exceptionTest(testClient, returnCode)
                returnCode = pair2.first
                testClient = pair2.second
                /** MULTI EXCEPTION TEST */
                /** MULTI EXCEPTION TEST */
                val pair3 = multiExceptionTest(testClient, returnCode)
                returnCode = pair3.first
                testClient = pair3.second
                /** ONEWAY TEST */
                /** ONEWAY TEST */
                returnCode = testClient.onewayTest(returnCode)
                val stop = System.nanoTime()
                val tot = stop - start
                println("Total time: " + tot / 1000 + "us")
                if (timeMin == 0L || tot < timeMin) {
                    timeMin = tot
                }
                if (tot > timeMax) {
                    timeMax = tot
                }
                timeTot += tot
                // transport.close()
            } catch (x: Exception) {
                System.out.printf("*** FAILURE ***\n")
                x.printStackTrace()
                returnCode = returnCode or ERR_UNKNOWN
            }
        }
        val timeAvg = timeTot / numTests
        println("Min time: " + timeMin / 1000 + "us")
        println("Max time: " + timeMax / 1000 + "us")
        println("Avg time: " + timeAvg / 1000 + "us")
        try {
            val json = TSerializer(TSimpleJSONProtocol.Factory()).toString(insane)
            println("\nSample TSimpleJSONProtocol output:\n$json")
        } catch (x: TException) {
            println("*** FAILURE ***")
            x.printStackTrace()
            returnCode = returnCode or ERR_BASETYPES
        }
        exitProcess(returnCode)
    }

    private suspend fun multiplexTest(returnCode: Int): Int {
        var code = returnCode
        if (
            protocolType == ProtocolType.Multi ||
                protocolType == ProtocolType.MultiJson ||
                protocolType == ProtocolType.MultiCompact
        ) {
            val secondClient: SecondServiceClient = createSecondServiceClient()
            print("secondtestString(\"Test2\")")
            val s = secondClient.secondtestString("Test2")
            print(" = \"$s\"\n")
            if (s != "testString(\"Test2\")") {
                code = code or ERR_PROTOCOLS
                println("*** FAILURE ***\n")
            }
        }
        return code
    }

    private suspend fun ThriftTestClient.enumTest(returnCode: Int): Int {
        var returnCode1 = returnCode
        print("testEnum(ONE)")
        var ret: Numberz = testEnum(Numberz.ONE)
        print(" = $ret\n")
        if (ret !== Numberz.ONE) {
            returnCode1 = returnCode1 or ERR_STRUCTS
            println("*** FAILURE ***\n")
        }
        print("testEnum(TWO)")
        ret = testEnum(Numberz.TWO)
        print(" = $ret\n")
        if (ret !== Numberz.TWO) {
            returnCode1 = returnCode1 or ERR_STRUCTS
            println("*** FAILURE ***\n")
        }
        print("testEnum(THREE)")
        ret = testEnum(Numberz.THREE)
        print(" = $ret\n")
        if (ret !== Numberz.THREE) {
            returnCode1 = returnCode1 or ERR_STRUCTS
            println("*** FAILURE ***\n")
        }
        print("testEnum(FIVE)")
        ret = testEnum(Numberz.FIVE)
        print(" = $ret\n")
        if (ret !== Numberz.FIVE) {
            returnCode1 = returnCode1 or ERR_STRUCTS
            println("*** FAILURE ***\n")
        }
        print("testEnum(EIGHT)")
        ret = testEnum(Numberz.EIGHT)
        print(" = $ret\n")
        if (ret !== Numberz.EIGHT) {
            returnCode1 = returnCode1 or ERR_STRUCTS
            println("*** FAILURE ***\n")
        }
        return returnCode1
    }

    private fun printMap(testMapParam: Map<Int, Int>) {
        print("testMap({")
        var first = true
        for (key in testMapParam.keys) {
            if (first) {
                first = false
            } else {
                print(", ")
            }
            print(key.toString() + " => " + testMapParam[key])
        }
        print("})")
    }

    private suspend fun exceptionTest(
        testClient: ThriftTestClient,
        returnCode: Int
    ): Pair<Int, ThriftTestClient> {
        var client = testClient
        var code = returnCode
        try {
            print("testClient.testException(\"Xception\") =>")
            client.testException("Xception")
            print("  void\n*** FAILURE ***\n")
            code = code or ERR_EXCEPTIONS
        } catch (e: Xception) {
            System.out.printf("  {%d, \"%s\"}\n", e.errorCode, e.message)
            client = createClient()
        }
        try {
            print("testClient.testException(\"TException\") =>")
            client.testException("TException")
            print("  void\n*** FAILURE ***\n")
            code = code or ERR_EXCEPTIONS
        } catch (e: TException) {
            System.out.printf("  {\"%s\"}\n", e.message)
            client = createClient()
        }
        try {
            print("testClient.testException(\"success\") =>")
            client.testException("success")
            print("  void\n")
        } catch (e: Exception) {
            System.out.printf("  exception\n*** FAILURE ***\n")
            code = code or ERR_EXCEPTIONS
        }
        return code to client
    }

    private suspend fun multiExceptionTest(
        testClient: ThriftTestClient,
        returnCode: Int
    ): Pair<Int, ThriftTestClient> {
        var client = testClient
        var code = returnCode
        try {
            System.out.printf("testClient.testMultiException(\"Xception\", \"test 1\") =>")
            client.testMultiException("Xception", "test 1")
            print("  result\n*** FAILURE ***\n")
            code = code or ERR_EXCEPTIONS
        } catch (e: Xception) {
            System.out.printf("  {%d, \"%s\"}\n", e.errorCode, e.message)
            client = createClient()
        }
        try {
            System.out.printf("testClient.testMultiException(\"Xception2\", \"test 2\") =>")
            client.testMultiException("Xception2", "test 2")
            print("  result\n*** FAILURE ***\n")
            code = code or ERR_EXCEPTIONS
        } catch (e: Xception2) {
            System.out.printf("  {%d, {\"%s\"}}\n", e.errorCode, e.struct_thing!!.string_thing)
            client = createClient()
        }
        try {
            print("testClient.testMultiException(\"success\", \"test 3\") =>")
            val result: Xtruct = client.testMultiException("success", "test 3")
            System.out.printf("  {{\"%s\"}}\n", result.string_thing)
        } catch (e: Exception) {
            System.out.printf("  exception\n*** FAILURE ***\n")
            code = code or ERR_EXCEPTIONS
        }
        return code to client
    }
}

private suspend fun ThriftTestClient.typedefTest(returnCode: Int): Int {
    var returnCode1 = returnCode
    print("testTypedef(309858235082523)")
    val uid: Long = testTypedef(309858235082523L)
    print(" = $uid\n")
    if (uid != 309858235082523L) {
        returnCode1 = returnCode1 or ERR_BASETYPES
        println("*** FAILURE ***\n")
    }
    return returnCode1
}

private suspend fun ThriftTestClient.structTest(returnCode: Int): Pair<Xtruct, Int> {
    var code = returnCode
    print("testStruct({\"Zero\", 1, -3, -5})")
    val out = Xtruct()
    out.string_thing = "Zero"
    out.byte_thing = 1.toByte()
    out.i32_thing = -3
    out.i64_thing = -5
    val input: Xtruct = testStruct(out)
    print(
        """ = {"${input.string_thing}",${input.byte_thing}, ${input.i32_thing}, ${input.i64_thing}}"""
    )
    if (input != out) {
        code = code or ERR_STRUCTS
        println("*** FAILURE ***\n")
    }
    return out to code
}

private suspend fun ThriftTestClient.onewayTest(returnCode: Int): Int {
    var returnCode1 = returnCode
    print("testOneway(3)...")
    val startOneway = System.nanoTime()
    testOneway(3)
    val onewayElapsedMillis = (System.nanoTime() - startOneway) / 1000000
    if (onewayElapsedMillis > 200) {
        println("Oneway test took too long to execute failed: took " + onewayElapsedMillis + "ms")
        println("oneway calls are 'fire and forget' and therefore should not cause blocking.")
        println("Some transports (HTTP) have a required response, and typically this failure")
        println("means the transport response was delayed until after the execution")
        println("of the RPC.  The server should post the transport response immediately and")
        println("before executing the RPC.")
        println("*** FAILURE ***")
        returnCode1 = returnCode1 or ERR_BASETYPES
    } else {
        println("Success - fire and forget only took " + onewayElapsedMillis + "ms")
    }
    return returnCode1
}

private suspend fun ThriftTestClient.nestedMapTest(returnCode: Int): Int {
    var returnCode1 = returnCode
    print("testMapMap(1)")
    val mm: Map<Int, Map<Int, Int>> = testMapMap(1)
    print(" = {")
    for (key in mm.keys) {
        print("$key => {")
        val m2 = mm[key]!!
        for (k2 in m2.keys) {
            print(k2.toString() + " => " + m2[k2] + ", ")
        }
        print("}, ")
    }
    print("}\n")
    if (mm.size != 2 || !mm.containsKey(4) || !mm.containsKey(-4)) {
        returnCode1 = returnCode1 or ERR_CONTAINERS
        println("*** FAILURE ***\n")
    } else {
        val m1 = mm[4]!!
        val m2 = mm[-4]!!
        if (
            m1[1] != 1 ||
                m1[2] != 2 ||
                m1[3] != 3 ||
                m1[4] != 4 ||
                m2[-1] != -1 ||
                m2[-2] != -2 ||
                m2[-3] != -3 ||
                m2[-4] != -4
        ) {
            returnCode1 = returnCode1 or ERR_CONTAINERS
            println("*** FAILURE ***\n")
        }
    }
    return returnCode1
}

private suspend fun ThriftTestClient.stringMapTest(returnCode: Int): Int {
    var returnCode1 = returnCode
    try {
        val smapout: MutableMap<String, String> = HashMap()
        smapout["a"] = "2"
        smapout["b"] = "blah"
        smapout["some"] = "thing"
        var first = true
        for (key in smapout.keys) {
            if (first) {
                first = false
            } else {
                print(", ")
            }
            print(key + " => " + smapout[key])
        }
        print("})")
        val smapin: Map<String, String> = testStringMap(smapout)
        print(" = {")
        first = true
        for (key in smapin.keys) {
            if (first) {
                first = false
            } else {
                print(", ")
            }
            print(key + " => " + smapout[key])
        }
        print("}\n")
        if (smapout != smapin) {
            returnCode1 = returnCode1 or ERR_CONTAINERS
            println("*** FAILURE ***\n")
        }
    } catch (ex: Exception) {
        returnCode1 = returnCode1 or ERR_CONTAINERS
        println("*** FAILURE ***\n")
        ex.printStackTrace(System.out)
    }
    return returnCode1
}

private suspend fun ThriftTestClient.nestedStructTest(out: Xtruct, returnCode: Int): Int {
    var code = returnCode
    print("testNest({1, {\"Zero\", 1, -3, -5}), 5}")
    val out2 = Xtruct2()
    out2.byte_thing = 1.toShort().toByte()
    out2.struct_thing = out
    out2.i32_thing = 5
    val xstruct2: Xtruct2 = testNest(out2)
    val input = xstruct2.struct_thing!!
    print(
        """ = {${xstruct2.byte_thing}, {"${input.string_thing}", ${input.byte_thing}, ${input.i32_thing}, ${input.i64_thing}}, ${xstruct2.i32_thing}}
    """
    )
    if (xstruct2 != out2) {
        code = code or ERR_STRUCTS
        println("*** FAILURE ***\n")
    }
    return code
}

private suspend fun ThriftTestClient.binaryTest(returnCode: Int): Int {
    var returnCode1 = returnCode
    try {
        print("testBinary(-128...127) = ")
        val data = testByteArray
        val bin: ByteBuffer = ByteBuffer.wrap(testBinary(data))
        bin.mark()
        val bytes = ByteArray(bin.limit() - bin.position())
        bin[bytes]
        bin.reset()
        print("{")
        var first = true
        for (i in bytes.indices) {
            if (first) first = false else print(", ")
            print(bytes[i])
        }
        println("}")
        if (ByteBuffer.wrap(data) != bin) {
            returnCode1 = returnCode1 or ERR_BASETYPES
            println("*** FAILURE ***\n")
        }
    } catch (ex: Exception) {
        returnCode1 = returnCode1 or ERR_BASETYPES
        println("\n*** FAILURE ***\n")
        ex.printStackTrace(System.out)
    }
    return returnCode1
}

private suspend fun ThriftTestClient.doubleTest(returnCode: Int): Int {
    var returnCode1 = returnCode
    print("testDouble(-5.325098235)")
    val dub: Double = testDouble(-5.325098235)
    print(" = $dub\n")
    if (abs(dub - -5.325098235) > 0.001) {
        returnCode1 = returnCode1 or ERR_BASETYPES
        println("*** FAILURE ***\n")
    }
    return returnCode1
}

private suspend fun ThriftTestClient.i64Test(returnCode: Int): Int {
    var returnCode1 = returnCode
    print("testI64(-34359738368)")
    val i64: Long = testI64(-34359738368L)
    print(" = $i64\n")
    if (i64 != -34359738368L) {
        returnCode1 = returnCode1 or ERR_BASETYPES
        println("*** FAILURE ***\n")
    }
    return returnCode1
}

private suspend fun ThriftTestClient.i32Test(returnCode: Int): Int {
    var returnCode1 = returnCode
    print("testI32(-1)")
    val i32: Int = testI32(-1)
    print(" = $i32\n")
    if (i32 != -1) {
        returnCode1 = returnCode1 or ERR_BASETYPES
        println("*** FAILURE ***\n")
    }
    return returnCode1
}

private suspend fun ThriftTestClient.byteTest(returnCode: Int): Int {
    var returnCode1 = returnCode
    print("testByte(1)")
    val i8: Byte = testByte(1.toByte())
    print(" = $i8\n")
    if (i8.toInt() != 1) {
        returnCode1 = returnCode1 or ERR_BASETYPES
        println("*** FAILURE ***\n")
    }
    return returnCode1
}

private suspend fun ThriftTestClient.stringTest(returnCode: Int): Int {
    var returnCode1 = returnCode
    print("testString(\"Test\")")
    val s: String = testString("Test")
    print(" = \"$s\"\n")
    if (s != "Test") {
        returnCode1 = returnCode1 or ERR_BASETYPES
        println("*** FAILURE ***\n")
    }
    return returnCode1
}

private suspend fun ThriftTestClient.voidTest(returnCode: Int): Int {
    var returnCode1 = returnCode
    try {
        print("testVoid()")
        testVoid()
        print(" = void\n")
    } catch (tax: TApplicationException) {
        tax.printStackTrace()
        returnCode1 = returnCode1 or ERR_BASETYPES
    }
    return returnCode1
}

fun main(args: Array<String>) {
    TestClient().main(args)
}

private val testByteArray = (-128..127).map { it.toByte() }.toByteArray()
