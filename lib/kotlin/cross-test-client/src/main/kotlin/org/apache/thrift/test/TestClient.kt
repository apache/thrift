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

import org.apache.http.impl.client.HttpClients
import org.apache.thrift.TApplicationException
import org.apache.thrift.TException
import org.apache.thrift.TSerializer
import org.apache.thrift.async.TAsyncClientManager
import org.apache.thrift.protocol.TBinaryProtocol
import org.apache.thrift.protocol.TCompactProtocol
import org.apache.thrift.protocol.TJSONProtocol
import org.apache.thrift.protocol.TMultiplexedProtocol
import org.apache.thrift.protocol.TProtocol
import org.apache.thrift.protocol.TSimpleJSONProtocol
import org.apache.thrift.transport.THttpClient
import org.apache.thrift.transport.TNonblockingSocket
import org.apache.thrift.transport.TSSLTransportFactory
import org.apache.thrift.transport.TTransport
import org.apache.thrift.transport.TZlibTransport
import org.apache.thrift.transport.layered.TFastFramedTransport
import org.apache.thrift.transport.layered.TFramedTransport
import thrift.test.Insanity
import thrift.test.Numberz
import thrift.test.ThriftTestClient
import thrift.test.Xception
import thrift.test.Xception2
import thrift.test.Xtruct
import thrift.test.Xtruct2
import java.nio.ByteBuffer
import kotlin.math.abs
import kotlin.system.exitProcess

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

suspend fun main(args: Array<String>) {
    var host = "localhost"
    var port = 9090
    var numTests = 1
    var protocolType = "binary"
    var transportType = "buffered"
    var ssl = false
    var zlib = false
    var httpClient = false
    var socketTimeout = 1000
    try {
        for (i in args.indices) {
            if (args[i].startsWith("--host")) {
                host = args[i].split("=").toTypedArray()[1]
                host.trim { it <= ' ' }
            } else if (args[i].startsWith("--port")) {
                port = Integer.valueOf(args[i].split("=").toTypedArray()[1])
            } else if (args[i].startsWith("--n") || args[i].startsWith("--testloops")) {
                numTests = Integer.valueOf(args[i].split("=").toTypedArray()[1])
            } else if (args[i] == "--timeout") {
                socketTimeout = Integer.valueOf(args[i].split("=").toTypedArray()[1])
            } else if (args[i].startsWith("--protocol")) {
                protocolType = args[i].split("=").toTypedArray()[1]
                protocolType.trim { it <= ' ' }
            } else if (args[i].startsWith("--transport")) {
                transportType = args[i].split("=").toTypedArray()[1]
                transportType.trim { it <= ' ' }
            } else if (args[i] == "--ssl") {
                ssl = true
            } else if (args[i] == "--zlib") {
                zlib = true
            } else if (args[i] == "--client") {
                httpClient = true
            } else if (args[i] == "--help") {
                println("Allowed options:")
                println("  --help\t\t\tProduce help message")
                println("  --host=arg (=$host)\tHost to connect")
                println("  --port=arg (=$port)\tPort number to connect")
                println(
                    "  --transport=arg (=$transportType)\n\t\t\t\tTransport: buffered, framed, fastframed, http, zlib"
                )
                println(
                    "  --protocol=arg (=$protocolType)\tProtocol: binary, compact, json, multi, multic, multij"
                )
                println("  --ssl\t\t\tEncrypted Transport using SSL")
                println("  --zlib\t\t\tCompressed Transport using Zlib")
                println("  --testloops[--n]=arg (=$numTests)\tNumber of Tests")
                exitProcess(0)
            }
        }
    } catch (x: Exception) {
        System.err.println("Can not parse arguments! See --help")
        exitProcess(ERR_UNKNOWN)
    }
    try {
        checkProtocolType(protocolType)
        checkTransportType(transportType)
        if (transportType == "http" && ssl) {
            throw Exception("SSL is not supported over http.")
        }
    } catch (e: Exception) {
        System.err.println("Error: " + e.message)
        exitProcess(ERR_UNKNOWN)
    }
    val transport: TTransport
    try {
        transport = getTTransport(transportType, host, port, httpClient, ssl, socketTimeout, zlib)
    } catch (x: Exception) {
        x.printStackTrace()
        exitProcess(ERR_UNKNOWN)
    }
    var tProtocol = getTProtocol(protocolType, transport)
    var tProtocol2: TProtocol? = null
    if (protocolType.startsWith("multi")) {
        tProtocol2 = TMultiplexedProtocol(tProtocol, "SecondService")
        tProtocol = TMultiplexedProtocol(tProtocol, "ThriftTest")
    }
    println("$tProtocol, $transport")

    val clientFactory = {
        ThriftTestClient(
            { TBinaryProtocol(it) },
            TAsyncClientManager(),
            TNonblockingSocket(host, port, socketTimeout)
        )
    }

    var testClient = clientFactory()
    val insane = Insanity()
    var timeMin: Long = 0
    var timeMax: Long = 0
    var timeTot: Long = 0
    var returnCode = 0
    for (test in 0 until numTests) {
        //        transport.startConnect()
        try {
            /** CONNECT TEST */
            println("Test #${test + 1}, connect $host:$port")
            //            if (!transport.isOpen) {
            //                try {
            //                    transport.open()
            //                } catch (ttx: TTransportException) {
            //                    ttx.printStackTrace()
            //                    println("Connect failed: " + ttx.message)
            //                    exitProcess(ERR_UNKNOWN)
            //                }
            //            }
            val start = System.nanoTime()
            /** VOID TEST */
            try {
                print("testVoid()")
                testClient.testVoid()
                print(" = void\n")
            } catch (tax: TApplicationException) {
                tax.printStackTrace()
                returnCode = returnCode or ERR_BASETYPES
            }
            /** STRING TEST */
            print("testString(\"Test\")")
            val s: String = testClient.testString("Test")
            print(" = \"$s\"\n")
            if (s != "Test") {
                returnCode = returnCode or ERR_BASETYPES
                println("*** FAILURE ***\n")
            }
            /** Multiplexed test */
            if (protocolType.startsWith("multi")) {
                throw UnsupportedOperationException("multi protocol is not yet supported")
                //                val secondClient: SecondServiceClient =
                // SecondServiceClient(tProtocol2)
                //                print("secondtestString(\"Test2\")")
                //                s = secondClient.secondtestString("Test2")
                //                print(" = \"$s\"\n")
                //                if (s != "testString(\"Test2\")") {
                //                    returnCode = returnCode or ERR_PROTOCOLS
                //                    println("*** FAILURE ***\n")
                //                }
            }
            /** BYTE TEST */
            print("testByte(1)")
            val i8: Byte = testClient.testByte(1.toByte())
            print(" = $i8\n")
            if (i8.toInt() != 1) {
                returnCode = returnCode or ERR_BASETYPES
                println("*** FAILURE ***\n")
            }
            /** I32 TEST */
            print("testI32(-1)")
            val i32: Int = testClient.testI32(-1)
            print(" = $i32\n")
            if (i32 != -1) {
                returnCode = returnCode or ERR_BASETYPES
                println("*** FAILURE ***\n")
            }
            /** I64 TEST */
            print("testI64(-34359738368)")
            val i64: Long = testClient.testI64(-34359738368L)
            print(" = $i64\n")
            if (i64 != -34359738368L) {
                returnCode = returnCode or ERR_BASETYPES
                println("*** FAILURE ***\n")
            }
            /** DOUBLE TEST */
            print("testDouble(-5.325098235)")
            val dub: Double = testClient.testDouble(-5.325098235)
            print(" = $dub\n")
            if (abs(dub - -5.325098235) > 0.001) {
                returnCode = returnCode or ERR_BASETYPES
                println("*** FAILURE ***\n")
            }
            /** BINARY TEST */
            try {
                print("testBinary(-128...127) = ")
                val data =
                    byteArrayOf(
                        -128,
                        -127,
                        -126,
                        -125,
                        -124,
                        -123,
                        -122,
                        -121,
                        -120,
                        -119,
                        -118,
                        -117,
                        -116,
                        -115,
                        -114,
                        -113,
                        -112,
                        -111,
                        -110,
                        -109,
                        -108,
                        -107,
                        -106,
                        -105,
                        -104,
                        -103,
                        -102,
                        -101,
                        -100,
                        -99,
                        -98,
                        -97,
                        -96,
                        -95,
                        -94,
                        -93,
                        -92,
                        -91,
                        -90,
                        -89,
                        -88,
                        -87,
                        -86,
                        -85,
                        -84,
                        -83,
                        -82,
                        -81,
                        -80,
                        -79,
                        -78,
                        -77,
                        -76,
                        -75,
                        -74,
                        -73,
                        -72,
                        -71,
                        -70,
                        -69,
                        -68,
                        -67,
                        -66,
                        -65,
                        -64,
                        -63,
                        -62,
                        -61,
                        -60,
                        -59,
                        -58,
                        -57,
                        -56,
                        -55,
                        -54,
                        -53,
                        -52,
                        -51,
                        -50,
                        -49,
                        -48,
                        -47,
                        -46,
                        -45,
                        -44,
                        -43,
                        -42,
                        -41,
                        -40,
                        -39,
                        -38,
                        -37,
                        -36,
                        -35,
                        -34,
                        -33,
                        -32,
                        -31,
                        -30,
                        -29,
                        -28,
                        -27,
                        -26,
                        -25,
                        -24,
                        -23,
                        -22,
                        -21,
                        -20,
                        -19,
                        -18,
                        -17,
                        -16,
                        -15,
                        -14,
                        -13,
                        -12,
                        -11,
                        -10,
                        -9,
                        -8,
                        -7,
                        -6,
                        -5,
                        -4,
                        -3,
                        -2,
                        -1,
                        0,
                        1,
                        2,
                        3,
                        4,
                        5,
                        6,
                        7,
                        8,
                        9,
                        10,
                        11,
                        12,
                        13,
                        14,
                        15,
                        16,
                        17,
                        18,
                        19,
                        20,
                        21,
                        22,
                        23,
                        24,
                        25,
                        26,
                        27,
                        28,
                        29,
                        30,
                        31,
                        32,
                        33,
                        34,
                        35,
                        36,
                        37,
                        38,
                        39,
                        40,
                        41,
                        42,
                        43,
                        44,
                        45,
                        46,
                        47,
                        48,
                        49,
                        50,
                        51,
                        52,
                        53,
                        54,
                        55,
                        56,
                        57,
                        58,
                        59,
                        60,
                        61,
                        62,
                        63,
                        64,
                        65,
                        66,
                        67,
                        68,
                        69,
                        70,
                        71,
                        72,
                        73,
                        74,
                        75,
                        76,
                        77,
                        78,
                        79,
                        80,
                        81,
                        82,
                        83,
                        84,
                        85,
                        86,
                        87,
                        88,
                        89,
                        90,
                        91,
                        92,
                        93,
                        94,
                        95,
                        96,
                        97,
                        98,
                        99,
                        100,
                        101,
                        102,
                        103,
                        104,
                        105,
                        106,
                        107,
                        108,
                        109,
                        110,
                        111,
                        112,
                        113,
                        114,
                        115,
                        116,
                        117,
                        118,
                        119,
                        120,
                        121,
                        122,
                        123,
                        124,
                        125,
                        126,
                        127
                    )
                val bin: ByteBuffer = ByteBuffer.wrap(testClient.testBinary(data))
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
                    returnCode = returnCode or ERR_BASETYPES
                    println("*** FAILURE ***\n")
                }
            } catch (ex: Exception) {
                returnCode = returnCode or ERR_BASETYPES
                println("\n*** FAILURE ***\n")
                ex.printStackTrace(System.out)
            }
            /** STRUCT TEST */
            print("testStruct({\"Zero\", 1, -3, -5})")
            val out = Xtruct()
            out.string_thing = "Zero"
            out.byte_thing = 1.toByte()
            out.i32_thing = -3
            out.i64_thing = -5
            var `in`: Xtruct = testClient.testStruct(out)
            print(
                """ = {"${`in`.string_thing}",${`in`.byte_thing}, ${`in`.i32_thing}, ${`in`.i64_thing}}
"""
            )
            if (`in` != out) {
                returnCode = returnCode or ERR_STRUCTS
                println("*** FAILURE ***\n")
            }
            /** NESTED STRUCT TEST */
            print("testNest({1, {\"Zero\", 1, -3, -5}), 5}")
            val out2 = Xtruct2()
            out2.byte_thing = 1.toShort().toByte()
            out2.struct_thing = out
            out2.i32_thing = 5
            val in2: Xtruct2 = testClient.testNest(out2)
            `in` = in2.struct_thing!!
            print(
                """ = {${in2.byte_thing}, {"${`in`.string_thing}", ${`in`.byte_thing}, ${`in`.i32_thing}, ${`in`.i64_thing}}, ${in2.i32_thing}}
"""
            )
            if (in2 != out2) {
                returnCode = returnCode or ERR_STRUCTS
                println("*** FAILURE ***\n")
            }
            /** MAP TEST */
            val mapout: MutableMap<Int, Int> = HashMap()
            for (i in 0..4) {
                mapout[i] = i - 10
            }
            print("testMap({")
            var first = true
            for (key in mapout.keys) {
                if (first) {
                    first = false
                } else {
                    print(", ")
                }
                print(key.toString() + " => " + mapout[key])
            }
            print("})")
            val mapin: Map<Int, Int> = testClient.testMap(mapout)
            print(" = {")
            first = true
            for (key in mapin.keys) {
                if (first) {
                    first = false
                } else {
                    print(", ")
                }
                print(key.toString() + " => " + mapout[key])
            }
            print("}\n")
            if (mapout != mapin) {
                returnCode = returnCode or ERR_CONTAINERS
                println("*** FAILURE ***\n")
            }
            /** STRING MAP TEST */
            try {
                val smapout: MutableMap<String, String> = HashMap()
                smapout["a"] = "2"
                smapout["b"] = "blah"
                smapout["some"] = "thing"
                for (key in smapout.keys) {
                    if (first) {
                        first = false
                    } else {
                        print(", ")
                    }
                    print(key + " => " + smapout[key])
                }
                print("})")
                val smapin: Map<String, String> = testClient.testStringMap(smapout)
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
                    returnCode = returnCode or ERR_CONTAINERS
                    println("*** FAILURE ***\n")
                }
            } catch (ex: Exception) {
                returnCode = returnCode or ERR_CONTAINERS
                println("*** FAILURE ***\n")
                ex.printStackTrace(System.out)
            }
            /** SET TEST */
            val setout: MutableSet<Int> = HashSet()
            for (i in -2..2) {
                setout.add(i)
            }
            print("testSet({")
            first = true
            for (elem in setout) {
                if (first) {
                    first = false
                } else {
                    print(", ")
                }
                print(elem)
            }
            print("})")
            val setin: Set<Int> = testClient.testSet(setout)
            print(" = {")
            first = true
            for (elem in setin) {
                if (first) {
                    first = false
                } else {
                    print(", ")
                }
                print(elem)
            }
            print("}\n")
            if (setout != setin) {
                returnCode = returnCode or ERR_CONTAINERS
                println("*** FAILURE ***\n")
            }
            /** LIST TEST */
            val listout: MutableList<Int> = ArrayList()
            for (i in -2..2) {
                listout.add(i)
            }
            print("testList({")
            first = true
            for (elem in listout) {
                if (first) {
                    first = false
                } else {
                    print(", ")
                }
                print(elem)
            }
            print("})")
            val listin: List<Int> = testClient.testList(listout)
            print(" = {")
            first = true
            for (elem in listin) {
                if (first) {
                    first = false
                } else {
                    print(", ")
                }
                print(elem)
            }
            print("}\n")
            if (listout != listin) {
                returnCode = returnCode or ERR_CONTAINERS
                println("*** FAILURE ***\n")
            }
            /** ENUM TEST */
            print("testEnum(ONE)")
            var ret: Numberz = testClient.testEnum(Numberz.ONE)
            print(" = $ret\n")
            if (ret !== Numberz.ONE) {
                returnCode = returnCode or ERR_STRUCTS
                println("*** FAILURE ***\n")
            }
            print("testEnum(TWO)")
            ret = testClient.testEnum(Numberz.TWO)
            print(" = $ret\n")
            if (ret !== Numberz.TWO) {
                returnCode = returnCode or ERR_STRUCTS
                println("*** FAILURE ***\n")
            }
            print("testEnum(THREE)")
            ret = testClient.testEnum(Numberz.THREE)
            print(" = $ret\n")
            if (ret !== Numberz.THREE) {
                returnCode = returnCode or ERR_STRUCTS
                println("*** FAILURE ***\n")
            }
            print("testEnum(FIVE)")
            ret = testClient.testEnum(Numberz.FIVE)
            print(" = $ret\n")
            if (ret !== Numberz.FIVE) {
                returnCode = returnCode or ERR_STRUCTS
                println("*** FAILURE ***\n")
            }
            print("testEnum(EIGHT)")
            ret = testClient.testEnum(Numberz.EIGHT)
            print(" = $ret\n")
            if (ret !== Numberz.EIGHT) {
                returnCode = returnCode or ERR_STRUCTS
                println("*** FAILURE ***\n")
            }
            /** TYPEDEF TEST */
            print("testTypedef(309858235082523)")
            val uid: Long = testClient.testTypedef(309858235082523L)
            print(" = $uid\n")
            if (uid != 309858235082523L) {
                returnCode = returnCode or ERR_BASETYPES
                println("*** FAILURE ***\n")
            }
            /** NESTED MAP TEST */
            print("testMapMap(1)")
            val mm: Map<Int, Map<Int, Int>> = testClient.testMapMap(1)
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
                returnCode = returnCode or ERR_CONTAINERS
                println("*** FAILURE ***\n")
            } else {
                val m1 = mm[4]!!
                val m2 = mm[-4]!!
                if (m1[1] != 1 ||
                    m1[2] != 2 ||
                    m1[3] != 3 ||
                    m1[4] != 4 ||
                    m2[-1] != -1 ||
                    m2[-2] != -2 ||
                    m2[-3] != -3 ||
                    m2[-4] != -4
                ) {
                    returnCode = returnCode or ERR_CONTAINERS
                    println("*** FAILURE ***\n")
                }
            }
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
                                print("{\"$string_thing\", $byte_thing, $i32_thing, $i64_thing}, ")
                            }
                        }
                        print("}")
                        print("}, ")
                    }
                    print("}, ")
                }
                print("}\n")
                if (whoa.size == 2 && whoa.containsKey(1L) && whoa.containsKey(2L)) {
                    val first_map = whoa[1L]!!
                    val second_map = whoa[2L]!!
                    if (first_map.size == 2 &&
                        first_map.containsKey(Numberz.TWO) &&
                        first_map.containsKey(Numberz.THREE) &&
                        second_map.size == 1 &&
                        second_map.containsKey(Numberz.SIX) &&
                        insane == first_map[Numberz.TWO] &&
                        insane == first_map[Numberz.THREE]
                    ) {
                        val six = second_map[Numberz.SIX]!!
                        // Cannot use "new Insanity().equals(six)" because as of now,
                        // struct/container
                        // fields with default requiredness have isset=false for local instances and
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
            try {
                print("testClient.testException(\"Xception\") =>")
                testClient.testException("Xception")
                print("  void\n*** FAILURE ***\n")
                returnCode = returnCode or ERR_EXCEPTIONS
            } catch (e: Xception) {
                System.out.printf("  {%d, \"%s\"}\n", e.errorCode, e.message)
                testClient = clientFactory()
            }
            try {
                print("testClient.testException(\"TException\") =>")
                testClient.testException("TException")
                print("  void\n*** FAILURE ***\n")
                returnCode = returnCode or ERR_EXCEPTIONS
            } catch (e: TException) {
                System.out.printf("  {\"%s\"}\n", e.message)
                testClient = clientFactory()
            }
            try {
                print("testClient.testException(\"success\") =>")
                testClient.testException("success")
                print("  void\n")
            } catch (e: Exception) {
                System.out.printf("  exception\n*** FAILURE ***\n")
                returnCode = returnCode or ERR_EXCEPTIONS
            }
            /** MULTI EXCEPTION TEST */
            try {
                System.out.printf("testClient.testMultiException(\"Xception\", \"test 1\") =>")
                testClient.testMultiException("Xception", "test 1")
                print("  result\n*** FAILURE ***\n")
                returnCode = returnCode or ERR_EXCEPTIONS
            } catch (e: Xception) {
                System.out.printf("  {%d, \"%s\"}\n", e.errorCode, e.message)
                testClient = clientFactory()
            }
            try {
                System.out.printf("testClient.testMultiException(\"Xception2\", \"test 2\") =>")
                testClient.testMultiException("Xception2", "test 2")
                print("  result\n*** FAILURE ***\n")
                returnCode = returnCode or ERR_EXCEPTIONS
            } catch (e: Xception2) {
                System.out.printf("  {%d, {\"%s\"}}\n", e.errorCode, e.struct_thing!!.string_thing)
                testClient = clientFactory()
            }
            try {
                print("testClient.testMultiException(\"success\", \"test 3\") =>")
                val result: Xtruct = testClient.testMultiException("success", "test 3")
                System.out.printf("  {{\"%s\"}}\n", result.string_thing)
            } catch (e: Exception) {
                System.out.printf("  exception\n*** FAILURE ***\n")
                returnCode = returnCode or ERR_EXCEPTIONS
            }
            /** ONEWAY TEST */
            print("testOneway(3)...")
            val startOneway = System.nanoTime()
            testClient.testOneway(3)
            val onewayElapsedMillis = (System.nanoTime() - startOneway) / 1000000
            if (onewayElapsedMillis > 200) {
                println(
                    "Oneway test took too long to execute failed: took " +
                            onewayElapsedMillis +
                            "ms"
                )
                println(
                    "oneway calls are 'fire and forget' and therefore should not cause blocking."
                )
                println(
                    "Some transports (HTTP) have a required response, and typically this failure"
                )
                println("means the transport response was delayed until after the execution")
                println(
                    "of the RPC.  The server should post the transport response immediately and"
                )
                println("before executing the RPC.")
                println("*** FAILURE ***")
                returnCode = returnCode or ERR_BASETYPES
            } else {
                println("Success - fire and forget only took " + onewayElapsedMillis + "ms")
            }
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
            //            transport.close()
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

private fun getTProtocol(protocol_type: String, transport: TTransport) =
    when (protocol_type) {
        "json", "multij" -> {
            TJSONProtocol(transport)
        }
        "compact", "multic" -> {
            TCompactProtocol(transport)
        }
        else -> {
            TBinaryProtocol(transport)
        }
    }

private fun checkTransportType(transport_type: String) {
    when (transport_type) {
        "buffered" -> {}
        "framed" -> {}
        "fastframed" -> {}
        "http" -> {}
        "zlib" -> {}
        else -> {
            throw Exception("Unknown transport type! $transport_type")
        }
    }
}

private fun checkProtocolType(protocol_type: String) {
    when (protocol_type) {
        "binary" -> {}
        "compact" -> {}
        "json" -> {}
        "multi" -> {}
        "multic" -> {}
        "multij" -> {}
        else -> {
            throw Exception("Unknown protocol type! $protocol_type")
        }
    }
}

private fun getTTransport(
    transport_type: String,
    host: String,
    port: Int,
    http_client: Boolean,
    ssl: Boolean,
    socketTimeout: Int,
    zlib: Boolean
): TTransport {
    when (transport_type) {
        "http" -> {
            val url = "http://$host:$port/test/service"
            return if (http_client) {
                THttpClient(url, HttpClients.createDefault())
            } else {
                THttpClient(url)
            }
        }
        else -> {
            val socket = if (ssl) {
                TSSLTransportFactory.getClientSocket(host, port, socketTimeout)
            } else {
                println("using non-blocking socket $host:$port")
                TNonblockingSocket(host, port, socketTimeout)
            }
            if (transport_type == "zlib") {
                return TZlibTransport(socket)
            } else {
                val wrapped = when (transport_type) {
                    "buffered" -> {
                        socket
                    }
                    "framed" -> {
                        TFramedTransport(socket)
                    }
                    "fastframed" -> {
                        TFastFramedTransport(socket)
                    }
                    else -> {
                        socket
                    }
                }
                return if (zlib) {
                    TZlibTransport(wrapped)
                } else {
                    wrapped
                }
            }
        }
    }
}
