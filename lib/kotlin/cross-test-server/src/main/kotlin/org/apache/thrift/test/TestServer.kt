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

import kotlin.system.exitProcess
import kotlinx.coroutines.GlobalScope
import org.apache.thrift.TException
import org.apache.thrift.TMultiplexedProcessor
import org.apache.thrift.protocol.TBinaryProtocol
import org.apache.thrift.protocol.TCompactProtocol
import org.apache.thrift.protocol.TJSONProtocol
import org.apache.thrift.protocol.TProtocol
import org.apache.thrift.protocol.TProtocolFactory
import org.apache.thrift.server.ServerContext
import org.apache.thrift.server.TNonblockingServer
import org.apache.thrift.server.TServer
import org.apache.thrift.server.TServerEventHandler
import org.apache.thrift.server.TSimpleServer
import org.apache.thrift.server.TThreadPoolServer
import org.apache.thrift.server.TThreadedSelectorServer
import org.apache.thrift.transport.TNonblockingServerSocket
import org.apache.thrift.transport.TNonblockingServerSocket.NonblockingAbstractServerSocketArgs
import org.apache.thrift.transport.TSSLTransportFactory
import org.apache.thrift.transport.TServerSocket
import org.apache.thrift.transport.TServerSocket.ServerSocketTransportArgs
import org.apache.thrift.transport.TTransport
import org.apache.thrift.transport.TTransportFactory
import org.apache.thrift.transport.TZlibTransport
import org.apache.thrift.transport.layered.TFastFramedTransport
import org.apache.thrift.transport.layered.TFramedTransport
import thrift.test.SecondService
import thrift.test.SecondServiceProcessor
import thrift.test.ThriftTestProcessor

object TestServer {

    // Multiplexed Protocol Support Details:
    //
    // For multiplexed testing we always use binary protocol underneath.
    //
    // "ThriftTest" named service implements "ThriftTest" from ThriftTest.thrift
    // "SecondService" named service implements "SecondService" from ThriftTest.thrift
    // In addition, to support older non-multiplexed clients using the same concrete protocol
    // the multiplexed processor is taught to use "ThriftTest" if the incoming request has no
    // multiplexed call name decoration.
    internal class SecondHandler : SecondService {
        @Throws(TException::class)
        override suspend fun secondtestString(thing: String): String {
            return "testString(\"$thing\")"
        }
    }

    internal class TestServerContext(var connectionId: Int) : ServerContext {

        override fun <T> unwrap(iface: Class<T>): T {
            try {
                return if (isWrapperFor(iface)) {
                    iface.cast(this)
                } else {
                    throw RuntimeException("The context is not a wrapper for " + iface.name)
                }
            } catch (e: Exception) {
                throw RuntimeException(
                    "The context is not a wrapper and does not implement the interface"
                )
            }
        }

        override fun isWrapperFor(iface: Class<*>): Boolean {
            return iface.isInstance(this)
        }
    }

    internal class TestServerEventHandler() : TServerEventHandler {
        private var nextConnectionId = 1
        override fun preServe() {
            println(
                "TServerEventHandler.preServe - called only once before server starts accepting connections"
            )
        }

        override fun createContext(input: TProtocol, output: TProtocol): ServerContext {
            // we can create some connection level data which is stored while connection is alive &
            // served
            val ctx = TestServerContext(nextConnectionId++)
            println(
                "TServerEventHandler.createContext - connection #" +
                    ctx.connectionId +
                    " established"
            )
            return ctx
        }

        override fun deleteContext(
            serverContext: ServerContext,
            input: TProtocol,
            output: TProtocol
        ) {
            val ctx = serverContext.unwrap(TestServerContext::class.java)
            println(
                "TServerEventHandler.deleteContext - connection #" +
                    ctx.connectionId +
                    " terminated"
            )
        }

        override fun processContext(
            serverContext: ServerContext,
            inputTransport: TTransport,
            outputTransport: TTransport
        ) {
            val ctx = serverContext.unwrap(TestServerContext::class.java)
            println(
                "TServerEventHandler.processContext - connection #" +
                    ctx.connectionId +
                    " is ready to process next request"
            )
        }
    }
}

fun main(args: Array<String>) {
    try {
        var port = 9090
        var ssl = false
        var zlib = false
        var transportType = "buffered"
        var protocolType = "binary"
//        var serverType = "thread-pool"
        var serverType = "nonblocking"
        val domainSocket = ""
        var stringLimit: Long = -1
        var containerLimit: Long = -1
        try {
            for (i in args.indices) {
                if (args[i].startsWith("--port")) {
                    port = Integer.valueOf(args[i].split("=").toTypedArray()[1])
                } else if (args[i].startsWith("--server-type")) {
                    serverType = args[i].split("=").toTypedArray()[1]
                    serverType.trim { it <= ' ' }
                } else if (args[i].startsWith("--port")) {
                    port = args[i].split("=").toTypedArray()[1].toInt()
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
                } else if (args[i].startsWith("--string-limit")) {
                    stringLimit = args[i].split("=").toTypedArray()[1].toLong()
                } else if (args[i].startsWith("--container-limit")) {
                    containerLimit = args[i].split("=").toTypedArray()[1].toLong()
                } else if (args[i] == "--help") {
                    println("Allowed options:")
                    println("  --help\t\t\tProduce help message")
                    println("  --port=arg (=$port)\tPort number to connect")
                    println(
                        "  --transport=arg (=$transportType)\n\t\t\t\tTransport: buffered, framed, fastframed, zlib"
                    )
                    println(
                        "  --protocol=arg (=$protocolType)\tProtocol: binary, compact, json, multi, multic, multij"
                    )
                    println("  --ssl\t\t\tEncrypted Transport using SSL")
                    println("  --zlib\t\t\tCompressed Transport using Zlib")
                    println(
                        "  --server-type=arg (=$serverType)\n\t\t\t\tType of server: simple, thread-pool, nonblocking, threaded-selector"
                    )
                    println("  --string-limit=arg (=$stringLimit)\tString read length limit")
                    println(
                        "  --container-limit=arg (=$containerLimit)\tContainer read length limit"
                    )
                    exitProcess(0)
                }
            }
        } catch (e: Exception) {
            System.err.println("Can not parse arguments! See --help")
            exitProcess(1)
        }
        try {
            when (serverType) {
                "simple" -> {}
                "thread-pool" -> {}
                "nonblocking" -> {
                    if (ssl) {
                        throw Exception("SSL is not supported over nonblocking servers!")
                    }
                }
                "threaded-selector" -> {
                    if (ssl) {
                        throw Exception("SSL is not supported over nonblocking servers!")
                    }
                }
                else -> {
                    throw Exception("Unknown server type! $serverType")
                }
            }
            when (protocolType) {
                "binary" -> {}
                "compact" -> {}
                "json" -> {}
                "multi" -> {}
                "multic" -> {}
                "multij" -> {}
                else -> {
                    throw Exception("Unknown protocol type! $protocolType")
                }
            }
            when (transportType) {
                "buffered" -> {}
                "framed" -> {}
                "fastframed" -> {}
                "zlib" -> {}
                else -> {
                    throw Exception("Unknown transport type! $transportType")
                }
            }
        } catch (e: Exception) {
            System.err.println("Error: " + e.message)
            exitProcess(1)
        }

        // Processors
        val testHandler = TestHandler()
        val testProcessor = ThriftTestProcessor(testHandler, scope = GlobalScope)
        val secondHandler = TestServer.SecondHandler()
        val secondProcessor = SecondServiceProcessor(secondHandler, scope = GlobalScope)

        // Protocol factory
        val tProtocolFactory: TProtocolFactory =
            when (protocolType) {
                "json", "multij" -> {
                    TJSONProtocol.Factory()
                }
                "compact", "multic" -> {
                    TCompactProtocol.Factory(stringLimit, containerLimit)
                }
                else -> { // also covers multi
                    TBinaryProtocol.Factory(stringLimit, containerLimit)
                }
            }
        val tTransportFactory: TTransportFactory =
            when (transportType) {
                "framed" -> {
                    TFramedTransport.Factory()
                }
                "fastframed" -> {
                    TFastFramedTransport.Factory()
                }
                "zlib" -> {
                    TZlibTransport.Factory()
                }
                else -> { // .equals("buffered") => default value
                    TTransportFactory()
                }
            }
        val serverEngine: TServer
        // If we are multiplexing services in one server...
        val multiplexedProcessor = TMultiplexedProcessor()
        multiplexedProcessor.registerDefault(testProcessor)
        multiplexedProcessor.registerProcessor("ThriftTest", testProcessor)
        multiplexedProcessor.registerProcessor("SecondService", secondProcessor)
        if (serverType == "nonblocking" || serverType == "threaded-selector") {
            // Nonblocking servers
            val tNonblockingServerSocket =
                TNonblockingServerSocket(NonblockingAbstractServerSocketArgs().port(port))
            if (serverType.contains("nonblocking")) {
                // Nonblocking Server
                val tNonblockingServerArgs = TNonblockingServer.Args(tNonblockingServerSocket)
                tNonblockingServerArgs.processor(
                    if (protocolType.startsWith("multi")) multiplexedProcessor else testProcessor
                )
                tNonblockingServerArgs.protocolFactory(tProtocolFactory)
                tNonblockingServerArgs.transportFactory(tTransportFactory)
                serverEngine = TNonblockingServer(tNonblockingServerArgs)
            } else { // server_type.equals("threaded-selector")
                // ThreadedSelector Server
                val tThreadedSelectorServerArgs =
                    TThreadedSelectorServer.Args(tNonblockingServerSocket)
                tThreadedSelectorServerArgs.processor(
                    if (protocolType.startsWith("multi")) multiplexedProcessor else testProcessor
                )
                tThreadedSelectorServerArgs.protocolFactory(tProtocolFactory)
                tThreadedSelectorServerArgs.transportFactory(tTransportFactory)
                serverEngine = TThreadedSelectorServer(tThreadedSelectorServerArgs)
            }
        } else {
            // Blocking servers

            // SSL socket
            val tServerSocket: TServerSocket = if (ssl) {
                TSSLTransportFactory.getServerSocket(port, 0)
            } else {
                TServerSocket(ServerSocketTransportArgs().port(port))
            }
            if (serverType == "simple") {
                // Simple Server
                val tServerArgs = TServer.Args(tServerSocket)
                tServerArgs.processor(
                    if (protocolType.startsWith("multi")) multiplexedProcessor else testProcessor
                )
                tServerArgs.protocolFactory(tProtocolFactory)
                tServerArgs.transportFactory(tTransportFactory)
                serverEngine = TSimpleServer(tServerArgs)
            } else { // server_type.equals("threadpool")
                // ThreadPool Server
                val tThreadPoolServerArgs = TThreadPoolServer.Args(tServerSocket)
                tThreadPoolServerArgs.processor(
                    if (protocolType.startsWith("multi")) multiplexedProcessor else testProcessor
                )
                tThreadPoolServerArgs.protocolFactory(tProtocolFactory)
                tThreadPoolServerArgs.transportFactory(tTransportFactory)
                serverEngine = TThreadPoolServer(tThreadPoolServerArgs)
            }
        }

        // Set server event handler
        serverEngine.setServerEventHandler(TestServer.TestServerEventHandler())

        // Run it
        println(
            "Starting the " +
                (if (ssl) "ssl server" else "server") +
                " [" +
                protocolType +
                "/" +
                transportType +
                "/" +
                serverType +
                "] on " +
                if (domainSocket === "") "port $port" else "unix socket $domainSocket"
        )
        serverEngine.serve()
    } catch (x: Exception) {
        x.printStackTrace()
    }
    println("done.")
}
