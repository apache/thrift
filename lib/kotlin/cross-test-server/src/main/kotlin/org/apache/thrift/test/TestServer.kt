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
import com.github.ajalt.clikt.parameters.types.long
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

enum class ServerType(val key: String) {
    Simple("simple"),
    ThreadPool("thread-pool"),
    NonBlocking("nonblocking"),
    ThreadedSelector("threaded-selector")
}

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
    FastFramed("fastframed"),
    Framed("framed"),
    Zlib("zlib")
}

class TestServerCommand : CliktCommand() {
    private val port: Int by option(help = "The cross test port to connect to").int().default(9090)
    private val protocolType: ProtocolType by
        option("--protocol", help = "Protocol type")
            .enum<ProtocolType> { it.key }
            .default(ProtocolType.Binary)
    private val transportType: TransportType by
        option("--transport", help = "Transport type")
            .enum<TransportType> { it.key }
            .default(TransportType.Buffered)
    private val serverType: ServerType by
        option("--server-type").enum<ServerType> { it.key }.default(ServerType.NonBlocking)
    private val useSSL: Boolean by
        option("--ssl", help = "Use SSL for encrypted transport").flag(default = false)
    private val stringLimit: Long by option("--string-limit").long().default(-1)
    private val containerLimit: Long by option("--container-limit").long().default(-1)

    @Suppress("OPT_IN_USAGE")
    override fun run() {
        val testHandler = TestHandler()
        val testProcessor = ThriftTestProcessor(testHandler, scope = GlobalScope)
        val secondHandler = TestServer.SecondHandler()
        val secondProcessor = SecondServiceProcessor(secondHandler, scope = GlobalScope)
        val serverEngine: TServer =
            getServerEngine(
                testProcessor,
                secondProcessor,
                serverType,
                port,
                protocolType,
                getProtocolFactory(),
                getTransportFactory(),
                useSSL
            )
        // Set server event handler
        serverEngine.setServerEventHandler(TestServer.TestServerEventHandler())
        // Run it
        println(
            "Starting the ${if (useSSL) "ssl server" else "server"} [$protocolType/$transportType/$serverType] on port $port"
        )
        serverEngine.serve()
    }

    private fun getTransportFactory(): TTransportFactory =
        when (transportType) {
            TransportType.Framed -> {
                TFramedTransport.Factory()
            }
            TransportType.FastFramed -> {
                TFastFramedTransport.Factory()
            }
            TransportType.Zlib -> {
                TZlibTransport.Factory()
            }
            TransportType.Buffered -> {
                TTransportFactory()
            }
        }

    private fun getProtocolFactory(): TProtocolFactory =
        when (protocolType) {
            ProtocolType.Json,
            ProtocolType.MultiJson -> TJSONProtocol.Factory()
            ProtocolType.Compact,
            ProtocolType.MultiCompact -> TCompactProtocol.Factory(stringLimit, containerLimit)
            ProtocolType.Binary,
            ProtocolType.Multi -> TBinaryProtocol.Factory(stringLimit, containerLimit)
        }
}

fun main(args: Array<String>) {
    TestServerCommand().main(args)
}

private fun getServerEngine(
    testProcessor: ThriftTestProcessor,
    secondProcessor: SecondServiceProcessor,
    serverType: ServerType,
    port: Int,
    protocolType: ProtocolType,
    tProtocolFactory: TProtocolFactory,
    tTransportFactory: TTransportFactory,
    ssl: Boolean
): TServer {
    val isMulti =
        protocolType == ProtocolType.Multi ||
            protocolType == ProtocolType.MultiCompact ||
            protocolType == ProtocolType.MultiJson
    // If we are multiplexing services in one server...
    val multiplexedProcessor = TMultiplexedProcessor()
    multiplexedProcessor.registerDefault(testProcessor)
    multiplexedProcessor.registerProcessor("ThriftTest", testProcessor)
    multiplexedProcessor.registerProcessor("SecondService", secondProcessor)
    when (serverType) {
        ServerType.NonBlocking,
        ServerType.ThreadedSelector -> {
            val tNonblockingServerSocket =
                TNonblockingServerSocket(NonblockingAbstractServerSocketArgs().port(port))
            when (serverType) {
                ServerType.NonBlocking -> {
                    val tNonblockingServerArgs = TNonblockingServer.Args(tNonblockingServerSocket)
                    tNonblockingServerArgs.processor(
                        if (isMulti) multiplexedProcessor else testProcessor
                    )
                    tNonblockingServerArgs.protocolFactory(tProtocolFactory)
                    tNonblockingServerArgs.transportFactory(tTransportFactory)
                    return TNonblockingServer(tNonblockingServerArgs)
                }
                else -> {
                    val tThreadedSelectorServerArgs =
                        TThreadedSelectorServer.Args(tNonblockingServerSocket)
                    tThreadedSelectorServerArgs.processor(
                        if (isMulti) multiplexedProcessor else testProcessor
                    )
                    tThreadedSelectorServerArgs.protocolFactory(tProtocolFactory)
                    tThreadedSelectorServerArgs.transportFactory(tTransportFactory)
                    return TThreadedSelectorServer(tThreadedSelectorServerArgs)
                }
            }
        }
        ServerType.Simple,
        ServerType.ThreadPool -> {
            // SSL socket
            val tServerSocket: TServerSocket =
                if (ssl) {
                    TSSLTransportFactory.getServerSocket(port, 0)
                } else {
                    TServerSocket(ServerSocketTransportArgs().port(port))
                }
            when (serverType) {
                ServerType.Simple -> {
                    val tServerArgs = TServer.Args(tServerSocket)
                    tServerArgs.processor(if (isMulti) multiplexedProcessor else testProcessor)
                    tServerArgs.protocolFactory(tProtocolFactory)
                    tServerArgs.transportFactory(tTransportFactory)
                    return TSimpleServer(tServerArgs)
                }
                else -> {
                    val tThreadPoolServerArgs = TThreadPoolServer.Args(tServerSocket)
                    tThreadPoolServerArgs.processor(
                        if (isMulti) multiplexedProcessor else testProcessor
                    )
                    tThreadPoolServerArgs.protocolFactory(tProtocolFactory)
                    tThreadPoolServerArgs.transportFactory(tTransportFactory)
                    return TThreadPoolServer(tThreadPoolServerArgs)
                }
            }
        }
    }
}
