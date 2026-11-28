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
module async_client_test;

import core.time : Duration, dur;
import std.exception : enforce;
import thrift.async.base;
import thrift.base;
import thrift.codegen.async_client;
import thrift.codegen.base;
import thrift.codegen.processor;
import thrift.protocol.binary;
import thrift.protocol.processor;
import thrift.transport.base;
import thrift.transport.memory;
import thrift.util.cancellation;
import thrift.util.future;

struct Request {
  int a;
  int b;
  string op;
  mixin TStructHelpers!();
}

interface Calculator {
  // Struct parameters are declared as ref const by the D generator.
  string calculate(ref const(Request) request);
  string label(string value, int count);
}

class CalculatorHandler : Calculator {
  override string calculate(ref const(Request) request) {
    import std.conv : text;
    return text(request.a, request.op, request.b);
  }
  override string label(string value, int count) {
    import std.conv : text;
    return text(value, count);
  }
}

/// Keeps the work items until the test runs them, like a busy event loop.
class QueueingManager : TAsyncManager {
  override void execute(TAsyncTransport transport, void delegate() work,
    TCancellation cancellation = null
  ) {
    queue ~= work;
  }
  override void delay(Duration duration, void delegate() work) {
    queue ~= work;
  }
  override bool stop(Duration waitFinishTimeout = dur!"hnsecs"(-1)) {
    return true;
  }

  void runAll() {
    auto items = queue;
    queue = null;
    foreach (w; items) w();
  }

  void delegate()[] queue;
}

/// Answers each flushed request by running it through the server processor.
class LoopbackTransport : TBaseTransport, TAsyncTransport {
  this(TAsyncManager manager, TProcessor processor) {
    manager_ = manager;
    processor_ = processor;
    request_ = new TMemoryBuffer;
    reply_ = new TMemoryBuffer;
  }

  override TAsyncManager asyncManager() @property {
    return manager_;
  }

  override bool isOpen() @property {
    return true;
  }

  override size_t read(ubyte[] buf) {
    return reply_.read(buf);
  }

  override void write(in ubyte[] buf) {
    request_.write(buf);
  }

  override void flush() {
    auto pf = new TBinaryProtocolFactory!();
    auto input = new TMemoryBuffer(request_.getContents());
    request_.reset();
    processor_.process(pf.getProtocol(input), pf.getProtocol(reply_));
  }

  TAsyncManager manager_;
  TProcessor processor_;
  TMemoryBuffer request_;
  TMemoryBuffer reply_;
}

void main() {
  auto manager = new QueueingManager;
  auto processor = new TServiceProcessor!Calculator(new CalculatorHandler);
  auto transport = new LoopbackTransport(manager, processor);
  auto client = new TAsyncClient!Calculator(transport,
    new TTransportFactory, new TBinaryProtocolFactory!());

  // The caller changes the struct after issuing the call, as the tutorial's
  // async client does (THRIFT-2131); each call must see the values it was
  // made with.
  Request r;
  r.a = 1; r.b = 1; r.op = "+";
  auto first = client.calculate(r);
  r.a = 15; r.b = 10; r.op = "-";
  auto second = client.calculate(r);

  // Out of scope parameters must stay valid, too.
  TFuture!string third;
  {
    Request scoped;
    scoped.a = 7; scoped.b = 6; scoped.op = "*";
    third = client.calculate(scoped);
    scoped.a = 0; scoped.b = 0; scoped.op = "?";
  }

  manager.runAll();

  enforce(first.get() == "1+1", "first call sent " ~ first.get());
  enforce(second.get() == "15-10", "second call sent " ~ second.get());
  enforce(third.get() == "7*6", "third call sent " ~ third.get());
}
