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

import org.apache.thrift.TException;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.server.TServer;
import org.apache.thrift.server.TSimpleServer;
import org.apache.thrift.transport.TServerSocket;
import org.apache.thrift.transport.TServerTransport;

// Generated code
import tutorial.*;
import shared.*;

import java.util.HashMap;

public class JavaServer {

  public static class CalculatorHandler implements Calculator.Iface {

    private HashMap<Integer,SharedStruct> log;

    public CalculatorHandler() {
      log = new HashMap<Integer, SharedStruct>();
    }

    public void ping() {
      System.out.println("ping()");
    }

    public int add(int n1, int n2) {
      System.out.println("add(" + n1 + "," + n2 + ")");
      return n1 + n2;
    }

    public int calculate(int logid, Work work) throws InvalidOperation {
      System.out.println("calculate(" + logid + ", {" + work.op + "," + work.num1 + "," + work.num2 + "})");
      int val = 0;
      switch (work.op) {
      case Operation.ADD:
        val = work.num1 + work.num2;
        break;
      case Operation.SUBTRACT:
        val = work.num1 - work.num2;
        break;
      case Operation.MULTIPLY:
        val = work.num1 * work.num2;
        break;
      case Operation.DIVIDE:
        if (work.num2 == 0) {
          InvalidOperation io = new InvalidOperation();
          io.what = work.op;
          io.why = "Cannot divide by 0";
          throw io;
        }
        val = work.num1 / work.num2;
        break;
      default:
        InvalidOperation io = new InvalidOperation();
        io.what = work.op;
        io.why = "Unknown operation";
        throw io;
      }

      SharedStruct entry = new SharedStruct();
      entry.key = logid;
      entry.value = Integer.toString(val);
      log.put(logid, entry);

      return val;
    }

    public SharedStruct getStruct(int key) {
      System.out.println("getStruct(" + key + ")");
      return log.get(key);
    }

    public void zip() {
      System.out.println("zip()");
    }

  }

  public static void main(String [] args) {
    try {
      CalculatorHandler handler = new CalculatorHandler();
      Calculator.Processor processor = new Calculator.Processor(handler);
      TServerTransport serverTransport = new TServerSocket(9090);
      TServer server = new TSimpleServer(processor, serverTransport);

      // Use this for a multithreaded server
      // server = new TThreadPoolServer(processor, serverTransport);

      System.out.println("Starting the server...");
      server.serve();

    } catch (Exception x) {
      x.printStackTrace();
    }
    System.out.println("done.");
  }
}
