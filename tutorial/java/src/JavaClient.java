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

// Generated code
import tutorial.*;
import shared.*;

import org.apache.thrift.TException;
import org.apache.thrift.transport.TTransport;
import org.apache.thrift.transport.TSocket;
import org.apache.thrift.transport.TTransportException;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocol;

import java.util.AbstractMap;
import java.util.HashMap;
import java.util.HashSet;
import java.util.ArrayList;

public class JavaClient {
  public static void main(String [] args) {
    try {

      TTransport transport = new TSocket("localhost", 9090);
      TProtocol protocol = new TBinaryProtocol(transport);
      Calculator.Client client = new Calculator.Client(protocol);

      transport.open();

      client.ping();
      System.out.println("ping()");

      int sum = client.add(1,1);
      System.out.println("1+1=" + sum);

      Work work = new Work();

      work.op = Operation.DIVIDE;
      work.num1 = 1;
      work.num2 = 0;
      try {
        int quotient = client.calculate(1, work);
        System.out.println("Whoa we can divide by 0");
      } catch (InvalidOperation io) {
        System.out.println("Invalid operation: " + io.why);
      }

      work.op = Operation.SUBTRACT;
      work.num1 = 15;
      work.num2 = 10;
      try {
        int diff = client.calculate(1, work);
        System.out.println("15-10=" + diff);
      } catch (InvalidOperation io) {
        System.out.println("Invalid operation: " + io.why);
      }

      SharedStruct log = client.getStruct(1);
      System.out.println("Check log: " + log.value);

      transport.close();

    } catch (TException x) {
      x.printStackTrace();
    }

  }

}
