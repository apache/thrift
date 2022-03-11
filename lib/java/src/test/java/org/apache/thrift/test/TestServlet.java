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

package org.apache.thrift.test;

import org.apache.thrift.TProcessor;
import org.apache.thrift.protocol.TCompactProtocol;
import org.apache.thrift.protocol.TProtocolFactory;
import org.apache.thrift.server.ServerTestBase.TestHandler;
import org.apache.thrift.server.TExtensibleServlet;

import thrift.test.ThriftTest;

@SuppressWarnings("serial")
public class TestServlet extends TExtensibleServlet {

  @Override
  protected TProtocolFactory getInProtocolFactory(){
      TProtocolFactory tProtocolFactory = new TCompactProtocol.Factory();
      return tProtocolFactory;
  }

  @Override
  protected TProtocolFactory getOutProtocolFactory(){
      TProtocolFactory tProtocolFactory = new TCompactProtocol.Factory();
      return tProtocolFactory;
  }

  @SuppressWarnings({"rawtypes", "unchecked"})
  @Override
  protected TProcessor getProcessor(){
      TestHandler testHandler = new TestHandler();
      ThriftTest.Processor testProcessor = new ThriftTest.Processor(testHandler);
      return testProcessor;
  }
}
