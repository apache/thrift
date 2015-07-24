/*
public 
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

import org.apache.thrift.server.TStreamServer;
import org.apache.thrift.transport.TIOStreamTransport;
import org.apache.thrift.transport.TTransportFactory;
import org.apache.thrift.protocol.TJSONProtocol;
import org.apache.thrift.server.StreamServerTestHandler.TestHandler;
import thrift.test.ThriftTest;

public class TestStreamServer {
	public static TestHandler handler;
	public static ThriftTest.Processor processor;
	
	public static void main(String[] args)
	{	
		try
        {
            handler = new TestHandler();
            processor = new ThriftTest.Processor(handler);

            TIOStreamTransport transport = new TIOStreamTransport(System.in, System.out);

            TStreamServer server = new TStreamServer(processor, transport, new TTransportFactory(), new TJSONProtocol.Factory());
            server.serve();
        }
        catch (Exception x)
        {
            x.printStackTrace();
        }   
	}

}
