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

package org.apache.thrift.server;

import org.apache.thrift.TException;
import org.apache.thrift.TProcessor;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocolFactory;
import org.apache.thrift.transport.TTransport;
import org.apache.thrift.transport.TTransportFactory;
import org.apache.thrift.transport.TIOStreamTransport;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

public class TStreamServer {

	private static final Logger LOGGER = LoggerFactory.getLogger(TStreamServer.class.getName());
	
	private TIOStreamTransport transport;
	private TProcessor processor;
	private TTransportFactory inputTransportFactory;
	private TTransportFactory outputTransportFactory;
	private TProtocolFactory inputProtocolFactory;
	private TProtocolFactory outputProtocolFactory;
	
	public TStreamServer(TProcessor processor, TIOStreamTransport transport)
	{
		this(processor, transport, new TTransportFactory(), new TBinaryProtocol.Factory());
	}
	
	public TStreamServer(TProcessor processor, TIOStreamTransport transport, TTransportFactory transportFactory, TProtocolFactory protocolFactory)
	{
		this.processor = processor;
		this.transport = transport;
		this.inputTransportFactory = transportFactory;
		this.outputTransportFactory = transportFactory;
		this.inputProtocolFactory = protocolFactory;
		this.outputProtocolFactory = protocolFactory;
	}
	
	public void serve() {
	    while (true) {
	      TTransport client = null;
	      TTransport inputTransport = null;
	      TTransport outputTransport = null;
	      TProtocol inputProtocol = null;
	      TProtocol outputProtocol = null;
	      client = transport;
	      
	      if (client != null) {
	          inputTransport = inputTransportFactory.getTransport(client);
	          outputTransport = outputTransportFactory.getTransport(client);
	          inputProtocol = inputProtocolFactory.getProtocol(inputTransport);
	          outputProtocol = outputProtocolFactory.getProtocol(outputTransport);
	          while (true) {
	            try {
					if(!processor.process(inputProtocol, outputProtocol)) {
					  break;
					}
				} catch (TException tx) {
					LOGGER.error("Thrift error occurred during processing of message. ", tx.getMessage());
				}
	          }
	      }

	      if (inputTransport != null) {
	        inputTransport.close();
	      }

	      if (outputTransport != null) {
	        outputTransport.close();
	      }

	    }
	  }
}
