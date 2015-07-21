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
