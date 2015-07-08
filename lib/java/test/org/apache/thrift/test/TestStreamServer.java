package org.apache.thrift.test;

import org.apache.thrift.server.TStreamServer;
import org.apache.thrift.transport.TIOStreamTransport;

public class TestStreamServer {
	public static MathServer handler;
	public static MathService.Processor processor;
	
	public static void main(String[] args)
	{	
		try
        {
            handler = new MathServer();
            processor = new MathService.Processor(handler);

            TIOStreamTransport transport = new TIOStreamTransport(System.in, System.out);

            TStreamServer server = new TStreamServer(processor, transport);
            server.serve();
        }
        catch (Exception x)
        {
            x.printStackTrace();
        }   
	}

}
