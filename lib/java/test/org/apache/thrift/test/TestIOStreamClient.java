package org.apache.thrift.test;

import java.io.IOException;
import java.io.PrintStream;

import org.apache.thrift.protocol.TJSONProtocol;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.transport.TIOStreamTransport;
import org.apache.thrift.transport.TTransport;

import src.MathService;



public class TestIOStreamClient {
	public static void main(String[] args) throws TException {
		ProcessBuilder builder = new ProcessBuilder("java", "-jar", "C:\\Users\\esjohn\\Documents\\JavaIOTransport\\server.jar");
		  
		try
		{
			Process server = builder.start();
		  
			PrintStream tmp = System.out;
			int sum, diff, product, quotient, mod;
			
			System.setIn(server.getInputStream());
			System.setOut(new PrintStream(server.getOutputStream()));
		  
			TTransport transport = new TIOStreamTransport(System.in, System.out);
			transport.open();
			TProtocol protocol = new TJSONProtocol(transport);
			MathService.Client client = new MathService.Client(protocol);
			
			sum = client.add(10,20);
			diff = client.sub(10,20);
			product = client.mul(10,20);
			quotient = client.div(10,20);
			mod = client.mod(10,20);
			  
			System.setOut(tmp);
			  
			System.out.println(sum);
			System.out.println(diff);
			System.out.println(product);
			System.out.println(quotient);
			System.out.println(mod);
			  
			transport.close();
			  
			server.destroy();//kill process
		  }
		  catch (IOException e)
		  {
			  e.printStackTrace();
		  }
	}
}
