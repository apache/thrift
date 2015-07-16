package org.apache.thrift.test;

import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import junit.framework.TestCase;

import org.junit.Before;
import org.junit.After;
import org.junit.Test;

import org.apache.thrift.protocol.TJSONProtocol;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.transport.TIOStreamTransport;
import org.apache.thrift.transport.TTransport;
import org.apache.thrift.transport.TTransportException;
import org.apache.thrift.TApplicationException;
import org.apache.thrift.TException;

import thrift.test.Insanity;
import thrift.test.Numberz;
import thrift.test.ThriftTest;
import thrift.test.Xception;
import thrift.test.Xception2;
import thrift.test.Xtruct;
import thrift.test.Xtruct2;


public class TestIOStreamClient extends TestCase {
	private Process server;
	private TTransport transport;
	private ThriftTest.Client testClient;
	
	@Before
	public void setUp()
	{
		ProcessBuilder builder = new ProcessBuilder("java", "-cp", "build\\*;build\\lib\\*", "org.apache.thrift.test.TestStreamServer");
		  
		try
		{
			server = builder.start();
		  
			System.setIn(server.getInputStream());
			System.setOut(new PrintStream(server.getOutputStream()));
		  
			transport = new TIOStreamTransport(System.in, System.out);
			transport.open();
			TProtocol protocol = new TJSONProtocol(transport);
			testClient = new ThriftTest.Client(protocol);
		}
		catch (IOException e)
		{
			e.printStackTrace();
		}
		catch (TTransportException e)
		{
			e.printStackTrace();
		}
	}
	
	@After
	public void tearDown()
	{
		transport.close();
		server.destroy();//kill process	
	}
			
	@Test
	public void testVoid() throws TException
	{
		try {
			testClient.testVoid();
		} catch (TApplicationException tax) {
			System.err.println("testVoid failed");
			tax.printStackTrace();
		}
	}
	
	@Test
	public void testString() throws TException
	{
		String s = testClient.testString("Test");
		assertEquals("Test", s);
	}

	@Test
	public void testByte() throws TException
	{
		byte i8 = testClient.testByte((byte)1);
		assertEquals(1, i8);
	}
	
	@Test
	public void testi32() throws TException
	{
		int i32 = testClient.testI32(-1);
		assertEquals(-1, i32);
	}
	
	@Test
	public void testi64() throws TException
	{
		long i64 = testClient.testI64(-34359738368L);
		assertEquals(-34359738368L, i64);
	}
	
	@Test
	public void testDouble() throws TException
	{
		double dub = testClient.testDouble(-5.325098235);
		assertEquals(-5.325098235, dub);
	}
	
	@Test
	public void testStruct() throws TException
	{
		Xtruct out = new Xtruct();
		out.string_thing = "Zero";
		out.byte_thing = (byte) 1;
		out.i32_thing = -3;
		out.i64_thing = -5;
		Xtruct in = testClient.testStruct(out);
		assertEquals(out, in);
	}

	@Test
	public void testNestedStruct() throws TException
	{
		Xtruct out = new Xtruct();
		out.string_thing = "Zero";
		out.byte_thing = (byte) 1;
		out.i32_thing = -3;
		out.i64_thing = -5;
		
		Xtruct2 out2 = new Xtruct2();
		out2.byte_thing = (short)1;
		out2.struct_thing = out;
		out2.i32_thing = 5;
		Xtruct2 in2 = testClient.testNest(out2);
		//in = in2.struct_thing;
		assertEquals(out2, in2);
	}

	@Test
	public void testMap() throws TException
	{
		Map<Integer, Integer> mapout = new HashMap<Integer,Integer>();
		for (int i = 0; i < 5; ++i) {
			mapout.put(i, i-10);
		}
		Map<Integer, Integer> mapin = testClient.testMap(mapout);
		assertEquals(mapout, mapin);
	}       
	
	@Test
	public void testSet() throws TException
	{
		Set<Integer> setout = new HashSet<Integer>();
		for (int i = -2; i < 3; ++i) {
			setout.add(i);
		}
		Set<Integer> setin = testClient.testSet(setout);
		assertEquals(setout, setin);
	}
	
	@Test
	public void testList() throws TException
	{
		List<Integer> listout = new ArrayList<Integer>();
		for (int i = -2; i < 3; ++i) {
			listout.add(i);
		}
		List<Integer> listin = testClient.testList(listout);
		assertEquals(listout, listin);
	}

	@Test
	public void testEnum() throws TException
	{
		Numberz ret1 = testClient.testEnum(Numberz.ONE);
		Numberz ret2 = testClient.testEnum(Numberz.TWO);
		Numberz ret3 = testClient.testEnum(Numberz.THREE);
		Numberz ret5 = testClient.testEnum(Numberz.FIVE);
		Numberz ret8 = testClient.testEnum(Numberz.EIGHT);

		assertEquals(Numberz.ONE, ret1);
		assertEquals(Numberz.TWO, ret2);
		assertEquals(Numberz.THREE, ret3);
		assertEquals(Numberz.FIVE, ret5);
		assertEquals(Numberz.EIGHT, ret8);
	}
	
	@Test
	public void testTypedef() throws TException
	{
		long uid = testClient.testTypedef(309858235082523L);
		assertEquals(309858235082523L, uid);
	}
	
	@Test
	public void testNestedMap() throws TException
	{
		//TODO: build the answer
		
		Map<Integer, Map<Integer, Integer>> mm = testClient.testMapMap(1);
		
	}
	
	@Test
	public void testInsanity() throws TException
	{
		Insanity insane = new Insanity();
		insane.userMap = new HashMap<Numberz, Long>();
		insane.userMap.put(Numberz.FIVE, (long)5000);
		Xtruct truck = new Xtruct();
		truck.string_thing = "Truck";
		truck.byte_thing = (byte)8;
		truck.i32_thing = 8;
		truck.i64_thing = 8;
		insane.xtructs = new ArrayList<Xtruct>();
		insane.xtructs.add(truck);
		
		Map<Long,Map<Numberz,Insanity>> whoa = testClient.testInsanity(insane);
		//TODO: equals assertion
		
	}
		        
	@Test
	public void testException() throws TException
	{
		try {
			testClient.testException("Xception");
			fail("should have thrown an Xception");
		} catch(Xception e) {
			assertEquals("Xception", e.message);
			assertEquals(1001, e.errorCode);
		} 
		
		try {
			testClient.testException("TException");
			fail("should have thrown a TException");
		} catch(TException e) {
			assertEquals("TException", e.getMessage());
		}
		        
		try {
			testClient.testException("success");
		} catch(Exception e) {
			fail("no exception should have been thrown");
		}
	}
		        
	@Test
	public void testMultiException() throws TException
	{		        
		try {
			testClient.testMultiException("Xception", "test 1");
			fail("should have thrown Xception");
		} catch(Xception e) {
			assertEquals(1001, e.errorCode);
			assertEquals("This is an Xception", e.message);
		} 
		        
		try {
			testClient.testMultiException("Xception2", "test 2");
			fail("should have thrown Xception2");
		} catch(Xception2 e) {
			assertEquals(2002, e.errorCode);
			assertEquals("This is an Xception2", e.getStruct_thing().string_thing);
		}
		
		try {
			Xtruct result = testClient.testMultiException("success", "test 3");
			assertEquals("test 3", result.getString_thing());
		} catch(Exception e) {
			fail("no exception should have been thrown");
		}
	}

	@Test
	public void testOneWay() throws TException
	{
		long startOneway = System.nanoTime();
		testClient.testOneway(3);
		long onewayElapsedMillis = (System.nanoTime() - startOneway) / 1000000;
		assertTrue(onewayElapsedMillis < 3);
	}
}
