package org.apache.thrift.test;

import java.io.IOException;
import java.io.PrintStream;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

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



public class TestIOStreamClient {
	public static void main(String[] args) throws TException {
		ProcessBuilder builder = new ProcessBuilder("java", "-jar", "bin\\streamServer.jar");
		  
		try
		{
			Process server = builder.start();
		  
			//store output stream before we redirect it.
			PrintStream tmp = System.out;
			
			System.setIn(server.getInputStream());
			System.setOut(new PrintStream(server.getOutputStream()));
		  
			TTransport transport = new TIOStreamTransport(System.in, System.out);
			transport.open();
			TProtocol protocol = new TJSONProtocol(transport);
			ThriftTest.Client testClient = new ThriftTest.Client(protocol);
			
			//variables to store results
			int failCount = 0;//imitating TestClient.java...
			
			//these are initialized to make java happy. They WILL be set.
			String s = null;
			byte i8 = 0;
			int i32 = 0;
			long i64 = 0, uid = 0, onewayElapsedMillis = -1;
			double dub = 0;
			Xtruct out = null, in = null, result = null;
			Xtruct2 out2 = null, in2 = null;
			Map<Integer, Integer> mapout = null, mapin = null;
			Set<Integer> setout = null, setin = null;
			List<Integer> listout = null, listin = null;
			Numberz ret1 = null, ret2 = null, ret3 = null, ret5 = null, ret8 = null;
			Map<Integer,Map<Integer,Integer>> mm = null;
			Insanity insane = null;
			Map<Long,Map<Numberz,Insanity>> whoa = null;
			Xception e1 = null, e4 = null;
			Xception2 e5 = null;
			TException e2 = null;
			Exception e3 = null, e6 = null;
			
			try {
		        
		        /**
		         * VOID TEST
		         */
		        try {
		          testClient.testVoid();
		          
		        } catch (TApplicationException tax) {
		          System.err.println("testVoid failed");
		          tax.printStackTrace();
		          failCount++;
		        }

		        /**
		         * STRING TEST
		         */
		        s = testClient.testString("Test");

		        /**
		         * BYTE TEST
		         */
		        i8 = testClient.testByte((byte)1);
		        
		        /**
		         * I32 TEST
		         */
		        i32 = testClient.testI32(-1);

		        /**
		         * I64 TEST
		         */
		        i64 = testClient.testI64(-34359738368L);
		        
		        /**
		         * DOUBLE TEST
		         */
		        dub = testClient.testDouble(-5.325098235);

		        /**
		         * STRUCT TEST
		         */
		        out = new Xtruct();
		        out.string_thing = "Zero";
		        out.byte_thing = (byte) 1;
		        out.i32_thing = -3;
		        out.i64_thing = -5;
		        in = testClient.testStruct(out);

		        /**
		         * NESTED STRUCT TEST
		         */
		        out2 = new Xtruct2();
		        out2.byte_thing = (short)1;
		        out2.struct_thing = out;
		        out2.i32_thing = 5;
		        in2 = testClient.testNest(out2);
		        in = in2.struct_thing;

		        /**
		         * MAP TEST
		         */
		        mapout = new HashMap<Integer,Integer>();
		        for (int i = 0; i < 5; ++i) {
		          mapout.put(i, i-10);
		        }
		        mapin = testClient.testMap(mapout);
		        
		        /**
		         * SET TEST
		         */
		        setout = new HashSet<Integer>();
		        for (int i = -2; i < 3; ++i) {
		          setout.add(i);
		        }
		        setin = testClient.testSet(setout);

		        /**
		         * LIST TEST
		         */
		        listout = new ArrayList<Integer>();
		        for (int i = -2; i < 3; ++i) {
		          listout.add(i);
		        }
		        listin = testClient.testList(listout);

		        /**
		         * ENUM TEST
		         */
		        ret1 = testClient.testEnum(Numberz.ONE);
		        ret2 = testClient.testEnum(Numberz.TWO);
		        ret3 = testClient.testEnum(Numberz.THREE);
		        ret5 = testClient.testEnum(Numberz.FIVE);
		        ret8 = testClient.testEnum(Numberz.EIGHT);

		        /**
		         * TYPEDEF TEST
		         */
		        uid = testClient.testTypedef(309858235082523L);

		        /**
		         * NESTED MAP TEST
		         */
		        mm = testClient.testMapMap(1);

		        /**
		         * INSANITY TEST
		         */
		        insane = new Insanity();
		        insane.userMap = new HashMap<Numberz, Long>();
		        insane.userMap.put(Numberz.FIVE, (long)5000);
		        Xtruct truck = new Xtruct();
		        truck.string_thing = "Truck";
		        truck.byte_thing = (byte)8;
		        truck.i32_thing = 8;
		        truck.i64_thing = 8;
		        insane.xtructs = new ArrayList<Xtruct>();
		        insane.xtructs.add(truck);
		        
		        whoa = testClient.testInsanity(insane);
		        
		        /**
		         * EXCEPTION TEST
		         */
		        try {
		          testClient.testException("Xception");
		          failCount++;
		        } catch(Xception e) {
		        	e1 = e;
		        }
		        
		        try {
		          testClient.testException("TException");
		          failCount++;
		        } catch(TException e) {
		          e2 = e;
		        }
		        
		        try {
		          testClient.testException("success");
		        } catch(Exception e) {
		          e3 = e;
		          failCount++;
		        }
		        
		        
		        /**
		         * MULTI EXCEPTION TEST
		         */
		        
		        try {
		          testClient.testMultiException("Xception", "test 1");
		          failCount++;
		        } catch(Xception e) {
		          e4 = e;
		        }
		        
		        try {
		          testClient.testMultiException("Xception2", "test 2");
		          failCount++;
		        } catch(Xception2 e) {
		          e5 = e;
		        }
		        
		        try {
		          result = testClient.testMultiException("success", "test 3");
		        } catch(Exception e) {
		        	e6 = e;
		        	failCount++;
		        }


		        
		        /**
		         * ONEWAY TEST
		         */
		        long startOneway = System.nanoTime();
		        testClient.testOneway(3);
		        onewayElapsedMillis = (System.nanoTime() - startOneway) / 1000000;

		        transport.close();
		        
		      } catch (Exception x) {
		    	System.err.println("talking to server failed");
		        x.printStackTrace();
		        failCount++;
		      }
			
			server.destroy();//kill process
			
			//print results
			
			//reset stdout
			System.setOut(tmp);
			 
			//void test
			System.out.print("testVoid() = void\n");
			
			//string test
			System.out.print("testString(\"Test\")");
			System.out.print(" = \"" + s + "\"\n");
	        if (s == null ||!s.equals("Test")) {
	          failCount++;
	          System.out.println("FAILURE\n");
	        }
	          
	        //byte test
	        System.out.print("testByte(1)");
	        System.out.print(" = " + i8 + "\n");
	        if (i8 != 1) {
	          failCount++; 
	          System.out.println("FAILURE\n");
	        }

	        //i32 test
	        System.out.print("testI32(-1)");
	        System.out.print(" = " + i32 + "\n");
	        if (i32 != -1) {
	          failCount++; 
	          System.out.println("FAILURE\n");
	        }
	        
	        //i64 test
	        System.out.print("testI64(-34359738368)");
	        System.out.print(" = " + i64 + "\n");
	        if (i64 != -34359738368L) {
	          failCount++;
	          System.out.println("FAILURE\n");
	        }

	        //double test
	        System.out.print("testDouble(-5.325098235)");
	        System.out.print(" = " + dub + "\n");
	        if (Math.abs(dub - (-5.325098235)) > 0.001) {
	          failCount++;
	          System.out.println("FAILURE\n");
	        }
	        
	        //struct test
	        System.out.print("testStruct({\"Zero\", 1, -3, -5})");
	        System.out.print(" = {" + "\"" + 
	                         in.string_thing + "\"," + 
	                         in.byte_thing + ", " + 
	                         in.i32_thing + ", " + 
	                         in.i64_thing + "}\n");
	        if (!in.equals(out)) {
	          failCount++; 
	          System.out.println("FAILURE\n");
	        }
	        
	        //nested struct test
	        System.out.print("testNest({1, {\"Zero\", 1, -3, -5}), 5}");
	        System.out.print(" = {" + in2.byte_thing + ", {" + "\"" + 
	                         in.string_thing + "\", " + 
	                         in.byte_thing + ", " +
	                         in.i32_thing + ", " +
	                         in.i64_thing + "}, " +
	                         in2.i32_thing + "}\n");
	        if (!in2.equals(out2)) {
	          failCount++;
	          System.out.println("FAILURE\n");
	        }
	        
	        //map test
	        System.out.print("testMap({");
	        boolean first = true;
	        for (int key : mapout.keySet()) {
	          if (first) {
	            first = false;
	          } else {
	            System.out.print(", ");
	          }
	          System.out.print(key + " => " + mapout.get(key));
	        }
	        System.out.print("})");
	        System.out.print(" = {");
	        first = true;
	        for (int key : mapin.keySet()) {
	          if (first) {
	            first = false;
	          } else {
	            System.out.print(", ");
	          }
	          System.out.print(key + " => " + mapout.get(key));
	        }
	        System.out.print("}\n");
	        if (!mapout.equals(mapin)) {
	          failCount++; 
	          System.out.println("FAILURE\n");
	        }
	        
	        //set test
	        System.out.print("testSet({");
	        first = true;
	        for (int elem : setout) {
	          if (first) {
	            first = false;
	          } else {
	            System.out.print(", ");
	          }
	          System.out.print(elem);
	        }
	        System.out.print("})");
	        
	        System.out.print(" = {");
	        first = true;
	        for (int elem : setin) {
	          if (first) {
	            first = false;
	          } else {
	            System.out.print(", ");
	          }
	          System.out.print(elem);
	        }
	        System.out.print("}\n");
	        if (!setout.equals(setin)) {
	          failCount++; 
	          System.out.println("FAILURE\n");
	        }
	        
	        //list test
	        System.out.print("testList({");
	        first = true;
	        for (int elem : listout) {
	          if (first) {
	            first = false;
	          } else {
	            System.out.print(", ");
	          }
	          System.out.print(elem);
	        }
	        System.out.print("})");
	        System.out.print(" = {");
	        first = true;
	        for (int elem : listin) {
	          if (first) {
	            first = false;
	          } else {
	            System.out.print(", ");
	          }
	          System.out.print(elem);
	        }
	        System.out.print("}\n");
	        if (!listout.equals(listin)) {
	          failCount++; 
	          System.out.println("FAILURE\n");
	        }
	        
	        //enum test
	        System.out.print("testEnum(ONE)");
	        System.out.print(" = " + ret1 + "\n");
	        if (ret1 != Numberz.ONE) {
	          failCount++;
	          System.out.println("FAILURE\n");
	        }

	        System.out.print("testEnum(TWO)");
	        System.out.print(" = " + ret2 + "\n");
	        if (ret2 != Numberz.TWO) {
	          failCount++; 
	          System.out.println("FAILURE\n");
	        }

	        System.out.print("testEnum(THREE)");
	        System.out.print(" = " + ret3 + "\n");
	        if (ret3 != Numberz.THREE) {
	          failCount++;
	          System.out.println("FAILURE\n");
	        }

	        System.out.print("testEnum(FIVE)");
	        System.out.print(" = " + ret5 + "\n");
	        if (ret5 != Numberz.FIVE) {
	          failCount++;
	          System.out.println("FAILURE\n");
	        }

	        System.out.print("testEnum(EIGHT)");
	        System.out.print(" = " + ret8 + "\n");
	        if (ret8 != Numberz.EIGHT) {
	          failCount++;
	          System.out.println("FAILURE\n");
	        }
	        
	        //typedef test
	        System.out.print("testTypedef(309858235082523)");
	        System.out.print(" = " + uid + "\n");
	        if (uid != 309858235082523L) {
	          failCount++;
	          System.out.println("FAILURE\n");
	        }
	       
	        //nested map test
	        System.out.print("testMapMap(1)");
	        System.out.print(" = {");
	        for (int key : mm.keySet()) {
	          System.out.print(key + " => {");
	          Map<Integer,Integer> m2 = mm.get(key);
	          for (int k2 : m2.keySet()) {
	            System.out.print(k2 + " => " + m2.get(k2) + ", ");
	          }
	          System.out.print("}, ");
	        }
	        System.out.print("}\n");
	        
	        //insane test
	        System.out.print("testInsanity()");
	        
	        System.out.print(" = {");
	        for (long key : whoa.keySet()) {
	          Map<Numberz,Insanity> val = whoa.get(key);
	          System.out.print(key + " => {");

	          for (Numberz k2 : val.keySet()) {
	            Insanity v2 = val.get(k2);
	            System.out.print(k2 + " => {");
	            Map<Numberz, Long> userMap = v2.userMap;
	            System.out.print("{");
	            if (userMap != null) {
	              for (Numberz k3 : userMap.keySet()) {
	                System.out.print(k3 + " => " + userMap.get(k3) + ", ");
	              }
	            }
	            System.out.print("}, ");

	            List<Xtruct> xtructs = v2.xtructs;
	            System.out.print("{");
	            if (xtructs != null) {
	              for (Xtruct x : xtructs) {
	                System.out.print("{" + "\"" + x.string_thing + "\", " + x.byte_thing + ", " + x.i32_thing + ", "+ x.i64_thing + "}, ");
	              }
	            }
	            System.out.print("}");

	            System.out.print("}, ");
	          }
	          System.out.print("}, ");
	        }
	        System.out.print("}\n");
	        
	        //exception test
	        System.out.print("testClient.testException(\"Xception\") =>");
	        if (e1 == null)
	        	System.out.print("  void\nFAILURE\n");
	        else
	        	System.out.printf("  {%d, \"%s\"}\n", e1.errorCode, e1.message);
	        
	        System.out.print("testClient.testException(\"TException\") =>");
	        if (e2 == null)
	        	System.out.print("  void\nFAILURE\n");
	        else
	        	System.out.printf("  {\"%s\"}\n", e2.getMessage());
	        
	        System.out.print("testClient.testException(\"success\") =>");
	        if (e3 != null)
	        	System.out.printf("  exception\nFAILURE\n");
	        else
	        	System.out.print("  void\n");
	        
	        //multi exception test
	        System.out.printf("testClient.testMultiException(\"Xception\", \"test 1\") =>");
	        if (e4 == null)
	        	System.out.print("  result\nFAILURE\n");
	        else
	        	System.out.printf("  {%d, \"%s\"}\n", e4.errorCode, e4.message);
	        
	        System.out.printf("testClient.testMultiException(\"Xception2\", \"test 2\") =>");
	        if (e5 == null)
	        	System.out.print("  result\nFAILURE\n");
	        else
	        	System.out.printf("  {%d, {\"%s\"}}\n", e5.errorCode, e5.struct_thing.string_thing);
	        
	        System.out.print("testClient.testMultiException(\"success\", \"test 3\") =>");
	        if (e6 != null)
	        	System.out.printf("  exception\nFAILURE\n");
	        else
	        	System.out.printf("  {{\"%s\"}}\n", result.string_thing);
	        
	        //oneway test
	        System.out.print("testOneway(3)...");
	        if (onewayElapsedMillis > 200) {
	            System.out.println("Oneway test failed: took " +
	                                Long.toString(onewayElapsedMillis) +
	                                "ms");
	            failCount++;
	          } else {
	            System.out.println("Success - took " +
	                               Long.toString(onewayElapsedMillis) +
	                               "ms");
	         }
	        System.exit(failCount);
		}
		  catch (IOException e)
		  {
			  e.printStackTrace();
		  }
	}
}
