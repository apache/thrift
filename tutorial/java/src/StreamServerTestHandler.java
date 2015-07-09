package org.apache.thrift.server;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.thrift.TException;

import thrift.test.Insanity;
import thrift.test.Numberz;
import thrift.test.ThriftTest;
import thrift.test.Xception;
import thrift.test.Xception2;
import thrift.test.Xtruct;
import thrift.test.Xtruct2;

public class StreamServerTestHandler {

	public static class TestHandler implements ThriftTest.Iface {
		
		public TestHandler() { }
		
		public void testVoid() {
		    //System.out.print("testVoid()\n");
		}
		  
		public String testString(String thing) {
			//System.out.print("testString(\"" + thing + "\")\n");
			return thing;
		}
		  
		public byte testByte(byte thing) {
			//System.out.print("testByte(" + thing + ")\n");
			return thing;
		}
		  
		public int testI32(int thing) {
			//System.out.print("testI32(" + thing + ")\n");
			return thing;
		}
		  
		public long testI64(long thing) {
			//System.out.print("testI64(" + thing + ")\n");
			return thing;
		}
		  
		public double testDouble(double thing) {
			//System.out.print("testDouble(" + thing + ")\n");
			return thing;
		}

		public ByteBuffer testBinary(ByteBuffer thing) {
			String hexstr = "TODO: toHexString(thing)";
			//System.out.print("testBinary(" + hexstr + ")\n");
			return thing;
		}

		public Xtruct testStruct(Xtruct thing) {
			/*System.out.print("testStruct({" +
					"\"" + thing.string_thing + "\", " +
					thing.byte_thing + ", " +
					thing.i32_thing + ", " +
					thing.i64_thing + "})\n");*/
		    return thing;
		}
		
		public Xtruct2 testNest(Xtruct2 nest) {
			Xtruct thing = nest.struct_thing;
			/*System.out.print("testNest({" +
					nest.byte_thing + ", {" +
					"\"" + thing.string_thing + "\", " +
					thing.byte_thing + ", " +
					thing.i32_thing + ", " +
					thing.i64_thing + "}, " +
					nest.i32_thing + "})\n");*/
			return nest;
		}
		  
		public Map<Integer,Integer> testMap(Map<Integer,Integer> thing) {
			//System.out.print("testMap({");
			//System.out.print(thing);
			//System.out.print("})\n");
			return thing;
		}

		public Map<String,String> testStringMap(Map<String,String> thing) {
			//System.out.print("testStringMap({");
			//System.out.print(thing);
			//System.out.print("})\n");
			return thing;
		}
		  
		public Set<Integer> testSet(Set<Integer> thing) {
			//System.out.print("testSet({");
			boolean first = true;
			for (int elem : thing) {
				if (first) {
					first = false;
				} else {
					//System.out.print(", ");
				}
		        //System.out.print(elem);
			}
			//System.out.print("})\n");
			return thing;
		}
		  
		public List<Integer> testList(List<Integer> thing) {
			//System.out.print("testList({");
			boolean first = true;
			for (int elem : thing) {
				if (first) {
					first = false;
				} else {
					//System.out.print(", ");
		        }
				//System.out.print(elem);
		      }
			//System.out.print("})\n");
			return thing;
		}
		  
		public Numberz testEnum(Numberz thing) {
			//System.out.print("testEnum(" + thing + ")\n");
			return thing;
		}
		  
		public long testTypedef(long thing) {
			//System.out.print("testTypedef(" + thing + ")\n");
			return thing;
		}
		  
		public Map<Integer,Map<Integer,Integer>> testMapMap(int hello) {
			//System.out.print("testMapMap(" + hello + ")\n");
			Map<Integer,Map<Integer,Integer>> mapmap =
					new HashMap<Integer,Map<Integer,Integer>>();
		  
			HashMap<Integer,Integer> pos = new HashMap<Integer,Integer>();
			HashMap<Integer,Integer> neg = new HashMap<Integer,Integer>();
			for (int i = 1; i < 5; i++) {
				pos.put(i, i);
		        neg.put(-i, -i);
			}
		  
			mapmap.put(4, pos);
			mapmap.put(-4, neg);
		  
			return mapmap;
		}
		  
		public Map<Long, Map<Numberz,Insanity>> testInsanity(Insanity argument) {
			//System.out.print("testInsanity()\n");
		  
			Xtruct hello = new Xtruct();
			hello.string_thing = "Hello2";
			hello.byte_thing = 2;
			hello.i32_thing = 2;
			hello.i64_thing = 2;
		  
			Xtruct goodbye = new Xtruct();
			goodbye.string_thing = "Goodbye4";
			goodbye.byte_thing = (byte)4;
			goodbye.i32_thing = 4;
			goodbye.i64_thing = (long)4;
		  
			Insanity crazy = new Insanity();
			crazy.userMap = new HashMap<Numberz, Long>();
			crazy.userMap.put(Numberz.EIGHT, (long)8);
			crazy.userMap.put(Numberz.FIVE, (long)5);
			crazy.xtructs = new ArrayList<Xtruct>();
			crazy.xtructs.add(goodbye);
			crazy.xtructs.add(hello);
		  
			HashMap<Numberz,Insanity> first_map = new HashMap<Numberz, Insanity>();
			HashMap<Numberz,Insanity> second_map = new HashMap<Numberz, Insanity>();;
		  
			first_map.put(Numberz.TWO, crazy);
			first_map.put(Numberz.THREE, crazy);
		  
			Insanity looney = new Insanity();
			second_map.put(Numberz.SIX, looney);
		  
			Map<Long,Map<Numberz,Insanity>> insane =
		        new HashMap<Long, Map<Numberz,Insanity>>();
			insane.put((long)1, first_map);
			insane.put((long)2, second_map);
		      
			return insane;
		}
		  
		public Xtruct testMulti(byte arg0, int arg1, long arg2, Map<Short,String> arg3, Numberz arg4, long arg5) {
			//System.out.print("testMulti()\n");
		  
			Xtruct hello = new Xtruct();;
			hello.string_thing = "Hello2";
			hello.byte_thing = arg0;
			hello.i32_thing = arg1;
			hello.i64_thing = arg2;
			return hello;
		}
		  
		public void testException(String arg) throws Xception, TException {
			//System.out.print("testException("+arg+")\n");
			if (arg.equals("Xception")) {
				Xception x = new Xception();
		        x.errorCode = 1001;
		        x.message = arg;
		        throw x;
			} else if (arg.equals("TException")) {
		        throw new TException(arg);
			} else {
		        Xtruct result = new Xtruct();
		        result.string_thing = arg;
			}
			return;
		}
		  
		public Xtruct testMultiException(String arg0, String arg1) throws Xception, Xception2 {
			//System.out.print("testMultiException(" + arg0 + ", " + arg1 + ")\n");
			if (arg0.equals("Xception")) {
				Xception x = new Xception();
		        x.errorCode = 1001;
		        x.message = "This is an Xception";
		        throw x;
			} else if (arg0.equals("Xception2")) {
		        Xception2 x = new Xception2();
		        x.errorCode = 2002;
		        x.struct_thing = new Xtruct();
		        x.struct_thing.string_thing = "This is an Xception2";
		        throw x;
			}
		  
			Xtruct result = new Xtruct();
			result.string_thing = arg1;
			return result;
		}
		  
		public void testOneway(int sleepFor) {
			/*System.out.println("testOneway(" + Integer.toString(sleepFor) +
		                         ") => sleeping...");*/
			try {
				Thread.sleep(sleepFor * 1000);
		        //System.out.println("Done sleeping!");
			} catch (InterruptedException ie) {
		        throw new RuntimeException(ie);
			}
		}
	}
}
