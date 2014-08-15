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

package;

import haxe.Int32;
import haxe.Int64;
import haxe.ds.IntMap;
import haxe.ds.StringMap;
import haxe.ds.ObjectMap;

import org.apache.thrift.*;
import org.apache.thrift.helper.*;
import org.apache.thrift.protocol.*;
import org.apache.thrift.transport.*;
import org.apache.thrift.server.*;
import org.apache.thrift.meta_data.*;

#if cpp
import cpp.vm.Thread;
#else
// no thread support (yet)
#end

import thrift.test.*;  // generated code


class TestClient {
	
	public static function Execute(args : Arguments) :  Void
	{
		try
		{
			var difft = Date.now().getTime();
			
			if( args.numThreads > 1) {
				var threads = new List<Thread>();
				for( test in 0 ... (args.numThreads-1)) {
					threads.add( StartThread( args));
				} 
				for( thread in threads) {
					Thread.readMessage(true);
				}
			} else {
				RunClient(args);
			}

			difft = Date.now().getTime() - difft;
			difft = (24 * 60 * 60) * difft;
			trace('Total time: $difft seconds');
		}
		catch (e : TException)
		{
			trace('$e');
		}
		catch (e : Dynamic)
		{
			trace('$e');
		}
	}

	
	private static function StartThread(args : Arguments) : Thread {
		var thread = Thread.create(
			function() : Void {
				var main : Thread = Thread.readMessage(true);
				try 
				{
					RunClient(args);
				}
				catch (e : TException)
				{
					trace('$e');
				}
				catch (e : Dynamic)
				{
					trace('$e');
				}					
				main.sendMessage("done");
			});
		
		thread.sendMessage(Thread.current());
		return thread;
	}

	
	public static function RunClient(args : Arguments)
	{
		var transport : TTransport = null;
		switch (args.transport)
		{
			case socket:
				transport = new TSocket(args.host, args.port);
			case http:
				throw "http transport not supported yet";
				//transport = new THttpClient(args.host);
			default:
				throw "Unhandled transport";
		}

		// optional: layered transport
		if ( args.framed) {
			trace("- framed transport");
			transport = new TFramedTransport(transport);
		} else if ( args.buffered) {
			trace("- buffered transport");
			throw "TBufferedTransport not implemented yet";
			//transport = new TBufferedTransport(transport);
		}

		// protocol
		var protocol : TProtocol = null;
		switch( args.protocol)
		{
		case binary:
			trace("- binary protocol");
			protocol = new TBinaryProtocol(transport);
		case json:
			trace("- json protocol");
			throw "JSON protocol not implemented yet";
			//protocol = new TJsonProtocol(transport);
		default:
			throw "Unhandled protocol";
		}


		ClientTest( transport, protocol);
					
	}

	public static function ClientTest( transport : TTransport, protocol : TProtocol) : Void
	{
		var client = new ThriftTestImpl(protocol,protocol);
		try
		{
			if (!transport.isOpen())
			{
				transport.open();
			}
		}
		catch (e : TException)
		{
			trace('$e');
			return;
		}
		catch (e : Dynamic)
		{
			trace('$e');
			return;
		}

		var start = Date.now();

		trace('testVoid()');
		client.testVoid();
		trace(' = void');

		trace('testString("Test")');
		var s = client.testString("Test");
		trace(' = "$s"');

		trace('testByte(1)');
		var i8 = client.testByte(1);
		trace(' = $i8');

		trace('testI32(-1)');
		var i32 = client.testI32(-1);
		trace(' = $i32');

		trace('testI64(-34359738368)');
		var i64 = client.testI64( Int64.make( 0xFFFFFFF8, 0x00000000)); // -34359738368
		trace(' = $i64');

		trace('testDouble(5.325098235)');
		var dub = client.testDouble(5.325098235);
		trace(' = $dub');

		trace('testStruct({"Zero", 1, -3, -5})');
		var o = new Xtruct();
		o.string_thing = "Zero";
		o.byte_thing = 1;
		o.i32_thing = -3;
		o.i64_thing = Int64.make(0,-5);
		var i = client.testStruct(o);
		trace(' = {"' + i.string_thing + '", ' + i.byte_thing +', '+ i.i32_thing +', '+ i.i64_thing + '}');

		trace('testNest({1, {\"Zero\", 1, -3, -5}, 5})');
		var o2 = new Xtruct2();
		o2.byte_thing = 1;
		o2.struct_thing = o;
		o2.i32_thing = 5;
		var i2 = client.testNest(o2);
		i = i2.struct_thing;
		trace(" = {" + i2.byte_thing + ", {\"" + i.string_thing + "\", " 
			  + i.byte_thing + ", " + i.i32_thing + ", " + i.i64_thing + "}, " 
			  + i2.i32_thing + "}");

		var mapout = new IntMap< haxe.Int32>();
		for ( j in 0 ... 4)
		{
			mapout.set(j, j - 10);
		}
		trace("testMap({");
		var first : Bool = true;
		for( key in mapout.keys())
		{
			if (first)
			{
				first = false;
			}
			else
			{
				trace(", ");
			}
			trace(key + " => " + mapout.get(key));
		}
		trace("})");

		var mapin = client.testMap(mapout);

		trace(" = {");
		first = true;
		for( key in mapin.keys())
		{
			if (first)
			{
				first = false;
			}
			else
			{
				trace(", ");
			}
			trace(key + " => " + mapin.get(key));
		}
		trace("}");

		var listout = new List<Int>();
		for (j in -2 ... 2)
		{
			listout.add(j);
		}
		trace("testList({");
		first = true;
		for( j in listout)
		{
			if (first)
			{
				first = false;
			}
			else
			{
				trace(", ");
			}
			trace(j);
		}
		trace("})");

		var listin = client.testList(listout);

		trace(" = {");
		first = true;
		for( j in listin)
		{
			if (first)
			{
				first = false;
			}
			else
			{
				trace(", ");
			}
			trace(j);
		}
		trace("}");

		//set
		var setout = new IntSet();
		for (j in -2 ... 3)
		{
			setout.add(j);
		}
		trace("testSet({");
		first = true;
		for( j in setout)
		{
			if (first)
			{
				first = false;
			}
			else
			{
				trace(", ");
			}
			trace(j);
		}
		trace("})");

		var setin = client.testSet(setout);

		trace(" = {");
		first = true;
		for( j in setin)
		{
			if (first)
			{
				first = false;
			}
			else
			{
				trace(", ");
			}
			trace(j);
		}
		trace("}");


		trace("testEnum(ONE)");
		var ret = client.testEnum(Numberz.ONE);
		trace(" = " + ret);

		trace("testEnum(TWO)");
		ret = client.testEnum(Numberz.TWO);
		trace(" = " + ret);

		trace("testEnum(THREE)");
		ret = client.testEnum(Numberz.THREE);
		trace(" = " + ret);

		trace("testEnum(FIVE)");
		ret = client.testEnum(Numberz.FIVE);
		trace(" = " + ret);

		trace("testEnum(EIGHT)");
		ret = client.testEnum(Numberz.EIGHT);
		trace(" = " + ret);

		trace("testTypedef(309858235082523)");
		var uid = client.testTypedef( Int64.make( 0x119D0, 0x7E08671B));  // 309858235082523
		trace(" = " + uid);

		trace("testMapMap(1)");
		var mm = client.testMapMap(1);
		trace(" = {");
		for( key in mm.keys())
		{
			trace(key + " => {");
			var m2 = mm.get(key);
			for( k2 in m2.keys())
			{
				trace(k2 + " => " + m2.get(k2) + ", ");
			}
			trace("}, ");
		}
		trace("}");

		var insane = new Insanity();
		insane.userMap = new IntMap< Int64>();
		insane.userMap.set( Numberz.FIVE, Int64.make(0,5000));
		var truck = new Xtruct();
		truck.string_thing = "Truck";
		truck.byte_thing = 8;
		truck.i32_thing = 8;
		truck.i64_thing = Int64.make(0,8);
		insane.xtructs = new List<Xtruct>();
		insane.xtructs.add(truck);
		trace("testInsanity()");
		var whoa = client.testInsanity(insane);
		trace(" = {");
		for( key in whoa.keys())
		{
			var val = whoa.get(key);
			trace(key + " => {");

			for( k2 in val.keys())
			{
				var v2 = val.get(k2);

				trace(k2 + " => {");
				var userMap = v2.userMap;

				trace("{");
				if (userMap != null)
				{
					for( k3 in userMap.keys())
					{
						trace(k3 + " => " + userMap.get(k3) + ", ");
					}
				}
				else
				{
					trace("null");
				}
				trace("}, ");

				var xtructs = v2.xtructs;

				trace("{");
				if (xtructs != null)
				{
					for( x in xtructs)
					{
						trace("{\"" + x.string_thing + "\", " 
							  + x.byte_thing + ", " + x.i32_thing + ", " 
							  + x.i32_thing + "}, ");
					}
				}
				else
				{
					trace("null");
				}
				trace("}");

				trace("}, ");
			}
			trace("}, ");
		}
		trace("}");

		var arg0 = 1;
		var arg1 = 2;
		var arg2 = Int64.make( 0x7FFFFFFF,0xFFFFFFFF);
		var multiDict = new IntMap< String>();
		multiDict.set(1, "one");
		var arg4 = Numberz.FIVE;
		var arg5 = Int64.make(0,5000000);
		trace("Test Multi(" + arg0 + "," + arg1 + "," + arg2 + "," + multiDict + "," + arg4 + "," + arg5 + ")");
		var multiResponse = client.testMulti(arg0, arg1, arg2, multiDict, arg4, arg5);
		trace(" = Xtruct(byte_thing:" + multiResponse.byte_thing + ",string_thing:" + multiResponse.string_thing
					+ ",i32_thing:" + multiResponse.i32_thing + ",i64_thing:" + multiResponse.i64_thing + ")\n");

		trace("Test Oneway(1)");
		client.testOneway(1);

		trace("Test Calltime()");
		var difft = Date.now().getTime();
		for ( k in 0 ... 999) {
			client.testVoid();
		}
		difft = Date.now().getTime() - difft;
		difft = (24.0 * 60 * 60) * difft;
		trace(' = $difft ms a testVoid() call');
	}
}
