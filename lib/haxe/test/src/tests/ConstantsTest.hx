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

package tests;

import haxe.Int64;
import haxe.io.BytesBuffer;
import tests.TestBase;

import org.apache.thrift.*;
import org.apache.thrift.protocol.*;
import org.apache.thrift.transport.*;
import org.apache.thrift.server.*;
import org.apache.thrift.meta_data.*;

import constantsDemo.*;  // generated code


class ConstantsTest extends tests.TestBase {

    public static function Run(server : Bool) : Void
    {
		TestConstants();		
    	TestProtocolConformity();
    }


	private static function TestConstants() : Void
	{
        tests.TestBase.Expect( ConstantsDemoConstants.myInt == 3, "myInt = 3");
        tests.TestBase.Expect( ConstantsDemoConstants.hex_const == 0x0001F, "hex_const = 31");
        tests.TestBase.Expect( ConstantsDemoConstants.negative_hex_constant == -0x0001F, "negative_hex_constant = -31");
        tests.TestBase.Expect( ConstantsDemoConstants.GEN_ME == -3523553, "GEN_ME = -3523553");
        tests.TestBase.Expect( ConstantsDemoConstants.GEn_DUB == 325.532, "GEn_DUB = 325.532");
        tests.TestBase.Expect( ConstantsDemoConstants.GEn_DU == 85.2355, "GEn_DU = 85.2355");
        tests.TestBase.Expect( ConstantsDemoConstants.GEN_STRING == "asldkjasfd", "GEN_STRING = \"asldkjasfd\"");
        tests.TestBase.Expect( ConstantsDemoConstants.e10 == 1e+10, "e10 = 1e+10");
        tests.TestBase.Expect( ConstantsDemoConstants.e11 == -1e+10, "e11 = -1e+10");
        tests.TestBase.Expect( ConstantsDemoConstants.GEN_UUID == "00000000-4444-CCCC-ffff-0123456789ab", "GEN_UUID = \"00000000-4444-CCCC-ffff-0123456789ab\"");

        tests.TestBase.Expect( ConstantsDemoConstants.GEN_MAP.get(35532) == 233, "GEN_MAP.get(35532) == 233");
        tests.TestBase.Expect( ConstantsDemoConstants.GEN_MAP.get(43523) == 853, "GEN_MAP.get(43523) == 853");
        
        tests.TestBase.Expect( ConstantsDemoConstants.GEN_LIST.length == 3, "GEN_LIST.size() == 3");
        tests.TestBase.Expect( ConstantsDemoConstants.GEN_LIST.join("/") == "235235/23598352/3253523", "GEN_LIST elements");

        tests.TestBase.Expect( ConstantsDemoConstants.GEN_MAPMAP.get(235).get(532) == 53255, "GEN_MAPMAP.get(235).get(532) == 53255");
        tests.TestBase.Expect( ConstantsDemoConstants.GEN_MAPMAP.get(235).get(235) == 235, "GEN_MAPMAP.get(235).get(235) == 235");

        tests.TestBase.Expect( ConstantsDemoConstants.GEN_MAP2.get("hello") == 233, "GEN_MAP2.get(\"hello\") == 233");
        tests.TestBase.Expect( ConstantsDemoConstants.GEN_MAP2.get("lkj98d") == 853, "GEN_MAP2.get(\"lkj98d\") == 853");
        tests.TestBase.Expect( ConstantsDemoConstants.GEN_MAP2.get('lkjsdf') == 98325, "GEN_MAP2.get('lkjsdf') == 98325");
        
        tests.TestBase.Expect( ConstantsDemoConstants.GEN_THING.hello == 325, "GEN_THING.hello == 325");
        tests.TestBase.Expect( ConstantsDemoConstants.GEN_THING.goodbye == 325352, "GEN_THING.goodbye == 325352");

        tests.TestBase.Expect( ConstantsDemoConstants.GEN_WHAT.get(35).hello == 325, "GEN_WHAT.get(35).hello == 325");
        tests.TestBase.Expect( ConstantsDemoConstants.GEN_WHAT.get(35).goodbye == 325352, "GEN_WHAT.get(35).goodbye == 325352");

        tests.TestBase.Expect( ConstantsDemoConstants.GEN_SET.size == 2, "GEN_SET.size() == 2");
        tests.TestBase.Expect( ConstantsDemoConstants.GEN_SET.contains(235), "GEN_SET.contains(235)");  // added twice, but this is a set
        tests.TestBase.Expect( ConstantsDemoConstants.GEN_SET.contains(53235), "GEN_SET.contains(53235)");
    }

	private static function TestProtocolConformity() : Void
	{
		for( factory in [new TBinaryProtocolFactory(), new TCompactProtocolFactory(), new TJSONProtocolFactory()])
		{
			DeserializeGuidData(factory);
		}
    }

	private static function DeserializeGuidData(factory : TProtocolFactory) : Void
	{
		var data = new BytesBuffer();
		var sCase = Type.getClassName(Type.getClass(factory)).split('.').pop();
		switch(sCase)
		{
			case "TJSONProtocolFactory":
				data.addString('"00112233-4455-6677-8899-aabbccddeeff"');

			case "TCompactProtocolFactory":
				data.addByte(0x00);
				data.addByte(0x11);
				data.addByte(0x22);
				data.addByte(0x33);
				data.addByte(0x44);
				data.addByte(0x55);
				data.addByte(0x66);
				data.addByte(0x77);
				data.addByte(0x88);
				data.addByte(0x99);
				data.addByte(0xaa);
				data.addByte(0xbb);
				data.addByte(0xcc);
				data.addByte(0xdd);
				data.addByte(0xee);
				data.addByte(0xff);

			case "TBinaryProtocolFactory":
				data.addByte(0x00);
				data.addByte(0x11);
				data.addByte(0x22);
				data.addByte(0x33);
				data.addByte(0x44);
				data.addByte(0x55);
				data.addByte(0x66);
				data.addByte(0x77);
				data.addByte(0x88);
				data.addByte(0x99);
				data.addByte(0xaa);
				data.addByte(0xbb);
				data.addByte(0xcc);
				data.addByte(0xdd);
				data.addByte(0xee);
				data.addByte(0xff);

			default:
				tests.TestBase.Expect( false, 'Unhandled ${sCase}');
		}

		var stream = new TMemoryStream(data.getBytes());
		stream.Position = 0;
		
		var config = new TConfiguration();
		var transport = new TStreamTransport(stream, stream, config);
		var protocol = factory.getProtocol(transport);
		
		var sUuid = protocol.readUuid();	
		tests.TestBase.Expect( sUuid == "00112233-4455-6677-8899-aabbccddeeff", 'DeserializeGuidData(${sCase}): ${sUuid}');
	}
}


