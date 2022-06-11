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

import haxe.Int64;

import org.apache.thrift.*;
import org.apache.thrift.protocol.*;
import org.apache.thrift.transport.*;
import org.apache.thrift.server.*;
import org.apache.thrift.meta_data.*;

import constantsDemo.*;  // generated code


class ConstantsTest extends TestBase {

    public static function Run(server : Bool) : Void
    {
        TestBase.Expect( ConstantsDemoConstants.myInt == 3, "myInt = 3");
        TestBase.Expect( ConstantsDemoConstants.hex_const == 0x0001F, "hex_const = 31");
        TestBase.Expect( ConstantsDemoConstants.negative_hex_constant == -0x0001F, "negative_hex_constant = -31");
        TestBase.Expect( ConstantsDemoConstants.GEN_ME == -3523553, "GEN_ME = -3523553");
        TestBase.Expect( ConstantsDemoConstants.GEn_DUB == 325.532, "GEn_DUB = 325.532");
        TestBase.Expect( ConstantsDemoConstants.GEn_DU == 85.2355, "GEn_DU = 85.2355");
        TestBase.Expect( ConstantsDemoConstants.GEN_STRING == "asldkjasfd", "GEN_STRING = \"asldkjasfd\"");
        TestBase.Expect( ConstantsDemoConstants.e10 == 1e+10, "e10 = 1e+10");
        TestBase.Expect( ConstantsDemoConstants.e11 == -1e+10, "e11 = -1e+10");
        TestBase.Expect( ConstantsDemoConstants.GEN_UUID == "00000000-4444-CCCC-ffff-0123456789ab", "GEN_UUID = \"00000000-4444-CCCC-ffff-0123456789ab\"");

        TestBase.Expect( ConstantsDemoConstants.GEN_MAP.get(35532) == 233, "GEN_MAP.get(35532) == 233");
        TestBase.Expect( ConstantsDemoConstants.GEN_MAP.get(43523) == 853, "GEN_MAP.get(43523) == 853");
        
        TestBase.Expect( ConstantsDemoConstants.GEN_LIST.length == 3, "GEN_LIST.size() == 3");
        TestBase.Expect( ConstantsDemoConstants.GEN_LIST.join("/") == "235235/23598352/3253523", "GEN_LIST elements");

        TestBase.Expect( ConstantsDemoConstants.GEN_MAPMAP.get(235).get(532) == 53255, "GEN_MAPMAP.get(235).get(532) == 53255");
        TestBase.Expect( ConstantsDemoConstants.GEN_MAPMAP.get(235).get(235) == 235, "GEN_MAPMAP.get(235).get(235) == 235");

        TestBase.Expect( ConstantsDemoConstants.GEN_MAP2.get("hello") == 233, "GEN_MAP2.get(\"hello\") == 233");
        TestBase.Expect( ConstantsDemoConstants.GEN_MAP2.get("lkj98d") == 853, "GEN_MAP2.get(\"lkj98d\") == 853");
        TestBase.Expect( ConstantsDemoConstants.GEN_MAP2.get('lkjsdf') == 98325, "GEN_MAP2.get('lkjsdf') == 98325");
        
        TestBase.Expect( ConstantsDemoConstants.GEN_THING.hello == 325, "GEN_THING.hello == 325");
        TestBase.Expect( ConstantsDemoConstants.GEN_THING.goodbye == 325352, "GEN_THING.goodbye == 325352");

        TestBase.Expect( ConstantsDemoConstants.GEN_WHAT.get(35).hello == 325, "GEN_WHAT.get(35).hello == 325");
        TestBase.Expect( ConstantsDemoConstants.GEN_WHAT.get(35).goodbye == 325352, "GEN_WHAT.get(35).goodbye == 325352");

        TestBase.Expect( ConstantsDemoConstants.GEN_SET.size == 2, "GEN_SET.size() == 2");
        TestBase.Expect( ConstantsDemoConstants.GEN_SET.contains(235), "GEN_SET.contains(235)");  // added twice, but this is a set
        TestBase.Expect( ConstantsDemoConstants.GEN_SET.contains(53235), "GEN_SET.contains(53235)");
    }

}


