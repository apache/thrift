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

#if sys

import tests.TestBase;

import org.apache.thrift.TConfiguration;
import org.apache.thrift.protocol.*;
import org.apache.thrift.transport.*;


// TProtocolUtil.skip() is handed the wire type of every field a reader does not
// recognise (generated code calls it for unknown field ids and the default
// field-type arm). It must handle every type that can appear on the wire,
// including UUID (TType.UUID = 16). A UUID field used to fall through skip()'s
// default arm and throw TProtocolException(UNKNOWN), desyncing the read; it is
// now consumed in the protocol-correct form (prot.readUuid()), like Java and Go.
class SkipTest extends tests.TestBase {

    public static function SkipsUuidField() : Void {
        var cfg = new TConfiguration();
        var stream = new TMemoryStream();
        var trans : TTransport = new TStreamTransport( stream, stream, cfg);
        var proto = new TBinaryProtocol( trans);

        // A uuid, then a sentinel i32 standing in for the next field on the wire.
        proto.writeUuid( "00112233-4455-6677-8899-aabbccddeeff");
        proto.writeI32( 4711);

        stream.Position = 0;  // rewind to read back what was written

        TProtocolUtil.skip( proto, TType.UUID);  // must consume exactly the 16 uuid bytes
        var sentinel = proto.readI32();

        tests.TestBase.Expect( sentinel == 4711,
            "skip(UUID) consumes exactly the uuid, leaving the following value readable");
    }

    public static function Run( server : Bool) : Void {
        SkipsUuidField();
    }
}

#end
