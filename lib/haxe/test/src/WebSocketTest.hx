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
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.transport.TSocket;
import thrift.test.ThriftTestImpl;

/**
 * TSocket on the js target talks WebSocket. websocket.mjs runs this against a
 * WebSocket server that answers each ThriftTest call with its own argument:
 *
 *   haxe --cwd . websocket.hxml
 *   node websocket.mjs bin/WebSocketTest.js
 *
 * The first call goes out right after open(), before the connection is up,
 * and two calls are made before either reply has arrived, as an application
 * may do. Exits 0 when every reply reads back as what was sent.
 */
class WebSocketTest {

    static var failures = 0;

    static function main() {
        var argv : Array<String> = untyped process.argv;
        var transport = new TSocket(argv[2], 0);  // the js target takes a ws:// URL
        transport.open();
        var client = new ThriftTestImpl(new TBinaryProtocol(transport));

        var text = "WebSocket héllo";
        var i64 = Int64.make(0x7fffffff, 0xfffffffe);

        client.testString(text, failed("testString"), function(r : String) {
            check(r == text, 'testString returns "$r"');

            // Both replies are awaited; each callback gets its own.
            client.testI32(-123456789, failed("testI32"), function(r : Int) {
                check(r == -123456789, 'testI32 returns $r');
            });
            client.testI64(i64, failed("testI64"), function(r : Int64) {
                check(Int64.compare(r, i64) == 0, 'testI64 returns ${Int64.toStr(r)}');
                finish();
            });
        });

        haxe.Timer.delay(function() {
            check(false, "all replies arrived within 10 s");
            finish();
        }, 10000);
    }

    static function failed(call : String) : Dynamic->Void {
        return function(e : Dynamic) {
            check(false, '$call: $e');
            finish();
        };
    }

    static function check(ok : Bool, what : String) : Void {
        trace('${ok ? "OK" : "FAILED"}: $what');
        if (!ok) {
            ++failures;
        }
    }

    static function finish() : Void {
        trace(failures == 0 ? "All WebSocket tests passed." : '$failures WebSocket test(s) failed.');
        untyped process.exit(failures == 0 ? 0 : 1);
    }
}
