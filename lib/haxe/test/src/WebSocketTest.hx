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
import org.apache.thrift.transport.TTransportException;
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
 * may do. Then the server closes the connection after a reply, and close() is
 * called while a call still waits for its reply; after each, open() makes the
 * transport usable again. Exits 0 when every reply reads back as what was sent.
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
                closedByServer(transport, client);
            });
        });

        haxe.Timer.delay(function() {
            check(false, "all replies arrived within 10 s");
            finish();
        }, 10000);
    }

    // The server answers testI32(-1) and then closes the connection.
    static function closedByServer(transport : TSocket, client : ThriftTestImpl) : Void {
        client.testI32(-1, failed("testI32(-1)"), function(r : Int) {
            check(r == -1, 'testI32(-1) returns $r');
            whenClosed(transport, 50, function() {
                check(!transport.isOpen(), "isOpen() is false once the server has closed the connection");
                try {
                    client.testI32(5, failed("testI32(5)"), function(r : Int) {
                        check(false, 'a call on the closed transport returns $r');
                    });
                    check(false, "a call on the closed transport is refused");
                } catch (e : TTransportException) {
                    check(e.errorID == TTransportException.NOT_OPEN, 'a call on the closed transport is refused: ${e.errorMsg}');
                }
                closedWhileWaiting(transport, client);
            });
        });
    }

    static function whenClosed(transport : TSocket, tries : Int, then : Void->Void) : Void {
        if (!transport.isOpen() || (tries == 0)) {
            then();
        } else {
            haxe.Timer.delay(function() { whenClosed(transport, tries - 1, then); }, 20);
        }
    }

    // The server never answers testI32(-2). close() ends that call, and after
    // open() the next reply goes to the call it answers.
    static function closedWhileWaiting(transport : TSocket, client : ThriftTestImpl) : Void {
        transport.open();
        var ended = false;
        client.testI32(-2, function(e : Dynamic) {
            ended = true;
            check(Std.isOfType(e, TTransportException), 'close() ends the waiting call with $e');
        }, function(r : Int) {
            check(false, 'the call waiting at close() returns $r');
            finish();
        });
        transport.close();
        check(ended, "close() ends a call still waiting for its reply");
        transport.open();
        client.testI32(7, failed("testI32(7) after open()"), function(r : Int) {
            check(r == 7, 'testI32(7) after open() returns $r');
            finish();
        });
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
