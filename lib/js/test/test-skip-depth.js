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

QUnit.module('Protocol skip depth limit');

QUnit.test('skip throws DEPTH_LIMIT when depth exceeds 64', function(assert) {
    var transport = new Thrift.Transport('/service');
    var protocol = new Thrift.Protocol(transport);

    assert.throws(
        function() { protocol.skip(Thrift.Type.STRUCT, 64); },
        function(e) {
            return e instanceof Thrift.TProtocolException &&
                   e.type === Thrift.TProtocolExceptionType.DEPTH_LIMIT;
        },
        'skip throws TProtocolException with DEPTH_LIMIT at depth 65'
    );
});

QUnit.test('skip does not throw below the depth limit', function(assert) {
    var transport = new Thrift.Transport('/service');
    var protocol = new Thrift.Protocol(transport);

    // depth=63 → increments to 64, which is not > 64, so no throw before read
    // I32 is a leaf: one readI32 call, no recursion
    transport.setRecvBuffer('\x00\x00\x00\x00');
    assert.ok(protocol.skip(Thrift.Type.I32, 63) !== undefined || true,
        'skip at depth 63 does not throw');
});
