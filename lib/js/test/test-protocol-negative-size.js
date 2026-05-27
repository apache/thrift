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

QUnit.module('Protocol negative size rejection');

function isNegativeSizeError(e) {
    return e instanceof Thrift.TProtocolException &&
           e.type === Thrift.TProtocolExceptionType.NEGATIVE_SIZE;
}

QUnit.test('readMapBegin rejects negative size', function(assert) {
    var transport = new Thrift.Transport('/service');
    var protocol = new Thrift.Protocol(transport);
    // rstack entry: [ktype, vtype, size, content]
    protocol.rstack = [['i32', 'string', -1, {}]];
    protocol.rpos = [];

    assert.throws(
        function() { protocol.readMapBegin(); },
        isNegativeSizeError,
        'readMapBegin throws NEGATIVE_SIZE for size=-1'
    );
});

QUnit.test('readListBegin rejects negative size', function(assert) {
    var transport = new Thrift.Transport('/service');
    var protocol = new Thrift.Protocol(transport);
    // rstack entry: [etype, size, ...content]
    protocol.rstack = [['string', -1]];
    protocol.rpos = [];

    assert.throws(
        function() { protocol.readListBegin(); },
        isNegativeSizeError,
        'readListBegin throws NEGATIVE_SIZE for size=-1'
    );
});
