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

// Round-trip the recursion-depth limit through the generated read/write code
// (RecTree struct, RecUnion union and RecError exception from
// JsRecursionDepthTest.thrift) rather than exercising the protocol counter in
// isolation.
QUnit.module('Protocol recursion depth limit');

(function() {
    var LIMIT = Thrift.DEFAULT_RECURSION_DEPTH;
    var DEPTH_LIMIT = Thrift.TProtocolExceptionType.DEPTH_LIMIT;
    var WRITE = Symbol.for('write');
    var READ = Symbol.for('read');

    // Build a linear chain "depth" levels deep. A union needs exactly one field
    // set, so inner nodes set "children" and the innermost one sets its leaf.
    function makeChain(Type, depth, leafField) {
        var leafArgs = {};
        leafArgs[leafField] = 1;
        var node = new Type(leafArgs);
        for (var i = 1; i < depth; i++) {
            node = new Type({children: [node]});
        }
        return node;
    }

    function chainDepth(node) {
        var d = 0;
        while (node) {
            d++;
            node = (node.children && node.children.length) ? node.children[0] : null;
        }
        return d;
    }

    // Serialize through the generated writer. When "unbounded" is set the guard
    // is neutralised on this protocol instance only, so we can craft an
    // over-limit payload and feed it back into a fresh (bounded) reader.
    function serialize(obj, unbounded) {
        var transport = new Thrift.Transport('/service');
        var protocol = new Thrift.Protocol(transport);
        if (unbounded) {
            protocol.incrementRecursionDepth = function() {};
            protocol.decrementRecursionDepth = function() {};
        }
        protocol.writeMessageBegin('', 0, 0);
        obj[WRITE](protocol);
        protocol.writeMessageEnd();
        return transport.send_buf;
    }

    function deserialize(serialized, Type) {
        var transport = new Thrift.Transport('/service');
        transport.setRecvBuffer(serialized);
        var protocol = new Thrift.Protocol(transport);
        protocol.readMessageBegin();
        var obj = new Type();
        obj[READ](protocol);
        protocol.readMessageEnd();
        return obj;
    }

    function isDepthLimit(e) {
        return e instanceof Thrift.TProtocolException && e.type === DEPTH_LIMIT;
    }

    [['RecTree', RecTree, 'item'], ['RecUnion', RecUnion, 'leaf'], ['RecError', RecError, 'leaf']].forEach(function(tc) {
        var tname = tc[0], Type = tc[1], leafField = tc[2];

        QUnit.test(tname + ' round-trips a chain at the depth limit', function(assert) {
            var decoded = deserialize(serialize(makeChain(Type, LIMIT, leafField)), Type);
            assert.equal(chainDepth(decoded), LIMIT,
                'decoded chain has the same depth as the original');
        });

        QUnit.test(tname + ' writing past the depth limit throws DEPTH_LIMIT', function(assert) {
            assert.throws(
                function() { serialize(makeChain(Type, LIMIT + 5, leafField)); },
                isDepthLimit,
                'write throws TProtocolException with DEPTH_LIMIT');
        });

        QUnit.test(tname + ' reading past the depth limit throws DEPTH_LIMIT', function(assert) {
            var serialized = serialize(makeChain(Type, LIMIT + 5, leafField), true);
            assert.throws(
                function() { deserialize(serialized, Type); },
                isDepthLimit,
                'read throws TProtocolException with DEPTH_LIMIT');
        });
    });
})();
