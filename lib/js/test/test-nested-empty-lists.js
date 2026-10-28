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

// The JSON protocol reads from the parsed message through a stack. An empty
// list leaves an undefined element and the emptied list on it, and readListEnd
// has to remove both before it pushes the next element of the enclosing list,
// or the elements that follow are read from the wrong place.

QUnit.module('JSON protocol nested empty lists');

(function() {
    // The wire form of a list nested "depth" levels deep, with i32 elements.
    function encodeList(values, depth) {
        var wire = [depth === 1 ? 'i32' : 'lst', values.length];
        values.forEach(function(value) {
            wire.push(depth === 1 ? value : encodeList(value, depth - 1));
        });
        return wire;
    }

    // Reads a list the way the generated code does.
    function readList(protocol, depth) {
        var size = protocol.readListBegin().size || 0;
        var values = [];
        for (var i = 0; i < size; i++) {
            values.push(depth === 1 ? protocol.readI32().value : readList(protocol, depth - 1));
        }
        protocol.readListEnd();
        return values;
    }

    // Parses a message whose struct holds the one field given, and reads that
    // field's value with readValue.
    function readField(field, readValue) {
        var transport = new Thrift.Transport('/service');
        transport.setRecvBuffer(JSON.stringify([1, 'm', Thrift.MessageType.CALL, 0, {'1': field}]));
        var protocol = new Thrift.Protocol(transport);
        protocol.readMessageBegin();
        protocol.readStructBegin();
        protocol.readFieldBegin();
        var value = readValue(protocol);
        protocol.readFieldEnd();
        protocol.readStructEnd();
        protocol.readMessageEnd();
        return value;
    }

    [
        {depth: 2, value: [[], [], [1, 2]]},
        {depth: 2, value: [[1], [], [], [2]]},
        {depth: 3, value: [[[1, 2], []], [], [[3]]]}
    ].forEach(function(tc) {
        QUnit.test('list ' + JSON.stringify(tc.value), function(assert) {
            var read = readField({lst: encodeList(tc.value, tc.depth)}, function(protocol) {
                return readList(protocol, tc.depth);
            });
            assert.deepEqual(read, tc.value, JSON.stringify(tc.value));
        });
    });

    // readSetEnd ends a set the way readListEnd ends a list.
    QUnit.test('list of sets [[],[],[5,6]]', function(assert) {
        var value = [[], [], [5, 6]];
        var read = readField({lst: ['set', 3, ['i32', 0], ['i32', 0], ['i32', 2, 5, 6]]}, function(protocol) {
            var size = protocol.readListBegin().size || 0;
            var sets = [];
            for (var i = 0; i < size; i++) {
                var setSize = protocol.readSetBegin().size || 0;
                var set = [];
                for (var j = 0; j < setSize; j++) {
                    set.push(protocol.readI32().value);
                }
                protocol.readSetEnd();
                sets.push(set);
            }
            protocol.readListEnd();
            return sets;
        });
        assert.deepEqual(read, value, JSON.stringify(value));
    });

    // The generated map reader pops one leftover stack entry before every key
    // but the first; the value [[],[]] used to leave two.
    QUnit.test('map values {"0":[[],[]],"1":[[7]]}', function(assert) {
        var value = {'0': [[], []], '1': [[7]]};
        var entries = {'0': encodeList(value['0'], 2), '1': encodeList(value['1'], 2)};
        var read = readField({map: ['i32', 'lst', 2, entries]}, function(protocol) {
            var size = protocol.readMapBegin().size || 0;
            var map = {};
            for (var i = 0; i < size; i++) {
                if (i > 0) {
                    if (protocol.rstack.length > protocol.rpos[protocol.rpos.length - 1] + 1) {
                        protocol.rstack.pop();
                    }
                }
                var key = protocol.readI32().value;
                map[key] = readList(protocol, 2);
            }
            protocol.readMapEnd();
            return map;
        });
        assert.deepEqual(read, value, JSON.stringify(value));
    });
})();

// list<list<string>>, the type of THRIFT-3669, and a list nested one level
// deeper, round-tripped through the code generated from
// JsDeepConstructorTest.thrift.
QUnit.module('Generated nested lists with empty lists');

(function() {
    function roundTrip(obj, Type) {
        var transport = new Thrift.Transport('/service');
        var protocol = new Thrift.Protocol(transport);
        protocol.writeMessageBegin('', 0, 0);
        obj[Symbol.for('write')](protocol);
        protocol.writeMessageEnd();

        var serialized = transport.send_buf;
        transport = new Thrift.Transport('/service');
        transport.setRecvBuffer(serialized);
        protocol = new Thrift.Protocol(transport);
        protocol.readMessageBegin();
        var received = new Type();
        received[Symbol.for('read')](protocol);
        protocol.readMessageEnd();
        return received;
    }

    QUnit.test('list<list<string>>', function(assert) {
        [
            [[], [], ['one', 'two']],
            [['one'], [], ['two'], ['three']]
        ].forEach(function(value) {
            var received = roundTrip(new Complex({list_of_list_field: value}), Complex);
            assert.deepEqual(received.list_of_list_field, value, JSON.stringify(value));
        });
    });

    QUnit.test('list<list<list<string>>>', function(assert) {
        var value = [[['one', 'two'], []], [], [['three']]];
        var received = roundTrip(new Complex({list_of_list_of_list_field: value}), Complex);
        assert.deepEqual(received.list_of_list_of_list_field, value, JSON.stringify(value));
    });
})();
