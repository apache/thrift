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

package org.apache.thrift.helper;

import haxe.io.Bytes;

// BytesSet is the Bytes-keyed counterpart to StringSet, using content equality.
// ObjectSet<Bytes> would use reference equality, which is semantically wrong — two
// equal byte arrays at different addresses would be treated as distinct elements.

class BytesSet {

    private var _elements = new haxe.ds.StringMap<Int>();
    private var _size : Int = 0;
    public var size(get,never) : Int;

    public function new( values : Array<Bytes> = null) {
        if (values != null)
            addRange( values.iterator());
    }

    private static inline function encode( k : Bytes) : String {
        return k == null ? "" : k.toHex();
    }

    private static inline function decode( s : String) : Bytes {
        return s.length == 0 ? null : Bytes.ofHex(s);
    }

    public function iterator() : Iterator<Bytes> {
        return [for (s in _elements.keys()) decode(s)].iterator();
    }

    public function traceAll() : Void {
        trace('$_size entries');
        for (entry in this) {
            var yes = contains(entry);
            trace('- ' + (entry == null ? "null" : entry.toHex()) + ', contains() = $yes');
        }
    }

    public function add( o : Bytes) : Bool {
        var k = encode(o);
        if (_elements.exists(k))
            return false;
        _size++;
        _elements.set( k, _size);
        return true;
    }

    public function addRange( values : Iterator<Bytes>) {
        if (values != null)
            for (value in values)
                add(value);
    }

    public function clear() : Void {
        _elements = new haxe.ds.StringMap<Int>();
        _size = 0;
    }

    public function contains( o : Bytes) : Bool {
        return _elements.exists( encode(o));
    }

    public function isEmpty() : Bool {
        return _size == 0;
    }

    public function remove( o : Bytes) : Bool {
        var k = encode(o);
        if (_elements.exists(k)) {
            _elements.remove(k);
            _size--;
            return true;
        }
        return false;
    }

    public function toArray() : Array<Bytes> {
        return [for (s in _elements.keys()) decode(s)];
    }

    public function get_size() : Int {
        return _size;
    }
}


// EOF
