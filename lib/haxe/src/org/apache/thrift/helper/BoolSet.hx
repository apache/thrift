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

// BoolSet is the Bool-keyed counterpart to IntSet/StringSet.
// ObjectSet<Bool> cannot be used since Bool is a value type that does not satisfy K:{}.

class BoolSet {

    private var _elements = new haxe.ds.IntMap<Int>();
    private var _size : Int = 0;
    public var size(get,never) : Int;

    public function new( values : Array<Bool> = null) {
        if (values != null)
            addRange( values.iterator());
    }

    private static inline function encode( k : Bool) : Int {
        return k ? 1 : 0;
    }

    public function iterator() : Iterator<Bool> {
        return [for (i in _elements.keys()) i != 0].iterator();
    }

    public function traceAll() : Void {
        trace('$_size entries');
        for (entry in this) {
            var yes = contains(entry);
            trace('- $entry, contains() = $yes');
        }
    }

    public function add( o : Bool) : Bool {
        if (_elements.exists( encode(o)))
            return false;
        _size++;
        _elements.set( encode(o), _size);
        return true;
    }

    public function addRange( values : Iterator<Bool>) {
        if (values != null)
            for (value in values)
                add(value);
    }

    public function clear() : Void {
        _elements = new haxe.ds.IntMap<Int>();
        _size = 0;
    }

    public function contains( o : Bool) : Bool {
        return _elements.exists( encode(o));
    }

    public function isEmpty() : Bool {
        return _size == 0;
    }

    public function remove( o : Bool) : Bool {
        if (contains(o)) {
            _elements.remove( encode(o));
            _size--;
            return true;
        }
        return false;
    }

    public function toArray() : Array<Bool> {
        return [for (i in _elements.keys()) i != 0];
    }

    public function get_size() : Int {
        return _size;
    }
}


// EOF
