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

import haxe.ds.IntMap;

// BoolMap allows mapping of Bool keys to arbitrary values.
// ObjectMap<> cannot be used since Bool is a value type that does not satisfy K:{}.

class BoolMap<T> implements haxe.Constraints.IMap<Bool, T> {

    private var data : IntMap<T>;

    public function new() {
        data = new IntMap<T>();
    }

    private static inline function encode( k : Bool) : Int {
        return k ? 1 : 0;
    }

    public function get( k : Bool) : Null<T> {
        return data.get( encode(k));
    }

    public function set( k : Bool, v : T) : Void {
        data.set( encode(k), v);
    }

    public function exists( k : Bool) : Bool {
        return data.exists( encode(k));
    }

    public function remove( k : Bool) : Bool {
        return data.remove( encode(k));
    }

    public function keys() : Iterator<Bool> {
        return [for (i in data.keys()) i != 0].iterator();
    }

    public function iterator() : Iterator<T> {
        return data.iterator();
    }

    public function keyValueIterator() : KeyValueIterator<Bool, T> {
        return new BoolMapKVIterator<T>( data);
    }

    public function copy() : haxe.Constraints.IMap<Bool, T> {
        var retval = new BoolMap<T>();
        for (k in this.keys())
            retval.set( k, this.get(k));
        return retval;
    }

    public function clear() : Void {
        data = new IntMap<T>();
    }

    public function toString() : String {
        var result = "{";
        var first = true;
        for (k in this.keys()) {
            if (first) first = false; else result += ",";
            result += " " + k + " => " + this.get(k);
        }
        return result + " }";
    }
}


private class BoolMapKVIterator<T> {

    private var inner : KeyValueIterator<Int, T>;

    public function new( data : IntMap<T>) {
        inner = data.keyValueIterator();
    }

    public function hasNext() : Bool {
        return inner.hasNext();
    }

    public function next() : {key : Bool, value : T} {
        var kv = inner.next();
        return {key : kv.key != 0, value : kv.value};
    }
}


// EOF
