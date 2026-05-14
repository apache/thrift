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

import haxe.ds.StringMap;
import haxe.io.Bytes;

// FloatMap allows mapping of Float keys to arbitrary values.
// ObjectMap<> cannot be used since Float is a value type that does not satisfy K:{}.
// Keys are stored as the 8-byte IEEE 754 bit pattern in hex, which gives exact
// equality semantics and correctly round-trips NaN, -0.0, and infinities.

class FloatMap<T> implements haxe.Constraints.IMap<Float, T> {

    private var data : StringMap<T>;

    public function new() {
        data = new StringMap<T>();
    }

    private static function encode( k : Float) : String {
        var b = Bytes.alloc(8);
        b.setDouble( 0, k);
        return b.toHex();
    }

    private static function decode( s : String) : Float {
        return Bytes.ofHex(s).getDouble(0);
    }

    public function get( k : Float) : Null<T> {
        return data.get( encode(k));
    }

    public function set( k : Float, v : T) : Void {
        data.set( encode(k), v);
    }

    public function exists( k : Float) : Bool {
        return data.exists( encode(k));
    }

    public function remove( k : Float) : Bool {
        return data.remove( encode(k));
    }

    public function keys() : Iterator<Float> {
        return [for (s in data.keys()) decode(s)].iterator();
    }

    public function iterator() : Iterator<T> {
        return data.iterator();
    }

    public function keyValueIterator() : KeyValueIterator<Float, T> {
        return new FloatMapKVIterator<T>( data);
    }

    public function copy() : haxe.Constraints.IMap<Float, T> {
        var retval = new FloatMap<T>();
        for (k in this.keys())
            retval.set( k, this.get(k));
        return retval;
    }

    public function clear() : Void {
        data = new StringMap<T>();
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


private class FloatMapKVIterator<T> {

    private var inner : KeyValueIterator<String, T>;

    public function new( data : StringMap<T>) {
        inner = data.keyValueIterator();
    }

    public function hasNext() : Bool {
        return inner.hasNext();
    }

    public function next() : {key : Float, value : T} {
        var kv = inner.next();
        return {key : Bytes.ofHex( kv.key).getDouble(0), value : kv.value};
    }
}


// EOF
