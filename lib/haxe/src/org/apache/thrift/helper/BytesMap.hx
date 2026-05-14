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

// BytesMap allows mapping of Bytes keys to arbitrary values using content equality.
// ObjectMap<Bytes,V> would use reference equality, which is semantically wrong for
// binary keys — two equal byte arrays at different addresses would be distinct keys.

class BytesMap<T> implements haxe.Constraints.IMap<Bytes, T> {

    private var data : StringMap<T>;

    public function new() {
        data = new StringMap<T>();
    }

    private static inline function encode( k : Bytes) : String {
        return k == null ? "" : k.toHex();
    }

    private static inline function decode( s : String) : Bytes {
        return s.length == 0 ? null : Bytes.ofHex(s);
    }

    public function get( k : Bytes) : Null<T> {
        return data.get( encode(k));
    }

    public function set( k : Bytes, v : T) : Void {
        data.set( encode(k), v);
    }

    public function exists( k : Bytes) : Bool {
        return data.exists( encode(k));
    }

    public function remove( k : Bytes) : Bool {
        return data.remove( encode(k));
    }

    public function keys() : Iterator<Bytes> {
        return [for (s in data.keys()) decode(s)].iterator();
    }

    public function iterator() : Iterator<T> {
        return data.iterator();
    }

    public function keyValueIterator() : KeyValueIterator<Bytes, T> {
        return new BytesMapKVIterator<T>( data);
    }

    public function copy() : haxe.Constraints.IMap<Bytes, T> {
        var retval = new BytesMap<T>();
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
            result += " " + (k == null ? "null" : k.toHex()) + " => " + this.get(k);
        }
        return result + " }";
    }
}


private class BytesMapKVIterator<T> {

    private var inner : KeyValueIterator<String, T>;

    public function new( data : StringMap<T>) {
        inner = data.keyValueIterator();
    }

    public function hasNext() : Bool {
        return inner.hasNext();
    }

    public function next() : {key : Bytes, value : T} {
        var kv = inner.next();
        return {key : kv.key.length == 0 ? null : Bytes.ofHex( kv.key), value : kv.value};
    }
}


// EOF
