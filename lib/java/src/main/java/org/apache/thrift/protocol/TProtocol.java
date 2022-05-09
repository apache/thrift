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

package org.apache.thrift.protocol;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.HashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.thrift.TException;
import org.apache.thrift.partial.TFieldData;
import org.apache.thrift.scheme.IScheme;
import org.apache.thrift.scheme.StandardScheme;
import org.apache.thrift.transport.TTransport;

/**
 * Protocol interface definition.
 */
public abstract class TProtocol implements TWriteProtocol, TReadProtocol {

    /**
     * Prevent direct instantiation
     */
    @SuppressWarnings("unused")
    private TProtocol() {
    }

    /**
     * Transport
     */
    protected TTransport trans_;

    /**
     * Constructor
     */
    protected TProtocol(TTransport trans) {
        trans_ = trans;
    }

    /**
     * Transport accessor
     */
    public TTransport getTransport() {
        return trans_;
    }

    protected void checkReadBytesAvailable(TMap map) throws TException {
        long elemSize = getMinSerializedSize(map.keyType) + getMinSerializedSize(map.valueType);
        trans_.checkReadBytesAvailable(map.size * elemSize);
    }

    protected void checkReadBytesAvailable(TList list) throws TException {
        long size = list.getSize();
        trans_.checkReadBytesAvailable(size * getMinSerializedSize(list.elemType));
    }

    protected void checkReadBytesAvailable(TSet set) throws TException {
        long size = set.getSize();
        trans_.checkReadBytesAvailable(size * getMinSerializedSize(set.elemType));
    }

    /**
     * Return min serialized size in bytes
     *
     * @param type Returns the minimum amount of bytes needed to store the smallest possible instance
     *             of TType.
     * @return min serialized size
     * @throws TException when error happens
     */
    public abstract int getMinSerializedSize(byte type) throws TException;

    public interface WriteCallback<T> {
        void call(T e) throws TException;
    }

    public interface ReadCallback<T, R> {
        R accept(T t) throws TException;
    }

    public interface ReadCollectionCallback<R> {
        R call() throws TException;
    }

    public interface ReadMapEntryCallback<K, V> {
        K getKey() throws TException;
        V getValue() throws TException;
    }

    public final <T> void writeSet(byte elementType, Set<T> set, WriteCallback<T> callback) throws TException {
        writeSetBegin(new TSet(elementType, set.size()));
        for (T t : set) {
            callback.call(t);
        }
        writeSetEnd();
    }

    public final <T> void writeList(byte elementType, List<T> list, WriteCallback<T> callback) throws TException {
        writeListBegin(new TList(elementType, list.size()));
        for (T t : list) {
            callback.call(t);
        }
        writeListEnd();
    }

    public final <K, V> void writeMap(byte keyType, byte valueType, Map<K, V> map, WriteCallback<Map.Entry<K, V>> callback) throws TException {
        writeMapBegin(new TMap(keyType, valueType, map.size()));
        for (Map.Entry<K, V> entry : map.entrySet()) {
            callback.call(entry);
        }
        writeMapEnd();
    }

    public final void writeField(TField field, WriteCallback<Void> callback) throws TException {
        writeFieldBegin(field);
        callback.call(null);
        writeFieldEnd();
    }

    public final void writeStruct(TStruct struct, WriteCallback<Void> callback) throws TException {
        writeStructBegin(struct);
        callback.call(null);
        writeStructEnd();
    }

    public final <T> T readMessage(ReadCallback<TMessage, T> callback) throws TException {
        TMessage tMessage = readMessageBegin();
        T t = callback.accept(tMessage);
        readMessageEnd();
        return t;
    }

    public final <T> T readStruct(ReadCallback<TStruct, T> callback) throws TException {
        TStruct tStruct = readStructBegin();
        T t = callback.accept(tStruct);
        readStructEnd();
        return t;
    }

    public final <T> boolean readField(ReadCallback<TField, T> callback) throws Exception {
        TField tField = readFieldBegin();
        if (tField.type == org.apache.thrift.protocol.TType.STOP) {
            return true;
        }
        callback.accept(tField);
        readFieldEnd();
        return false;
    }

    public final <T> T readMap(ReadCallback<TMap, T> callback) throws TException {
        TMap tMap = readMapBegin();
        T t = callback.accept(tMap);
        readMapEnd();
        return t;
    }

    public final <K, V> Map<K, V> readMap(ReadMapEntryCallback<K, V> callback) throws TException {
        return readMap(tMap -> {
            Map<K, V> map = new HashMap<>(tMap.size);
            for (int i = 0; i < tMap.size; i += 1) {
                map.put(callback.getKey(), callback.getValue());
            }
            return map;
        });
    }

    public final <T> T readList(ReadCallback<TList, T> callback) throws TException {
        TList tList = readListBegin();
        T t = callback.accept(tList);
        readListEnd();
        return t;
    }

    public final <T> List<T> readList(ReadCollectionCallback<T> callback) throws TException {
        return readList(tList -> {
            List<T> list = new ArrayList<>(tList.size);
            for (int i = 0; i < tList.size; i += 1) {
                list.add(callback.call());
            }
            return list;
        });
    }

    public final <T> T readSet(ReadCallback<TSet, T> callback) throws TException {
        TSet tSet = readSetBegin();
        T t = callback.accept(tSet);
        readSetEnd();
        return t;
    }

    public final <T> Set<T> readSet(ReadCollectionCallback<T> callback) throws TException {
        return readSet(tSet -> {
            Set<T> set = new HashSet<>(tSet.size);
            for (int i = 0; i < tSet.size; i += 1) {
                set.add(callback.call());
            }
            return set;
        });
    }

    /**
     * Reset any internal state back to a blank slate. This method only needs to be implemented for
     * stateful protocols.
     */
    public void reset() {
    }

    /**
     * Scheme accessor
     */
    public Class<? extends IScheme> getScheme() {
        return StandardScheme.class;
    }

    // -----------------------------------------------------------------
    // Additional methods to improve performance.

    public int readFieldBeginData() throws TException {
        // Derived classes should provide a more efficient version of this
        // method if allowed by the encoding used by that protocol.
        TField tfield = this.readFieldBegin();
        return TFieldData.encode(tfield.type, tfield.id);
    }

    public void skip(byte fieldType) throws TException {
        this.skip(fieldType, Integer.MAX_VALUE);
    }

    public void skip(byte fieldType, int maxDepth) throws TException {
        if (maxDepth <= 0) {
            throw new TException("Maximum skip depth exceeded");
        }

        switch (fieldType) {
            case TType.BOOL:
                this.skipBool();
                break;

            case TType.BYTE:
                this.skipByte();
                break;

            case TType.I16:
                this.skipI16();
                break;

            case TType.I32:
                this.skipI32();
                break;

            case TType.I64:
                this.skipI64();
                break;

            case TType.DOUBLE:
                this.skipDouble();
                break;

            case TType.STRING:
                this.skipBinary();
                break;

            case TType.STRUCT:
                this.readStructBegin();
                while (true) {
                    int tfieldData = this.readFieldBeginData();
                    byte tfieldType = TFieldData.getType(tfieldData);
                    if (tfieldType == TType.STOP) {
                        break;
                    }
                    this.skip(tfieldType, maxDepth - 1);
                    this.readFieldEnd();
                }
                this.readStructEnd();
                break;

            case TType.MAP:
                TMap map = this.readMapBegin();
                for (int i = 0; i < map.size; i++) {
                    this.skip(map.keyType, maxDepth - 1);
                    this.skip(map.valueType, maxDepth - 1);
                }
                this.readMapEnd();
                break;

            case TType.SET:
                TSet set = this.readSetBegin();
                for (int i = 0; i < set.size; i++) {
                    this.skip(set.elemType, maxDepth - 1);
                }
                this.readSetEnd();
                break;

            case TType.LIST:
                TList list = this.readListBegin();
                for (int i = 0; i < list.size; i++) {
                    this.skip(list.elemType, maxDepth - 1);
                }
                this.readListEnd();
                break;

            default:
                throw new TProtocolException(
                    TProtocolException.INVALID_DATA, "Unrecognized type " + fieldType);
        }
    }

    /**
     * The default implementation of all skip() methods calls the corresponding read() method.
     * Protocols that derive from this class are strongly encouraged to provide a more efficient
     * alternative.
     */
    protected void skipBool() throws TException {
        this.readBool();
    }

    protected void skipByte() throws TException {
        this.readByte();
    }

    protected void skipI16() throws TException {
        this.readI16();
    }

    protected void skipI32() throws TException {
        this.readI32();
    }

    protected void skipI64() throws TException {
        this.readI64();
    }

    protected void skipDouble() throws TException {
        this.readDouble();
    }

    protected void skipBinary() throws TException {
        this.readBinary();
    }

    static final int MAX_SKIPPED_BYTES = 256;
    protected byte[] skippedBytes = new byte[MAX_SKIPPED_BYTES];

    protected void skipBytes(int numBytes) throws TException {
        if (numBytes <= MAX_SKIPPED_BYTES) {
            if (this.getTransport().getBytesRemainingInBuffer() >= numBytes) {
                this.getTransport().consumeBuffer(numBytes);
            } else {
                this.getTransport().readAll(skippedBytes, 0, numBytes);
            }
        } else {
            int remaining = numBytes;
            while (remaining > 0) {
                skipBytes(Math.min(remaining, MAX_SKIPPED_BYTES));
                remaining -= MAX_SKIPPED_BYTES;
            }
        }
    }
}
