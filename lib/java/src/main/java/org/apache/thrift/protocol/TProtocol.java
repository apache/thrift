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
import java.util.function.IntFunction;
import org.apache.thrift.TException;
import org.apache.thrift.partial.TFieldData;
import org.apache.thrift.scheme.IScheme;
import org.apache.thrift.scheme.StandardScheme;
import org.apache.thrift.transport.TTransport;

/** Protocol interface definition. */
public abstract class TProtocol implements TWriteProtocol, TReadProtocol {

  /** Prevent direct instantiation */
  @SuppressWarnings("unused")
  private TProtocol() {}

  /** Transport */
  protected TTransport trans_;

  /** Constructor */
  protected TProtocol(TTransport trans) {
    trans_ = trans;
  }

  /** Transport accessor */
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
   *     of TType.
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

  public final <T> void writeSet(byte elementType, Set<T> set, WriteCallback<T> callback)
      throws TException {
    writeSetBegin(new TSet(elementType, set.size()));
    for (T t : set) {
      callback.call(t);
    }
    writeSetEnd();
  }

  public final <T> void writeList(byte elementType, List<T> list, WriteCallback<T> callback)
      throws TException {
    writeListBegin(new TList(elementType, list.size()));
    for (T t : list) {
      callback.call(t);
    }
    writeListEnd();
  }

  public final <K, V> void writeMap(
      byte keyType, byte valueType, Map<K, V> map, WriteCallback<Map.Entry<K, V>> callback)
      throws TException {
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

  public final void writeMessage(TMessage message, WriteCallback<Void> callback) throws TException {
    writeMessageBegin(message);
    callback.call(null);
    writeMessageEnd();
  }

  /**
   * read a message by delegating to a callback, handles {@link #readMessageBegin() begin} and
   * {@link #readMessageEnd() end} automatically.
   *
   * @param callback callback for actual reading
   * @param <T> result message type
   * @return the message read
   * @throws TException when any sub-operation failed
   */
  public final <T> T readMessage(ReadCallback<TMessage, T> callback) throws TException {
    TMessage tMessage = readMessageBegin();
    T t = callback.accept(tMessage);
    readMessageEnd();
    return t;
  }

  /**
   * read a struct by delegating to a callback, handles {@link #readStructBegin() begin} and {@link
   * #readStructEnd() end} automatically.
   *
   * @param callback callback for actual reading
   * @param <T> result struct type
   * @return the struct read
   * @throws TException when any sub-operation failed
   */
  public final <T> T readStruct(ReadCallback<TStruct, T> callback) throws TException {
    TStruct tStruct = readStructBegin();
    T t = callback.accept(tStruct);
    readStructEnd();
    return t;
  }

  /**
   * read a field by delegating to a callback, handles {@link #readFieldBegin() begin} and {@link
   * #readFieldEnd() end} automatically, and returns whether the {@link TType#STOP stop signal} was
   * encountered. Because the value is not returned, you (the compiler generated code in most cases)
   * are expected to set the field yourself within the callback.
   *
   * @param callback callback for reading a field
   * @param <T> result field type
   * @return true if a stop signal was encountered, false otherwise
   * @throws Exception when any sub-operation failed
   */
  public final <T> boolean readField(ReadCallback<TField, T> callback) throws Exception {
    TField tField = readFieldBegin();
    if (tField.type == org.apache.thrift.protocol.TType.STOP) {
      return true;
    }
    callback.accept(tField);
    readFieldEnd();
    return false;
  }

  /**
   * read a {@link Map} of elements by delegating to the callback, handles {@link #readMapBegin()
   * begin} and {@link #readMapEnd() end} automatically.
   *
   * @param callback callback for reading the map
   * @param <T> result map type
   * @return the map read
   * @throws TException when any sub-operation fails
   */
  public final <T extends Map<?, ?>> T readMap(ReadCallback<TMap, T> callback) throws TException {
    TMap tMap = readMapBegin();
    T t = callback.accept(tMap);
    readMapEnd();
    return t;
  }

  /**
   * read a {@link Map} of elements by delegating key and value reading to the callback, handles
   * {@link #readMapBegin() begin} and {@link #readMapEnd() end} automatically.
   *
   * @param callback callback for reading keys and values, calls to {@link
   *     ReadMapEntryCallback#getKey()} and {@link ReadMapEntryCallback#getValue()} will be in
   *     alternating orders, i.e. k1, v1, k2, v2, .., k_n, v_n
   * @param <K> key type
   * @param <V> value type
   * @return the map read
   * @throws TException when any sub-operation fails
   */
  public final <K, V> Map<K, V> readMap(ReadMapEntryCallback<K, V> callback) throws TException {
    return readMap(callback, HashMap::new);
  }

  /**
   * read a {@link Map} of elements by delegating key and value reading to the callback, handles
   * {@link #readMapBegin() begin} and {@link #readMapEnd() end} automatically, with a specialized
   * map creator given the size hint.
   *
   * @param callback callback for reading keys and values, calls to {@link
   *     ReadMapEntryCallback#getKey()} and {@link ReadMapEntryCallback#getValue()} will be in
   *     alternating orders, i.e. k1, v1, k2, v2, .., k_n, v_n
   * @param mapCreator map creator given the size hint
   * @param <K> key type
   * @param <V> value type
   * @return the map read
   * @throws TException when any sub-operation fails
   */
  public final <K, V> Map<K, V> readMap(
      ReadMapEntryCallback<K, V> callback, IntFunction<Map<K, V>> mapCreator) throws TException {
    return readMap(
        tMap -> {
          Map<K, V> map = mapCreator.apply(tMap.size);
          for (int i = 0; i < tMap.size; i += 1) {
            map.put(callback.getKey(), callback.getValue());
          }
          return map;
        });
  }

  /**
   * read a {@link List} by delegating to the callback, handles {@link #readListBegin() begin} and
   * {@link #readListEnd() end} automatically.
   *
   * @param callback callback for reading the list
   * @param <T> result list type
   * @return the list read
   * @throws TException when any sub-operation fails
   */
  public final <T extends List<?>> T readList(ReadCallback<TList, T> callback) throws TException {
    TList tList = readListBegin();
    T t = callback.accept(tList);
    readListEnd();
    return t;
  }

  /**
   * read a {@link List} by delegating element reading to the callback, handles {@link
   * #readListBegin() begin} and {@link #readListEnd() end} automatically.
   *
   * @param callback callback for reading one element
   * @param <T> element type
   * @return list of elements read
   * @throws TException when any sub-operation fails
   */
  public final <T> List<T> readList(ReadCollectionCallback<T> callback) throws TException {
    return readList(callback, ArrayList::new);
  }

  /**
   * read a {@link List} by delegating element reading to the callback, handles {@link
   * #readListBegin() begin} and {@link #readListEnd() end} automatically, with a specialized list
   * creator given the size hint.
   *
   * @param callback callback for reading one element
   * @param listCreator list creator given size hint
   * @param <T> element type
   * @return list of elements read
   * @throws TException when any sub-operation fails
   */
  public final <T> List<T> readList(
      ReadCollectionCallback<T> callback, IntFunction<List<T>> listCreator) throws TException {
    return readList(
        tList -> {
          List<T> list = listCreator.apply(tList.size);
          for (int i = 0; i < tList.size; i += 1) {
            list.add(callback.call());
          }
          return list;
        });
  }

  /**
   * read a {@link Set} of elements by delegating to the callback, handles {@link #readSetBegin()
   * begin} and {@link #readSetEnd() end} automatically
   *
   * @param callback callback for reading the set
   * @param <T> result set type
   * @return the set read
   * @throws TException when any sub-operation fails
   */
  public final <T extends Set<?>> T readSet(ReadCallback<TSet, T> callback) throws TException {
    TSet tSet = readSetBegin();
    T t = callback.accept(tSet);
    readSetEnd();
    return t;
  }

  /**
   * read a {@link Set} of elements by delegating element reading to the callback, handles {@link
   * #readSetBegin() begin} and {@link #readSetEnd() end} automatically
   *
   * @param callback callback for reading one element
   * @param <T> element type
   * @return set of elements read
   * @throws TException when any sub-operation fails
   */
  public final <T> Set<T> readSet(ReadCollectionCallback<T> callback) throws TException {
    return readSet(callback, HashSet::new);
  }

  /**
   * read a {@link Set} of elements by delegating element reading to the callback, handles {@link
   * #readSetBegin() begin} and {@link #readSetEnd() end} automatically, with a specialized set
   * creator given the size hint.
   *
   * @param callback callback for reading one elment
   * @param setCreator set creator given size hint
   * @param <T> element type
   * @return set of elements read
   * @throws TException when any sub-operation fails
   */
  public final <T> Set<T> readSet(
      ReadCollectionCallback<T> callback, IntFunction<Set<T>> setCreator) throws TException {
    return readSet(
        tSet -> {
          Set<T> set = setCreator.apply(tSet.size);
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
  public void reset() {}

  /** Scheme accessor */
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
