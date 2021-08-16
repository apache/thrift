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

package org.apache.thrift.partial;

import org.apache.thrift.TBase;
import org.apache.thrift.TDeserializer;
import org.apache.thrift.TException;
import org.apache.thrift.TSerializer;
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TCompactProtocol;

public class ThriftSerDe {
  private TSerializer binarySerializer;
  private TSerializer compactSerializer;
  private TDeserializer binaryDeserializer;
  private TDeserializer compactDeserializer;

  public ThriftSerDe() throws TException {
    this.binarySerializer = new TSerializer(new TBinaryProtocol.Factory());
    this.compactSerializer = new TSerializer(new TCompactProtocol.Factory());
    this.binaryDeserializer = new TDeserializer(new TBinaryProtocol.Factory());
    this.compactDeserializer = new TDeserializer(new TCompactProtocol.Factory());
  }

  public byte[] serializeBinary(TBase obj) throws TException {
    return binarySerializer.serialize(obj);
  }

  public byte[] serializeCompact(TBase obj) throws TException {
    return compactSerializer.serialize(obj);
  }

  public <T extends TBase> T deserializeBinary(byte[] bytes, Class<T> clazz) throws TException {
    T instance = this.newInstance(clazz);
    binaryDeserializer.deserialize(instance, bytes);
    return clazz.cast(instance);
  }

  public <T extends TBase> T deserializeCompact(byte[] bytes, Class<T> clazz) throws TException {
    T instance = this.newInstance(clazz);
    compactDeserializer.deserialize(instance, bytes);
    return clazz.cast(instance);
  }

  private <T extends TBase> T newInstance(Class<T> clazz) {
    T instance = null;
    try {
      instance = clazz.newInstance();
    } catch (InstantiationException e) {
    } catch (IllegalAccessException e) {
    }
    return clazz.cast(instance);
  }
}
