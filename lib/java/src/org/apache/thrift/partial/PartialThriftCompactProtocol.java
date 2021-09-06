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

import org.apache.thrift.TException;
import org.apache.thrift.protocol.TCompactProtocol;
import org.apache.thrift.protocol.TField;
import org.apache.thrift.protocol.TProtocol;

import java.io.Serializable;

/**
 * Enables partial deserialization of compact-encoded thrift objects.
 *
 * This class is meant to be a helper class for {@link PartialThriftDeserializer}.
 * It cannot be used separately on its own.
 */
public class PartialThriftCompactProtocol extends PartialThriftProtocol implements Serializable {

  public PartialThriftCompactProtocol() {
  }

  @Override
  protected TProtocol createProtocol() {
    return new TCompactProtocol(transport);
  }

  // -----------------------------------------------------------------
  // Additional methods to improve performance.

  @Override
  public int readFieldBeginData() throws TException {
    // Having to call readFieldBegin() to compute TFieldData really results in lower
    // performance. However, readFieldBegin() accesses some private vars that this method
    // does not have access to.
    // TODO: make changes to TCompactProtocol to allow better performance of this method.

    TField tfield = readFieldBegin();
    return TFieldData.encode(tfield.type, tfield.id);
  }

  @Override
  protected void skipBinary() throws TException {
    int size = intToZigZag(readI32());
    this.skipBytes(size);
  }

  // -------------------------------------------------------
  // Implementing skip for the following methods is tricky (but not impossible).
  // For now, we call the corresponding read() method.

  @Override
  protected void skipBool() throws TException {
    this.readBool();
  }

  @Override
  protected void skipI16() throws TException {
    this.readI16();
  }

  @Override
  protected void skipI32() throws TException {
    this.readI32();
  }

  @Override
  protected void skipI64() throws TException {
    this.readI64();
  }
  // -------------------------------------------------------

  private int intToZigZag(int n) {
    return (n << 1) ^ (n >> 31);
  }
}
