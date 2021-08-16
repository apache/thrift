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
import org.apache.thrift.protocol.TBinaryProtocol;
import org.apache.thrift.protocol.TProtocol;
import org.apache.thrift.protocol.TType;

import java.io.Serializable;

/**
 * Enables partial deserialization of binary-encoded thrift objects.
 *
 * This class is meant to be a helper class for {@link PartialThriftDeserializer}.
 * It cannot be used separately on its own.
 */
public class PartialThriftBinaryProtocol extends PartialThriftProtocol implements Serializable {

  public PartialThriftBinaryProtocol() {
  }

  @Override
  protected TProtocol createProtocol() {
    return new TBinaryProtocol(transport);
  }

  // -----------------------------------------------------------------
  // Additional methods to improve performance.

  @Override
  public int readFieldBeginData() throws TException {
    byte type = readByte();
    if (type == TType.STOP) {
      return TFieldData.encode(type);
    }

    short id = readI16();
    return TFieldData.encode(type, id);
  }
}
