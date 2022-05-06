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

/**
 * Holds the type and id members of a {@link org.apache.thrift.protocol.TField} into a single int.
 *
 * <p>This encoding scheme obviates the need to instantiate TField during the partial
 * deserialization process.
 */
public class TFieldData {
  public static int encode(byte type) {
    return type & 0xff;
  }

  public static int encode(byte type, short id) {
    return (type & 0xff) | (((int) id) << 8);
  }

  public static byte getType(int data) {
    return (byte) (0xff & data);
  }

  public static short getId(int data) {
    return (short) ((0xffff00 & data) >> 8);
  }
}
