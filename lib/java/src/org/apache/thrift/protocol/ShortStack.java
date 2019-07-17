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

import java.util.Arrays;

/**
 * ShortStack is a short-specific Stack implementation written for the express
 * purpose of very fast operations on TCompactProtocol's field id stack. This
 * implementation performs at least 10x faster than java.util.Stack.
 */
class ShortStack {

  private short[] vector;

  /** Always points to the next location */
  private int top = 0;

  public ShortStack(int initialCapacity) {
    vector = new short[initialCapacity];
  }

  public short pop() {
    return vector[--top];
  }

  public void push(short pushed) {
    if (vector.length == top) {
      grow();
    }
    vector[top++] = pushed;
  }

  private void grow() {
    vector = Arrays.copyOf(vector, vector.length << 1);
  }

  public void clear() {
    top = 0;
  }

  @Override
  public String toString() {
    StringBuilder sb = new StringBuilder();
    sb.append("<ShortStack vector:[");
    for (int i = 0; i < vector.length; i++) {
      boolean isTop = (i == (top - 1));
      short value = vector[i];
      if (i != 0) {
        sb.append(' ');
      }
      if (isTop) {
        sb.append(">>").append(value).append("<<");
      } else {
        sb.append(value);
      }
    }
    sb.append("]>");
    return sb.toString();
  }
}
