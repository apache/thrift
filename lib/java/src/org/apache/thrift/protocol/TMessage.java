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

import org.apache.commons.lang3.builder.HashCodeBuilder;

/**
 * Helper class that encapsulates struct metadata.
 *
 */
public final class TMessage {
  public TMessage() {
    this("", TType.STOP, 0);
  }

  public TMessage(String n, byte t, int s) {
    name = n;
    type = t;
    seqid = s;
  }

  public final String name;
  public final byte type;
  public final int seqid;

  @Override
  public String toString() {
    return "<TMessage name:'" + name + "' type: " + type + " seqid:" + seqid + ">";
  }

  @Override
  public int hashCode() {
    return new HashCodeBuilder()
      .append(name)
      .append(type)
      .append(seqid)
      .toHashCode();
  }

  @Override
  public boolean equals(Object object) {
    if (this == object) {
      return true;
    } else if (!(object instanceof TMessage)) {
      return false;
    }
    TMessage other = (TMessage) object;
    return name.equals(other.name) && type == other.type && seqid == other.seqid;
  }
}
