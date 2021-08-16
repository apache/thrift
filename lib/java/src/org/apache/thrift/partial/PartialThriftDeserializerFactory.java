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

import org.apache.thrift.partial.Validate;

import org.apache.thrift.TBase;
import org.apache.thrift.TException;

import java.util.Collection;
import java.util.List;

/**
 * Factory methods for creating instances of {@link PartialThriftDeserializer}.
 */
public class PartialThriftDeserializerFactory {

  /**
   * Creates an instance of {@link PartialThriftDeserializer} that supports
   * deserialization of data serialized using {@link org.apache.thrift.protocol.TBinaryProtocol}.
   *
   * @param thriftClass Class type of the deserialized object.
   * @param fieldNames Collection of names of fields to deserialize.
   * @return An instance of {@code PartialThriftDeserializer}.
   */
  public static <T extends TBase> PartialThriftDeserializer createBinary(
      Class<T> thriftClass,
      Collection<String> fieldNames) {

    return create(
        thriftClass,
        fieldNames,
        new ThriftStructProcessor(),
        new PartialThriftBinaryProtocol());
  }

  /**
   * Creates an instance of {@link PartialThriftDeserializer} that supports
   * deserialization of data serialized using {@link org.apache.thrift.protocol.TCompactProtocol}.
   *
   * @param thriftClass Class type of the deserialized object.
   * @param fieldNames Collection of names of fields to deserialize.
   * @return An instance of {@code PartialThriftDeserializer}.
   */
  public static <T extends TBase> PartialThriftDeserializer createCompact(
      Class<T> thriftClass,
      Collection<String> fieldNames) {

    return create(
        thriftClass,
        fieldNames,
        new ThriftStructProcessor(),
        new PartialThriftCompactProtocol());
  }

  public static <T extends TBase> PartialThriftDeserializer create(
      Class<T> thriftClass,
      Collection<String> fieldNames,
      ThriftFieldValueProcessor processor,
      PartialThriftProtocol protocol) {

    Validate.checkNotNull(thriftClass, "thriftClass");
    Validate.checkNotNull(fieldNames, "fieldNames");
    Validate.checkNotNull(processor, "processor");
    Validate.checkNotNull(protocol, "protocol");

    List<ThriftField> fields = ThriftField.fromNames(fieldNames);
    ThriftMetadata.ThriftStruct metadata =
        ThriftMetadata.ThriftStruct.fromFields(thriftClass, fields);
    return new PartialThriftDeserializer(metadata, processor, protocol);
  }
}
