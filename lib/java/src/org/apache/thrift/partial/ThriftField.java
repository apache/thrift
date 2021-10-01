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

import java.util.ArrayList;
import java.util.Collection;
import java.util.Collections;
import java.util.List;

/**
 * Holds name of a thrift field and of its sub-fields recursively.
 * <p>
 * This class is meant to be used in conjunction with {@code TDeserializer}.
 */
public class ThriftField {

  /**
   * Name of this field as it appears in a thrift file. Case sensitive.
   */
  public final String name;

  /**
   * List of sub-fields of this field.
   *
   * This list should have only those sub-fields that need to be deserialized
   * by the {@code TDeserializer}.
   */
  public final List<ThriftField> fields;

  /**
   * Constructs a {@link ThriftField}.
   *
   * @param name the name of this field as it appears in a thrift file. Case sensitive.
   * @param fields List of sub-fields of this field.
   */
  ThriftField(String name, List<ThriftField> fields) {
    Validate.checkNotNullAndNotEmpty(name, "name");
    Validate.checkNotNull(fields, "fields");

    this.name = name;
    this.fields = Collections.unmodifiableList(fields);
  }

  /**
   * Constructs a {@link ThriftField} that does not have any sub-fields.
   */
  ThriftField(String name) {
    this(name, Collections.emptyList());
  }

  // Internal-only constructor that does not mark fields as read-only.
  // That allows fromNames() to construct fields from names.
  // The actual value of allowFieldAdds is ignored.
  // It is used only for generating a different function signature.
  ThriftField(String name, List<ThriftField> fields, boolean allowFieldAdds) {
    Validate.checkNotNullAndNotEmpty(name, "name");
    Validate.checkNotNull(fields, "fields");

    this.name = name;
    this.fields = fields;
  }

  private int hashcode = 0;

  @Override
  public int hashCode() {
    if (this.hashcode == 0) {
      int hc = this.name.toLowerCase().hashCode();
      for (ThriftField subField : this.fields) {
        hc ^= subField.hashCode();
      }

      this.hashcode = hc;
    }

    return this.hashcode;
  }

  @Override
  public boolean equals(Object o) {
    if (o == null) {
      return false;
    }

    if (!(o instanceof ThriftField)) {
      return false;
    }

    ThriftField other = (ThriftField) o;

    if (!this.name.equalsIgnoreCase(other.name)) {
      return false;
    }

    if (this.fields.size() != other.fields.size()) {
      return false;
    }

    for (int i = 0; i < this.fields.size(); i++) {
      if (!this.fields.get(i).equals(other.fields.get(i))) {
        return false;
      }
    }

    return true;
  }

  @Override
  public String toString() {
    return String.join(", ", this.getFieldNames());
  }

  public List<String> getFieldNames() {
    List<String> fieldsList = new ArrayList<>();
    if (this.fields.size() == 0) {
      fieldsList.add(this.name);
    } else {
      for (ThriftField f : this.fields) {
        for (String subF : f.getFieldNames()) {
          fieldsList.add(this.name + "." + subF);
        }
      }
    }

    return fieldsList;
  }

  /**
   * Generates and returns n-ary tree of fields and their sub-fields.
   * <p>
   * @param fieldNames collection of fully qualified field names.
   *
   *        for example,
   *        In case of PinJoin thrift struct, the following are valid field names
   *        -- signature
   *        -- pins.user.userId
   *        -- textSignal.termSignal.termDataMap
   *
   * @return n-ary tree of fields and their sub-fields.
   */
  public static List<ThriftField> fromNames(Collection<String> fieldNames) {
    Validate.checkNotNullAndNotEmpty(fieldNames, "fieldNames");

    List<String> fieldNamesList = new ArrayList<>(fieldNames);
    Collections.sort(fieldNamesList, String.CASE_INSENSITIVE_ORDER);

    List<ThriftField> fields = new ArrayList<>();

    for (String fieldName : fieldNamesList) {
      List<ThriftField> tfields = fields;
      String[] tokens = fieldName.split("\\.");

      for (String token : tokens) {
        ThriftField field = findField(token, tfields);
        if (field == null) {
          field = new ThriftField(token, new ArrayList<>(), true);
          tfields.add(field);
        }
        tfields = field.fields;
      }
    }

    return makeReadOnly(fields);
  }

  private static ThriftField findField(String name, List<ThriftField> fields) {
    for (ThriftField field : fields) {
      if (field.name.equalsIgnoreCase(name)) {
        return field;
      }
    }
    return null;
  }

  private static List<ThriftField> makeReadOnly(List<ThriftField> fields) {
    List<ThriftField> result = new ArrayList<>(fields.size());
    for (ThriftField field : fields) {
      ThriftField copy = new ThriftField(field.name, makeReadOnly(field.fields));
      result.add(copy);
    }
    return Collections.unmodifiableList(result);
  }
}
