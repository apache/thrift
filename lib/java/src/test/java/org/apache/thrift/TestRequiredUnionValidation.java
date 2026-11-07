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
package org.apache.thrift;

import static org.junit.jupiter.api.Assertions.assertDoesNotThrow;
import static org.junit.jupiter.api.Assertions.assertThrows;

import org.apache.thrift.protocol.TProtocolException;
import org.junit.jupiter.api.Test;
import thrift.test.requnion.RequiredUnionChoice;
import thrift.test.requnion.RequiredUnionHolder;

/** THRIFT-735: a required union field must hold a union that has a field set. */
public class TestRequiredUnionValidation {

  private static RequiredUnionHolder valid() {
    RequiredUnionHolder holder = new RequiredUnionHolder();
    holder.setChoice(RequiredUnionChoice.number(1));
    holder.setAliased(RequiredUnionChoice.text("x"));
    return holder;
  }

  @Test
  public void testSetUnionsValidate() {
    assertDoesNotThrow(() -> valid().validate());
  }

  @Test
  public void testMissingUnionStillRejected() {
    RequiredUnionHolder holder = valid();
    holder.setChoice(null);
    assertThrows(TProtocolException.class, holder::validate);
  }

  @Test
  public void testUnsetRequiredUnionRejected() {
    RequiredUnionHolder holder = valid();
    holder.setChoice(new RequiredUnionChoice());
    assertThrows(TProtocolException.class, holder::validate);
  }

  @Test
  public void testUnsetRequiredTypedefUnionRejected() {
    RequiredUnionHolder holder = valid();
    holder.setAliased(new RequiredUnionChoice());
    assertThrows(TProtocolException.class, holder::validate);
  }

  @Test
  public void testUnsetOptionalUnionAccepted() {
    RequiredUnionHolder holder = valid();
    holder.setMaybe(new RequiredUnionChoice());
    assertDoesNotThrow(holder::validate);
  }
}
