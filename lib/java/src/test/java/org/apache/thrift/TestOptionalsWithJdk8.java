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

import junit.framework.TestCase;
import thrift.test.optiontypejdk8.Person;

// Tests and documents behavior for the JDK8 "Option<T>" type
public class TestOptionalsWithJdk8 extends TestCase {

    public void testConstruction() {
        Person person = new Person(1L, "name");
        assertFalse(person.getAge().isPresent());
        assertFalse(person.isSetAge());
        assertFalse(person.getPhone().isPresent());
        assertFalse(person.isSetPhone());
        assertEquals(1L, person.getId());
        assertTrue(person.isSetId());
        assertEquals("name", person.getName());
        assertTrue(person.isSetName());

        assertFalse(person.getAddresses().isPresent());
        assertEquals(Integer.valueOf(0), person.getAddressesSize().orElse(0));
        assertFalse(person.getPets().isPresent());
        assertEquals(Integer.valueOf(0), person.getPetsSize().orElse(0));
    }

    public void testEmpty() {
        Person person = new Person();
        person.setPhone("phone");
        assertFalse(person.getAge().isPresent());
        assertFalse(person.isSetAge());
        assertTrue(person.getPhone().isPresent());
        assertEquals("phone", person.getPhone().get());
        assertTrue(person.isSetPhone());
        assertEquals(0L, person.getId());
        assertFalse(person.isSetId());
        assertNull(person.getName());
        assertFalse(person.isSetName());

        assertFalse(person.getAddresses().isPresent());
        assertEquals(Integer.valueOf(0), person.getAddressesSize().orElse(0));
        assertFalse(person.getPets().isPresent());
        assertEquals(Integer.valueOf(0), person.getPetsSize().orElse(0));
    }
}
