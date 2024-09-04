/*
 * Licensed to the Apache Software Foundation (ASF) under one
 * or more contributor license agreements.  See the NOTICE file
 * distributed with this work for additional information
 * regarding copyright ownership.  The ASF licenses this file
 * to you under the Apache License, Version 2.0 (the
 * "License"); you may not use this file except in compliance
 * with the License.  You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing,
 * software distributed under the License is distributed on an
 * "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
 * KIND, either express or implied.  See the License for the
 * specific language governing permissions and limitations
 * under the License.
 */

package org.apache.thrift

import kotlin.test.assertEquals
import org.apache.thrift.kotlin.annotation.test.Person
import org.apache.thrift.meta_data.FieldMetaData
import org.junit.jupiter.api.Test

internal class MetaDataTest {
    @Test
    internal fun testAnnotation() {
        val personMetadata = FieldMetaData.getStructMetaDataMap(Person::class.java)
        assertEquals(3, personMetadata.size)
        val idField = personMetadata[Person._Fields.ID]!!
        assertEquals("id", idField.fieldName)
        assertEquals(mapOf("max" to "100000", "min" to "1"), idField.fieldAnnotations)
    }
}
