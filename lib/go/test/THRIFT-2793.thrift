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
 *
 * Contains some contributions under the Thrift Software License.
 * Please see doc/old-thrift-license.txt in the Thrift distribution for
 * details.
 */

/*
 * Test case for THRIFT-2793
 *
 * Without the patch for THRIFT-2793 the thrift compiler
 * for Go produces invalid code for the structs below.
 */
struct THRIFT_2793_A { 1: set<THRIFT_2793_B> b }
struct THRIFT_2793_B { 1: i64 id }
struct THRIFT_2793_C { 1: list<THRIFT_2793_D> d }
struct THRIFT_2793_D { 1: i64 id }

