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

struct Simple {
  1: string value
}

struct Complex {
  1: Simple struct_field
  2: list<Simple> struct_list_field
  3: set<Simple> struct_set_field
  4: map<string,Simple> struct_map_field
  5: list<set<map<string,list<Simple>>>> struct_nested_containers_field
  6: map<string, list<map<string,Simple>> > struct_nested_containers_field2
  7: list<list<string>> list_of_list_field
  8: list<list<list<string>>> list_of_list_of_list_field
}

struct ComplexList {
  1: list<Complex> struct_list_field;
}
