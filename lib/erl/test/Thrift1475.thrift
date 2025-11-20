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

struct StructB
{
  1: string x
}

struct StructA
{
  1: string a,
  2: binary b,
  3: optional string c,
  4: optional binary d,
  5: required string e,
  6: required binary f,
  7: string g = "foo",
  8: i32 h,
  9: optional i32 i,
  10: required i32 j,
  11: required i32 k = 5,
  12: double l,
  13: optional double m,
  14: required double n,
  15: double o = 3.14159,
  16: list<string> string_list,
  17: list<byte> byte_list = [1, 2, 3],
  18: required list<string> rsl,
  19: optional list<string> osl,
  20: set<string> string_set,
  21: required set<string> rss,
  22: optional set<string> oss,
  23: map<string, string> string_map,
  24: required map<string, string> rsm,
  25: optional map<string, string> osm,
  26: StructB structb
}
