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

// Maps whose keys come off the wire, in the JavaScript generator's output. The
// value types differ on purpose: a generated map is a plain object, and what a
// member name does to one depends on what is being stored under it.

struct MapValue {
  1: string s
}

struct MapKeyHolder {
  1: map<string, MapValue> structValues
  2: map<string, string> stringValues
  3: map<string, list<string>> listValues
  4: map<string, map<string, string>> mapValues
}
