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

// Testcase for THRIFT-4833 warning CS0114 "Xception.Message" hides inherited member "Exception.Message"
//
// A field becomes a property of the same name, starting with an uppercase letter. Every generated
// class inherits GetType(), MemberwiseClone() and ReferenceEquals() from System.Object, and an
// exception inherits the members of System.Exception as well. Finalize() cannot be hidden.

namespace * Thrift4833

exception InheritedNames {
  1: string message
  2: string data
  3: string source
  4: string helpLink
  5: string stackTrace
  6: string innerException
  7: i32 hResult
  8: string targetSite
  9: string getBaseException
  10: string getObjectData
  11: string getType
  12: string serializeObjectState
  13: string memberwiseClone
  14: string referenceEquals
  15: string finalize
}

exception RequiredInheritedNames {
  1: required string message
  2: required string data
  3: required string source
  4: required string helpLink
  5: required string stackTrace
  6: required string innerException
  7: required i32 hResult
  8: required string targetSite
  9: required string getBaseException
  10: required string getObjectData
  11: required string getType
  12: required string serializeObjectState
  13: required string memberwiseClone
  14: required string referenceEquals
  15: required string finalize
}

struct ObjectNames {
  1: string getType
  2: string memberwiseClone
  3: string referenceEquals
  4: string finalize
  5: string message
  6: string data
}

service InheritedNamesService {
  string Call(1: string getType, 2: string memberwiseClone, 3: string referenceEquals, 4: string message) throws (1: InheritedNames getType)
}
