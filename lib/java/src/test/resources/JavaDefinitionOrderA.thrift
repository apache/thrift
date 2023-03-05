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

// Define Parent, then Child. No forward declarations.
struct Parent {
    1: required string Name
}

typedef Parent MyParent
typedef list<Parent> Parents

enum MyEnum {
    FOO = 1
    BAR = 2
}

typedef i8 Age
typedef MyEnum MyEnumV2
typedef set<MyEnum> MyEnums
typedef map<MyEnumV2, Parent> MyMapping
typedef binary MyBinary

struct Child {
    1: required string Name
    2: required Age Age
    3: required Parent Parent1
    4: required MyParent Parent2
    5: required Parents GrandParents
    6: required MyEnum MyEnum
    7: required MyEnumV2 MyEnumV2
    8: required MyEnums MyEnums
    9: required MyMapping MyMapping
    10: required MyBinary MyBinary
}
