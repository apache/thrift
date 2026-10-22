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

namespace netstd EnumFlagsTest

enum Permissions {
  None = 0,
  Read = 1,
  Write = 2,
  ReadWrite = 3,
  Execute = 4,
} (enum.flags)

enum Ordinary {
  First = 1,
  Second = 2,
}

typedef Permissions PermissionAlias

const Permissions DEFAULT_PERMISSIONS = Permissions.Read

struct FlagsContainer {
  1: Permissions permissions = Permissions.Read,
  2: list<Permissions> values,
  3: PermissionAlias alias,
}

struct FrozenFlags {
  1: Permissions permissions = Permissions.Read,
} (python.immutable)

struct NestedFlags {
  1: FlagsContainer flags,
  2: map<Permissions, Permissions> mapping,
  3: set<Permissions> values,
  4: optional NestedFlags child,
}
