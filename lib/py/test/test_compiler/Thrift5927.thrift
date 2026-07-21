# Licensed to the Apache Software Foundation (ASF) under one
# or more contributor license agreements. See the NOTICE file
# distributed with this work for additional information
# regarding copyright ownership. The ASF licenses this file
# to you under the Apache License, Version 2.0 (the
# "License"); you may not use this file except in compliance
# with the License. You may obtain a copy of the License at
#
#   http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing,
# software distributed under the License is distributed on an
# "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
# KIND, either express or implied. See the License for the
# specific language governing permissions and limitations
# under the License.

namespace py thrift5927

include "thrift5927include.thrift"

enum Lambda {
	None = 0,
	FluxCapacitor = 1
}

struct False {
	1: Lambda finally
}

union import {
	1: import print
	2: False while
}

exception True {
	1: import print
}

service continue {
	import return(1: False while) throws (1: True yield)
}

# Exercises type_name()'s cross-module branch: a field typed with a struct
# defined in an included file, where that struct's name is a keyword.
struct UsesIncluded {
	1: thrift5927include.except item
}

# Exercises the extends import/base-class code path with a keyword-named
# parent service defined in the same file.
service AlsoDerived extends continue {
}

# Exercises type_name()'s service branch for both "extends" (import of the
# parent's module + base class reference) and the cross-module include path
# at once, since the parent service's name is also a keyword.
service Derived extends thrift5927include.class {
}

# Exercises a plain top-level const whose own name is a keyword.
const i32 break = 42

# Exercises render_const_value()'s enum branch (an unescaped enum *value*
# reference, e.g. "Lambda.None"), reached both from a top-level const like
# this one and from any field's default value (they share the same
# rendering code), together with a keyword-named const.
const Lambda del = Lambda.None

# Exercises the required-field self-check ("if self.<field> is None:").
struct RequiredKw {
	1: required i32 pass
}

# Exercises two gen_type_hints_/is_immutable-gated code paths at once: the
# class-level type-hint annotation (-gen py:type_hints,enum) and the
# __setattr__ override's "EnumType.__members__.get(<field>)" call
# (-gen py:enum), both keyed on the same keyword-named, enum-typed,
# default-valued field.
struct ImmutableEnumField {
	1: Lambda with = Lambda.None
} (python.immutable = "")

