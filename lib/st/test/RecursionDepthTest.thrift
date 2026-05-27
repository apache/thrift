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

// Input for TProtocolRecursionDepthTest. The generated struct read/write path is
// driven deep enough to cross the recursion limit (64) so that the limit added
// in thrift.st / the Smalltalk generator can be exercised end to end.
//
// A *finite* chain of distinct struct types is used rather than a genuinely
// recursive type (as in test/Recursive.thrift): the Smalltalk generator
// inline-expands nested struct serialization, so a self- or mutually-recursive
// type makes it recurse without bound at code-generation time and crash. That
// is a separate, pre-existing generator limitation, tracked apart from this fix.
//
// Field 'f' is optional so an instance (or a crafted payload) can stop at any
// depth, letting one chain exercise both the at-limit and over-limit cases.

struct A1 { 1: optional A2 f }
struct A2 { 1: optional A3 f }
struct A3 { 1: optional A4 f }
struct A4 { 1: optional A5 f }
struct A5 { 1: optional A6 f }
struct A6 { 1: optional A7 f }
struct A7 { 1: optional A8 f }
struct A8 { 1: optional A9 f }
struct A9 { 1: optional A10 f }
struct A10 { 1: optional A11 f }
struct A11 { 1: optional A12 f }
struct A12 { 1: optional A13 f }
struct A13 { 1: optional A14 f }
struct A14 { 1: optional A15 f }
struct A15 { 1: optional A16 f }
struct A16 { 1: optional A17 f }
struct A17 { 1: optional A18 f }
struct A18 { 1: optional A19 f }
struct A19 { 1: optional A20 f }
struct A20 { 1: optional A21 f }
struct A21 { 1: optional A22 f }
struct A22 { 1: optional A23 f }
struct A23 { 1: optional A24 f }
struct A24 { 1: optional A25 f }
struct A25 { 1: optional A26 f }
struct A26 { 1: optional A27 f }
struct A27 { 1: optional A28 f }
struct A28 { 1: optional A29 f }
struct A29 { 1: optional A30 f }
struct A30 { 1: optional A31 f }
struct A31 { 1: optional A32 f }
struct A32 { 1: optional A33 f }
struct A33 { 1: optional A34 f }
struct A34 { 1: optional A35 f }
struct A35 { 1: optional A36 f }
struct A36 { 1: optional A37 f }
struct A37 { 1: optional A38 f }
struct A38 { 1: optional A39 f }
struct A39 { 1: optional A40 f }
struct A40 { 1: optional A41 f }
struct A41 { 1: optional A42 f }
struct A42 { 1: optional A43 f }
struct A43 { 1: optional A44 f }
struct A44 { 1: optional A45 f }
struct A45 { 1: optional A46 f }
struct A46 { 1: optional A47 f }
struct A47 { 1: optional A48 f }
struct A48 { 1: optional A49 f }
struct A49 { 1: optional A50 f }
struct A50 { 1: optional A51 f }
struct A51 { 1: optional A52 f }
struct A52 { 1: optional A53 f }
struct A53 { 1: optional A54 f }
struct A54 { 1: optional A55 f }
struct A55 { 1: optional A56 f }
struct A56 { 1: optional A57 f }
struct A57 { 1: optional A58 f }
struct A58 { 1: optional A59 f }
struct A59 { 1: optional A60 f }
struct A60 { 1: optional A61 f }
struct A61 { 1: optional A62 f }
struct A62 { 1: optional A63 f }
struct A63 { 1: optional A64 f }
struct A64 { 1: optional A65 f }
struct A65 { 1: optional i16 x }

service Deep {
  A1 echo(1: A1 a)
}
