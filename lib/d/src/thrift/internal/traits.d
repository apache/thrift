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
module thrift.internal.traits;

import std.traits;

/**
 * Adds or removes attributes from the given function (pointer) or delegate
 * type.
 *
 * Besides the base type, two std.traits.FunctionAttribute bitfields are
 * accepted, representing the attributes to add to the function signature
 * resp. to remove from it. If an attribute appears in both fields, an error
 * is raised.
 *
 * Params:
 *   T = The base type.
 *   setAttrs = The attributes to add to the type, if not already present.
 *   clearAttrs = The attributes to remove from the type, if present.
 */
template ChangeFuncAttrs(
  T,
  FunctionAttribute setAttrs = FunctionAttribute.none,
  FunctionAttribute clearAttrs = FunctionAttribute.none
) if (isFunctionPointer!T || isDelegate!T) {
  static assert(!(setAttrs & clearAttrs),
    "Cannot set and clear attributes at the same time.");
  mixin({
    enum newAttrs = (functionAttributes!T | setAttrs) & ~clearAttrs;
    static assert(!(newAttrs & FunctionAttribute.trusted) ||
      !(newAttrs & FunctionAttribute.safe),
      "Cannot have a function/delegate that is both trusted and safe.");

    string result = "alias ";

    static if (functionLinkage!T != "D") {
      result ~= "extern(" ~ functionLinkage!T ~ ") ";
    }

    static if (newAttrs & FunctionAttribute.ref_) {
      result ~= "ref ";
    }

    result ~= "ReturnType!T";

    static if (isDelegate!T) {
      result ~= " delegate";
    } else {
      result ~= " function";
    }

    result ~= "(ParameterTypeTuple!T)";

    static if (newAttrs & FunctionAttribute.pure_) {
      result ~= " pure";
    }
    static if (newAttrs & FunctionAttribute.nothrow_) {
      result ~= " nothrow";
    }
    static if (newAttrs & FunctionAttribute.property) {
      result ~= " @property";
    }
    static if (newAttrs & FunctionAttribute.trusted) {
      result ~= " @trusted";
    }
    static if (newAttrs & FunctionAttribute.safe) {
      result ~= " @safe";
    }

    result ~= " ChangeFuncAttrs;";
    return result;
  }());
}

/// Ditto
template ChangeFuncAttrs(
  T,
  FunctionAttribute setAttrs = FunctionAttribute.none,
  FunctionAttribute clearAttrs = FunctionAttribute.none
) if (is(T == function)) {
  // To avoid a lot of syntactic headaches, we just use the above version to
  // operate on the corresponding function pointer type and then remove the
  // pointer again.
  alias FunctionTypeOf!(ChangeFuncAttrs!(T*, setAttrs, clearAttrs))
    ChangeFuncAttrs;
}

version (unittest) {
  import std.algorithm;
  import std.metastrings;
  import std.typetuple;
}
unittest {
  alias FunctionAttribute FA;
  foreach (T0; TypeTuple!(
    int function(int),
    int delegate(int),
    FunctionTypeOf!(int function(int))
  )) {
    alias ChangeFuncAttrs!(T0, FA.safe) T1;
    static assert(functionAttributes!T1 == FA.safe);

    alias ChangeFuncAttrs!(T1, FA.nothrow_ | FA.ref_, FA.safe) T2;
    static assert(functionAttributes!T2 == (FA.nothrow_ | FA.ref_));

    enum allAttrs = reduce!"a | b"([EnumMembers!FA]) & ~FA.safe;

    alias ChangeFuncAttrs!(T2, allAttrs) T3;
    static assert(functionAttributes!T3 == allAttrs);

    alias ChangeFuncAttrs!(T3, FA.none, allAttrs) T4;
    static assert(is(T4 == T0));
  }
}

/**
 * Adds »nothrow« to the type of the passed function pointer/delegate, if it
 * is not already present.
 *
 * Technically, assumeNothrow just performs a cast, but using it has the
 * advantage of being explicitly about the operation that is performed.
 */
auto assumeNothrow(T)(T t) if (isFunctionPointer!T || isDelegate!T) {
  return cast(ChangeFuncAttrs!(T, FunctionAttribute.nothrow_))t;
}
