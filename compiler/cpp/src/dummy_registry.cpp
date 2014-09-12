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

#include "generate/t_generator_registry.h"

// Forced references to symbols in libgenerate library

#define THRIFT_GENERATOR_REFERENCE(language)                            \
  class t_##language##_generator_factory_impl;                          \
  extern t_##language##_generator_factory_impl global_##language##_registerer; \
  void* dummy_##language##_registry_function()                          \
  {                                                                     \
    return &global_##language##_registerer;                             \
  }

THRIFT_GENERATOR_REFERENCE(as3);
THRIFT_GENERATOR_REFERENCE(c_glib);
THRIFT_GENERATOR_REFERENCE(cocoa);
THRIFT_GENERATOR_REFERENCE(cpp);
THRIFT_GENERATOR_REFERENCE(csharp);
THRIFT_GENERATOR_REFERENCE(delphi);
THRIFT_GENERATOR_REFERENCE(d);
THRIFT_GENERATOR_REFERENCE(erl);
THRIFT_GENERATOR_REFERENCE(go);
THRIFT_GENERATOR_REFERENCE(gv);
THRIFT_GENERATOR_REFERENCE(hs);
THRIFT_GENERATOR_REFERENCE(html);
THRIFT_GENERATOR_REFERENCE(java);
THRIFT_GENERATOR_REFERENCE(javame);
THRIFT_GENERATOR_REFERENCE(js);
THRIFT_GENERATOR_REFERENCE(lua);
THRIFT_GENERATOR_REFERENCE(ocaml);
THRIFT_GENERATOR_REFERENCE(perl);
THRIFT_GENERATOR_REFERENCE(php);
THRIFT_GENERATOR_REFERENCE(py);
THRIFT_GENERATOR_REFERENCE(rb);
THRIFT_GENERATOR_REFERENCE(st);
THRIFT_GENERATOR_REFERENCE(xsd);
