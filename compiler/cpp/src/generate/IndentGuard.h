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

#ifndef THRIFT_COMPILER_GENERATE_INDENTGUARD_H
#define THRIFT_COMPILER_GENERATE_INDENTGUARD_H

#include "IndentKeeper.h"

namespace apache { namespace thrift { namespace compiler {

class IndentGuard {
public:
  IndentGuard(IndentKeeper& keeper)
    : keeper_(keeper),
      start_indent_(keeper.get_indent())
  {
    keeper_.indent_up();
  }

  ~IndentGuard() {
    keeper_.set_indent(start_indent_);
  }

private:
  IndentKeeper& keeper_;
  const int start_indent_;
};

}}} // apache::thrift::compiler

#endif // THRIFT_COMPILER_GENERATE_INDENTGUARD_H
