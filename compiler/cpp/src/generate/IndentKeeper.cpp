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

#include "IndentKeeper.h"

using namespace apache::thrift::compiler;

IndentKeeper::IndentKeeper()
  : indent_(0)
{}

std::string IndentKeeper::indent() const {
  return std::string(indent_ * INDENT_SIZE, ' ');
}

void IndentKeeper::set_indent(int indent) {
  indent_ = indent;
}

void IndentKeeper::indent_up() {
  ++indent_;
}

void IndentKeeper::indent_down() {
  --indent_;
}
