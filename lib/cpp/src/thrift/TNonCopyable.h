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

#ifndef TNONCOPYABLE_H
#define TNONCOPYABLE_H

/**
 * @brief A simple non-copyable base class pattern. Derive from TNonCopyable to
 * make a class non-copyable and prohibit assignment and copy-construction.
 */
namespace apache {
namespace thrift {

class TNonCopyable {
protected:
  TNonCopyable() = default;
  ~TNonCopyable() = default;

  TNonCopyable(const TNonCopyable&) = delete;
  TNonCopyable& operator=(const TNonCopyable&) = delete;
};

}
}

#endif
