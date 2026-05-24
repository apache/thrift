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

/**
 * Fixture for markdown generator @param/@return rendering tests.
 */

namespace * markdown_test

/**
 * A service with documented functions.
 */
service DocTest {

  /**
   * Computes a value from two inputs.
   * @param i32 x - the numeric input
   * @param string label - a label for the result
   * @return i32 - the computed result
   */
  i32 compute(1: i32 x, 2: string label),

  /**
   * Plain prose doc, no @param or @return tags.
   */
  void plain(1: i32 x),

  /**
   * @param i32 bare - no preceding prose
   */
  void withbare(1: i32 bare),

}
