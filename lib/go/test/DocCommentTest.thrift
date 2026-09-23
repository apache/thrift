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

// Doc comments whose shape gofmt rewrites: empty lines at either end and
// doubled empty lines. The generated code must come out gofmt-clean; the
// sca workflow runs gofmt over it.

namespace go doccommenttest

/**
 * A doc comment whose last line is empty.
 *
 */
struct TrailingEmptyLine {
  /**
   * The same shape on a field.
   *
   */
  1: i32 a
}

/**
 *
 * A doc comment whose first line is empty.
 */
struct LeadingEmptyLine {
  1: i32 a
}

/**
 * Two paragraphs with two empty lines between them.
 *
 *
 * The second paragraph.
 */
struct DoubledEmptyLine {
  1: i32 a
}

/**
 *
 */
struct EmptyDoc {
  1: i32 a
}

/**
 * A service with the same shapes.
 *
 */
service DocCommentService {
  /**
   *
   * A function whose doc starts with an empty line.
   */
  TrailingEmptyLine echo(1: TrailingEmptyLine value)
}
