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

@protocol TTransport <NSObject>

  /**
   * Guarantees that all of len bytes are read
   *
   * @param buf Buffer to read into
   * @param offset Index in buffer to start storing bytes at
   * @param length Maximum number of bytes to read
   * @return The number of bytes actually read, which must be equal to len
   * @throws TTransportException if there was an error reading data
   */
- (size_t) readAll: (uint8_t *) buf offset: (size_t) offset length: (size_t) length;

- (void) write: (const uint8_t *) data offset: (size_t) offset length: (size_t) length;

- (void) flush;
@end
