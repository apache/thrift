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
 * Interface for storing server's connection context.
 */
package org.apache.thrift.server;

public interface ServerContext {

  /**
   * Returns an object that implements the given interface to allow access to
   * application specific contexts.
   *
   * @param iface A Class defining an interface that the result must implement
   * @return an object that implements the interface
   * @throws RuntimeException If the context cannot be unwrapped to the provided
   *           class
   */
  <T> T unwrap(Class<T> iface);

  /**
   * Returns true if this server context is a wrapper for the provided
   * application specific context interface argument or returns false otherwise.
   *
   * @param iface a Class defining the underlying context
   * @return true if this implements the interface can be unwrapped to the
   *         provided class
   * @throws RuntimeException if an error occurs while determining whether the
   *           provided class can be unwrapped from this context.
   */
  boolean isWrapperFor(Class<?> iface);
}
