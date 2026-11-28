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

package org.apache.thrift.async;

import java.util.concurrent.CompletableFuture;

/**
 * A simple adapter that bridges {@link AsyncMethodCallback} with {@link
 * CompletableFuture}-returning style clients. Compiler generated code will invoke this adapter to
 * implement {@code FutureClient}s.
 *
 * @param <T> return type (can be {@link Void}).
 */
public final class AsyncMethodFutureAdapter<T> implements AsyncMethodCallback<T> {

  private AsyncMethodFutureAdapter() {}

  public static <T> AsyncMethodFutureAdapter<T> create() {
    return new AsyncMethodFutureAdapter<>();
  }

  private final CompletableFuture<T> future = new CompletableFuture<>();

  public CompletableFuture<T> getFuture() {
    return future;
  }

  @Override
  public void onComplete(T response) {
    future.complete(response);
  }

  @Override
  public void onError(Exception exception) {
    future.completeExceptionally(exception);
  }
}
