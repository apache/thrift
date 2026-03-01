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
