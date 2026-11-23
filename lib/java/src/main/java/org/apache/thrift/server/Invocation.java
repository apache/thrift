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

package org.apache.thrift.server;

import org.apache.thrift.server.AbstractNonblockingServer.FrameBuffer;

/**
 * An Invocation represents a method call that is prepared to execute, given an idle worker thread.
 * It contains the input and output protocols the thread's processor should use to perform the usual
 * Thrift invocation.
 */
class Invocation implements Runnable {
  private final FrameBuffer frameBuffer;

  public Invocation(final FrameBuffer frameBuffer) {
    this.frameBuffer = frameBuffer;
  }

  @Override
  public void run() {
    frameBuffer.invoke();
  }
}
