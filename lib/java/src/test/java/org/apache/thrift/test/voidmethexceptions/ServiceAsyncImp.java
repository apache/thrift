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

package org.apache.thrift.test.voidmethexceptions;

import org.apache.thrift.TApplicationException;
import org.apache.thrift.TException;
import org.apache.thrift.async.AsyncMethodCallback;
import thrift.test.voidmethexceptions.TAppService01;
import thrift.test.voidmethexceptions.TExampleException;

public class ServiceAsyncImp extends ServiceBase implements TAppService01.AsyncIface {

  @Override
  public void returnString(String msg,
      boolean throwException,
      AsyncMethodCallback<String> resultHandler) throws TException {
    if (throwException) {
      resultHandler.onError(new TExampleException(msg));
    } else {
      resultHandler.onComplete(msg);
    }
  }

  @Override
  public void returnVoidThrows(String msg,
      boolean throwException,
      AsyncMethodCallback<Void> resultHandler) throws TException {
    if (throwException) {
      resultHandler.onError(new TExampleException(msg));
    } else {
      resultHandler.onComplete(null);
    }
  }

  @Override
  public void returnVoidNoThrowsRuntimeException(String msg,
      boolean throwException,
      AsyncMethodCallback<Void> resultHandler) throws TException {
    if (throwException) {
      resultHandler.onError(new RuntimeException(msg));
    } else {
      resultHandler.onComplete(null);
    }
  }

  @Override
  public void returnVoidNoThrowsTApplicationException(String msg,
      boolean throwException,
      AsyncMethodCallback<Void> resultHandler) throws TException {
    if (throwException) {
      resultHandler.onError(new TApplicationException(TApplicationException.INTERNAL_ERROR, msg));
    } else {
      resultHandler.onComplete(null);
    }
  }

  @Override
  public void onewayVoidNoThrows(String msg,
      boolean throwException,
      AsyncMethodCallback<Void> resultHandler) throws TException {
    if (throwException) {
      resultHandler.onError(new TApplicationException(TApplicationException.INTERNAL_ERROR, msg));
    } else {
      // simulate hang up
    }
  }

}
