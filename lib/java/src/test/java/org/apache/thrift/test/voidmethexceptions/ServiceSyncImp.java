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
import thrift.test.voidmethexceptions.TAppService01;
import thrift.test.voidmethexceptions.TExampleException;

public class ServiceSyncImp extends ServiceBase implements TAppService01.Iface {

  @Override
  public String returnString(String msg,
      boolean throwException) throws TExampleException, TException {
    if (throwException) {
      throw new TExampleException(msg);
    }
    return msg;
  }

  @Override
  public void returnVoidThrows(String msg,
      boolean throwException) throws TExampleException, TException {
    if (throwException) {
      throw new TExampleException(msg);
    }
  }

  @Override
  public void returnVoidNoThrowsRuntimeException(String msg,
      boolean throwException) throws TException {
    if (throwException) {
      throw new RuntimeException(msg);
    }
  }

  @Override
  public void returnVoidNoThrowsTApplicationException(String msg,
      boolean throwException) throws TException {
    if (throwException) {
      throw new TApplicationException(TApplicationException.INTERNAL_ERROR, msg);
    }
  }

  @Override
  public void onewayVoidNoThrows(String msg, boolean throwException) throws TException {
    if (throwException) {
      throw new TApplicationException(TApplicationException.INTERNAL_ERROR, msg);
    } else {
      // simulate hang up
      waitForCancel();
    }
  }

}
