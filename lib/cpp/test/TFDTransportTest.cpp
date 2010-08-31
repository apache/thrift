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

#include <cstdlib>
#include <stdexcept>
#include <Thrift.h>
#include <transport/TFDTransport.h>
using apache::thrift::transport::TTransportException;
using apache::thrift::transport::TFDTransport;

class DummyException : std::exception {
};

int main() {
  {
    TFDTransport t(256, TFDTransport::NO_CLOSE_ON_DESTROY);
  }

  try {
    {
      TFDTransport t(256, TFDTransport::CLOSE_ON_DESTROY);
    }
    std::abort();
  } catch (TTransportException) {
  }

  try {
    {
      TFDTransport t(256, TFDTransport::CLOSE_ON_DESTROY);
      throw DummyException();
    }
    std::abort();
  } catch (TTransportException&) {
    abort();
  } catch (DummyException&) {
  }

  return 0;

}
