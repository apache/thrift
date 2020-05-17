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

#include <functional>
#include <memory>
#include <string>

#include <openssl/bio.h>
#include <openssl/evp.h>

#include <thrift/Thrift.h>

using std::string;

namespace apache {
namespace thrift {
namespace transport {

std::string base64Encode(unsigned char* data, int length) {
  std::unique_ptr<BIO, std::function<void(BIO*)>> base64(BIO_new(BIO_f_base64()),
                                                         [](BIO* b) { BIO_free_all(b); });
  BIO_set_flags(base64.get(), BIO_FLAGS_BASE64_NO_NL);

  BIO* dest = BIO_new(BIO_s_mem());
  BIO_push(base64.get(), dest);
  BIO_write(base64.get(), data, length);
  int ret = BIO_flush(base64.get());
  THRIFT_UNUSED_VARIABLE(ret);

  char* encoded;
  length = BIO_get_mem_data(dest, &encoded);
  return std::string(encoded, length);
}
} // namespace transport
} // namespace thrift
} // namespace apache
