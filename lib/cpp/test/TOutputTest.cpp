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

#include <boost/test/unit_test.hpp>
#include <thrift/TOutput.h>

#include <cerrno>
#include <cstring>
#include <string>

using apache::thrift::TOutput;

BOOST_AUTO_TEST_SUITE(TOutputTest)

BOOST_AUTO_TEST_CASE(strerror_s_returns_the_message) {
  const std::string message = TOutput::strerror_s(ENOENT);
  BOOST_CHECK(!message.empty());
  BOOST_CHECK(message.find("strerror_r() failed") == std::string::npos);
#ifndef _WIN32
  // Windows looks the code up with FormatMessage(), which has its own texts.
  BOOST_CHECK_EQUAL(message, std::string(std::strerror(ENOENT)));
  BOOST_CHECK_EQUAL(TOutput::strerror_s(EINVAL), std::string(std::strerror(EINVAL)));
#endif
}

BOOST_AUTO_TEST_SUITE_END()
