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
 *
 * @author: David Suárez <david.sephirot@gmail.com>
 */

#ifndef THRIFT_SOCKETCOMMON_H
#define THRIFT_SOCKETCOMMON_H

#ifndef _WIN32

#include <thrift/thrift-config.h>

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif

#ifdef HAVE_SYS_UN_H
#include <sys/un.h>
#endif

#include <string>

namespace apache {
namespace thrift {
namespace transport {

socklen_t fillUnixSocketAddr(struct sockaddr_un& address, std::string& path);

}
}
} // apache::thrift::transport

#endif // _WIN32

#endif //THRIFT_SOCKETCOMMON_H
