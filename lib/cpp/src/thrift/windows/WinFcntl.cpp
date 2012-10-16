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

#include "WinFcntl.h"

int fcntl(SOCKET fd, int cmd, int flags)
{
    if(cmd != F_GETFL && cmd != F_SETFL)
    {
        return -1;
    }

    if(flags != O_NONBLOCK && flags != 0)
    {
        return -1;
    }

    if(cmd == F_GETFL)
    {
        return 0;
    }

    int res;
    if(flags)
    {
        res = ioctlsocket(fd, FIONBIO, reinterpret_cast<u_long *>(&(flags = 1)));
    }
    else
    {
        res = ioctlsocket(fd, FIONBIO, reinterpret_cast<u_long *>(&(flags = 0)));
    }

    return res;
}
