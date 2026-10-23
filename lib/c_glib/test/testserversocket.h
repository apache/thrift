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

#ifndef _THRIFT_C_GLIB_TEST_SERVER_SOCKET_H
#define _THRIFT_C_GLIB_TEST_SERVER_SOCKET_H

#include <netinet/in.h>
#include <sys/socket.h>

#include <thrift/c_glib/thrift_configuration.h>
#include <thrift/c_glib/transport/thrift_server_socket.h>
#include <thrift/c_glib/transport/thrift_server_transport.h>

/* Start tsocket, created with port 0, listening on a port the kernel picks,
   and return that port.

   Tests call this before fork(): the child inherits a socket that already
   accepts connections, so the parent can connect at once instead of sleeping,
   and no fixed port can collide with another socket on the host. */
static inline guint
server_socket_listen_on_free_port (ThriftServerSocket *tsocket)
{
  struct sockaddr_in addr;
  socklen_t len = sizeof (addr);
  GError *error = NULL;

  thrift_server_transport_listen (THRIFT_SERVER_TRANSPORT (tsocket), &error);
  g_assert_no_error (error);

  g_assert_cmpint (getsockname (tsocket->sd, (struct sockaddr *) &addr, &len),
                   ==, 0);
  return ntohs (addr.sin_port);
}

/* Create a server socket listening on a port the kernel picks, and store
   that port in *port. configuration may be NULL; the socket takes its own
   reference.

   The parent must unref its copy right after the fork, so that a child which
   dies closes the last copy and the parent's connect fails instead of
   hanging. */
static inline ThriftServerSocket *
server_socket_new_on_free_port (ThriftConfiguration *configuration, guint *port)
{
  ThriftServerSocket *tsocket;

  tsocket = g_object_new (THRIFT_TYPE_SERVER_SOCKET, "port", 0,
                          "configuration", configuration, NULL);
  *port = server_socket_listen_on_free_port (tsocket);

  return tsocket;
}

#endif /* _THRIFT_C_GLIB_TEST_SERVER_SOCKET_H */
