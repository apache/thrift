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

/* A class that overrides finalize() has to chain up to its parent's
 * implementation.  Only GObject's own finalize() releases the data attached to
 * an instance: whatever was set with g_object_set_data_full(), and whatever
 * GLib keeps there itself while it disposes of the object.  A finalize() that
 * stops the chain leaks that data every time an object is destroyed.
 *
 * Each test attaches data with a destroy notification to a fresh object,
 * drops the only reference to it and checks that the notification ran. */

#include <glib-object.h>

#include <thrift/c_glib/thrift_application_exception.h>
#include <thrift/c_glib/transport/thrift_buffered_transport.h>
#include <thrift/c_glib/transport/thrift_fd_transport.h>
#include <thrift/c_glib/transport/thrift_framed_transport.h>
#include <thrift/c_glib/transport/thrift_memory_buffer.h>
#include <thrift/c_glib/transport/thrift_server_socket.h>
#include <thrift/c_glib/transport/thrift_socket.h>
#include "gen-c_glib/t_test_thrift_test_types.h"

static void
count_release (gpointer data)
{
  guint *releases = data;

  (*releases)++;
}

/* Drops the only reference to object and answers how many times the data
 * attached to it just before was released. */
static guint
releases_on_last_unref (gpointer object)
{
  guint releases = 0;

  g_object_set_data_full (G_OBJECT (object), "testfinalize", &releases,
                          count_release);
  g_object_unref (object);

  return releases;
}

static void
test_generated_struct (void)
{
  TTestXtruct *xtruct = g_object_new (T_TEST_TYPE_XTRUCT,
                                      "string_thing", "a string",
                                      NULL);

  g_assert_cmpuint (releases_on_last_unref (xtruct), ==, 1);
}

static void
test_generated_exception (void)
{
  TTestXception *xception = g_object_new (T_TEST_TYPE_XCEPTION,
                                          "errorCode", 1001,
                                          "message", "an exception",
                                          NULL);

  g_assert_cmpuint (releases_on_last_unref (xception), ==, 1);
}

static void
test_application_exception (void)
{
  ThriftApplicationException *xception =
    g_object_new (THRIFT_TYPE_APPLICATION_EXCEPTION,
                  "message", "an exception",
                  NULL);

  g_assert_cmpuint (releases_on_last_unref (xception), ==, 1);
}

/* The buffered, framed and zlib transports do not own the transport they
 * wrap, so it is released after the wrapper. */
static void
test_buffered_transport (void)
{
  ThriftMemoryBuffer *inner = g_object_new (THRIFT_TYPE_MEMORY_BUFFER, NULL);
  ThriftBufferedTransport *transport =
    g_object_new (THRIFT_TYPE_BUFFERED_TRANSPORT, "transport", inner, NULL);

  g_assert_cmpuint (releases_on_last_unref (transport), ==, 1);

  g_object_unref (inner);
}

static void
test_fd_transport (void)
{
  ThriftFDTransport *transport = g_object_new (THRIFT_TYPE_FD_TRANSPORT, NULL);

  g_assert_cmpuint (releases_on_last_unref (transport), ==, 1);
}

static void
test_framed_transport (void)
{
  ThriftMemoryBuffer *inner = g_object_new (THRIFT_TYPE_MEMORY_BUFFER, NULL);
  ThriftFramedTransport *transport =
    g_object_new (THRIFT_TYPE_FRAMED_TRANSPORT, "transport", inner, NULL);

  g_assert_cmpuint (releases_on_last_unref (transport), ==, 1);

  g_object_unref (inner);
}

static void
test_memory_buffer (void)
{
  ThriftMemoryBuffer *buffer = g_object_new (THRIFT_TYPE_MEMORY_BUFFER, NULL);

  g_assert_cmpuint (releases_on_last_unref (buffer), ==, 1);
}

static void
test_server_socket (void)
{
  ThriftServerSocket *tsocket = g_object_new (THRIFT_TYPE_SERVER_SOCKET, NULL);

  g_assert_cmpuint (releases_on_last_unref (tsocket), ==, 1);
}

static void
test_socket (void)
{
  ThriftSocket *tsocket = g_object_new (THRIFT_TYPE_SOCKET, NULL);

  g_assert_cmpuint (releases_on_last_unref (tsocket), ==, 1);
}

int
main (int argc, char *argv[])
{
#if (!GLIB_CHECK_VERSION (2, 36, 0))
  g_type_init ();
#endif

  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/testfinalize/GeneratedStruct", test_generated_struct);
  g_test_add_func ("/testfinalize/GeneratedException",
                   test_generated_exception);
  g_test_add_func ("/testfinalize/ApplicationException",
                   test_application_exception);
  g_test_add_func ("/testfinalize/BufferedTransport", test_buffered_transport);
  g_test_add_func ("/testfinalize/FDTransport", test_fd_transport);
  g_test_add_func ("/testfinalize/FramedTransport", test_framed_transport);
  g_test_add_func ("/testfinalize/MemoryBuffer", test_memory_buffer);
  g_test_add_func ("/testfinalize/ServerSocket", test_server_socket);
  g_test_add_func ("/testfinalize/Socket", test_socket);

  return g_test_run ();
}
