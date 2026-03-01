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

#include <netdb.h>

#include <thrift/c_glib/transport/thrift_transport.h>
#include <thrift/c_glib/transport/thrift_socket.h>
#include <thrift/c_glib/transport/thrift_server_transport.h>
#include <thrift/c_glib/transport/thrift_server_socket.h>

static const gchar TEST_DATA[11] = "abcdefghij";

#include "../src/thrift/c_glib/transport/thrift_memory_buffer.c"

#define MAX_MESSAGE_SIZE 2

static void
test_open_and_close (void)
{
  ThriftMemoryBuffer *tbuffer = NULL;
  ThriftConfiguration *tconfiguration = NULL;

  /* create a ThriftConfiguration */
  tconfiguration = g_object_new (THRIFT_TYPE_CONFIGURATION, 
                                 "max_message_size", MAX_MESSAGE_SIZE,
                                 "max_frame_size", MAX_MESSAGE_SIZE,
                                 NULL);
  /* create a ThriftMemoryBuffer */
  tbuffer = g_object_new (THRIFT_TYPE_MEMORY_BUFFER, "configuration", tconfiguration, NULL);

  /* no-ops */
  g_assert (thrift_memory_buffer_open (THRIFT_TRANSPORT (tbuffer), NULL) == TRUE);
  g_assert (thrift_memory_buffer_is_open (THRIFT_TRANSPORT (tbuffer)) == TRUE);
  g_assert (thrift_memory_buffer_close (THRIFT_TRANSPORT (tbuffer), NULL) == TRUE);

  g_object_unref (tbuffer);
  g_object_unref (tconfiguration);
}

static void
test_read_and_write (void)
{
  ThriftConfiguration *tconfiguration = NULL;
  ThriftMemoryBuffer *tbuffer = NULL;
  gint got, want;
  gchar read[10];
  gchar *b;
  GError *error = NULL;

  tconfiguration = g_object_new (THRIFT_TYPE_CONFIGURATION, "max_message_size", MAX_MESSAGE_SIZE, NULL);
  tbuffer = g_object_new (THRIFT_TYPE_MEMORY_BUFFER, "buf_size", 15, "configuration", tconfiguration, NULL);
  THRIFT_TRANSPORT_GET_CLASS (tbuffer)->resetConsumedMessageSize(THRIFT_TRANSPORT(tbuffer), -1, NULL);
  g_assert (thrift_memory_buffer_write (THRIFT_TRANSPORT (tbuffer),
                                        (gpointer) TEST_DATA, 10, &error) == TRUE);
  g_assert (error == NULL);

  memset(read, 0, 10);
  b = read;
  want = 10;
  got = thrift_memory_buffer_read (THRIFT_TRANSPORT (tbuffer),
                                   (gpointer) b, want, &error);
  g_assert (got < 0);
  g_object_unref (tbuffer);
  g_object_unref (tconfiguration);
}

int
main(int argc, char *argv[])
{
#if (!GLIB_CHECK_VERSION (2, 36, 0))
  g_type_init ();
#endif

  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/testthriftmemorybufferreadcheck/OpenAndClose", test_open_and_close);
  g_test_add_func ("/testthriftmemorybufferreadcheck/ReadAndWrite", test_read_and_write);

  return g_test_run ();
}
