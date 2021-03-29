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

/* Disable string-function optimizations when glibc is used, as these produce
   compiler warnings about string length when a string function is used inside
   a call to g_assert () */
#if !defined(__APPLE__) && !defined(__FreeBSD__) && \
    !defined(__OpenBSD__) && !defined(__NetBSD__)
#include <features.h>
#endif

#ifdef __GLIBC__
#define __NO_STRING_INLINES 1
#endif

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <netdb.h>
#include <string.h>
#include <sys/wait.h>

#include <thrift/c_glib/protocol/thrift_protocol.h>
#include <thrift/c_glib/transport/thrift_socket.h>
#include <thrift/c_glib/transport/thrift_server_socket.h>
#include <thrift/c_glib/transport/thrift_framed_transport.h>

#define TEST_BOOL TRUE
#define TEST_BYTE 123
#define TEST_I16 12345
#define TEST_I32 1234567890
#define TEST_I64 123456789012345
#define TEST_NI16 (-12345)
#define TEST_NI32 (-1234567890)
#define TEST_NI64 (-123456789012345)
#define TEST_DOUBLE 1234567890.123
#define TEST_STRING "this is a test string 1234567890!@#$%^&*()"
#define TEST_PORT 51199

#define MAX_MESSAGE_SIZE 2

static int transport_read_count = 0;
static int transport_read_error = 0;
static int transport_read_error_at = -1;
gint32
my_thrift_transport_read_all (ThriftTransport *transport, gpointer buf,
                              guint32 len, GError **error)
{
  if (transport_read_count != transport_read_error_at
      && transport_read_error == 0)
  {
    transport_read_count++;
    return thrift_transport_read_all (transport, buf, len, error);
  }
  return -1;
}

static int transport_write_count = 0;
static int transport_write_error = 0;
static int transport_write_error_at = -1;
gboolean
my_thrift_transport_write (ThriftTransport *transport, const gpointer buf,
                           const guint32 len, GError **error)
{
  if (transport_write_count != transport_write_error_at
      && transport_write_error == 0)
  {
    transport_write_count++;
    return thrift_transport_write (transport, buf, len, error);
  }
  return FALSE;
}

#define thrift_transport_read_all my_thrift_transport_read_all
#define thrift_transport_write my_thrift_transport_write
#include "../src/thrift/c_glib/protocol/thrift_compact_protocol.c"
#undef thrift_transport_read_all
#undef thrift_transport_write

static void thrift_server_complex_types (const int port);

static void
test_create_and_destroy (void)
{
  GObject *object = NULL;

  /* create an object and then destroy it */
  object = g_object_new (THRIFT_TYPE_COMPACT_PROTOCOL, NULL);
  g_assert (object != NULL);
  g_object_unref (object);
}

static void
test_initialize (void)
{
  ThriftSocket *tsocket = NULL;
  ThriftCompactProtocol *protocol = NULL;
  ThriftSocket *temp = NULL;
  ThriftConfiguration *tconfiguration = NULL;
  ThriftConfiguration *tempconf = NULL;
  glong tempsize = 0;

  /* create a ThriftConfiguration */
  tconfiguration = g_object_new (THRIFT_TYPE_CONFIGURATION, "max_message_size", MAX_MESSAGE_SIZE,
                                 "max_frame_size", MAX_MESSAGE_SIZE, NULL);
  /* create a ThriftTransport */
  tsocket = g_object_new (THRIFT_TYPE_SOCKET, "hostname", "localhost",
                          "port", 51188, "configuration", tconfiguration, 
                          "remainingmessagesize", MAX_MESSAGE_SIZE, NULL);
  g_assert (tsocket != NULL);
  /* fetch the properties */
  g_object_get (G_OBJECT (tconfiguration), "max_message_size", &tempsize, NULL);
  g_assert (tempsize == MAX_MESSAGE_SIZE);
  /* fetch the properties */
  g_object_get (G_OBJECT (tsocket), "remainingmessagesize", &tempsize, NULL);
  g_assert (tempsize == MAX_MESSAGE_SIZE);
  /* fetch the properties */
  g_object_get (G_OBJECT (tsocket), "configuration", &tempconf, NULL);
  g_object_unref (tempconf);
  /* create a ThriftCompactProtocol using the Transport */
  protocol = g_object_new (THRIFT_TYPE_COMPACT_PROTOCOL, "transport",
                           tsocket, NULL);
  g_assert (protocol != NULL);
  /* fetch the properties */
  g_object_get (G_OBJECT (protocol), "transport", &temp, NULL);
  g_object_unref (temp);

  /* clean up memory */
  g_object_unref (protocol);
  g_object_unref (tsocket);
}


static void
test_read_and_write_complex_types (void)
{
  int status;
  pid_t pid;
  ThriftSocket *tsocket = NULL;
  ThriftTransport *transport = NULL;
  ThriftCompactProtocol *tc = NULL;
  ThriftProtocol *protocol = NULL;
  int port = TEST_PORT;

  /* fork a server from the client */
  pid = fork ();
  g_assert (pid >= 0);

  if (pid == 0)
  {
    /* child listens */
    thrift_server_complex_types (port);
    exit (0);
  } else {
    /* parent.  wait a bit for the socket to be created. */
    sleep (1);

    /* create a ThriftSocket */
    tsocket = g_object_new (THRIFT_TYPE_SOCKET, "hostname", "localhost",
                            "port", port, NULL);
    transport = THRIFT_TRANSPORT (tsocket);
    thrift_transport_open (transport, NULL);
    g_assert (thrift_transport_is_open (transport));

    /* create a ThriftCompactTransport */
    tc = g_object_new (THRIFT_TYPE_COMPACT_PROTOCOL, "transport",
                       tsocket, NULL);
    protocol = THRIFT_PROTOCOL (tc);
    g_assert (protocol != NULL);

    g_assert (thrift_compact_protocol_write_map_begin (protocol, T_VOID, T_BYTE,
                                                       1, NULL) > 0);
    g_assert (thrift_compact_protocol_write_map_end (protocol, NULL) == 0);

    g_assert (thrift_compact_protocol_write_map_begin (protocol, T_VOID, T_BYTE,
                                                       3, NULL) > 0);
    g_assert (thrift_compact_protocol_write_map_end (protocol, NULL) == 0);

    g_assert (thrift_compact_protocol_write_list_begin (protocol, T_BYTE,
                                                        1, NULL) > 0);
    g_assert (thrift_compact_protocol_write_list_end (protocol, NULL) == 0);

    g_assert (thrift_compact_protocol_write_list_begin (protocol, T_I32,
                                                        3, NULL) > 0);
    g_assert (thrift_compact_protocol_write_list_end (protocol, NULL) == 0);

    /* clean up */
    thrift_transport_close (transport, NULL);
    g_object_unref (tsocket);
    g_object_unref (protocol);
    g_assert (wait (&status) == pid);
    g_assert (status == 0);
  }
}


static void
thrift_server_complex_types (const int port)
{
  ThriftServerTransport *transport = NULL;
  ThriftTransport *client = NULL;
  ThriftCompactProtocol *tc = NULL;
  ThriftProtocol *protocol = NULL;
  ThriftType element_type, key_type, value_type;
  guint32 size = 0;

  ThriftConfiguration *tconfiguration = g_object_new (THRIFT_TYPE_CONFIGURATION, "max_message_size", MAX_MESSAGE_SIZE,
                                                      "max_frame_size", MAX_MESSAGE_SIZE, NULL);
  ThriftServerSocket *tsocket = g_object_new (THRIFT_TYPE_SERVER_SOCKET,
                                              "port", port, "configuration", tconfiguration, NULL);
  transport = THRIFT_SERVER_TRANSPORT (tsocket);
  THRIFT_SERVER_TRANSPORT_GET_CLASS (tsocket)->resetConsumedMessageSize(transport, -1, NULL);
  thrift_server_transport_listen (transport, NULL);
  client = thrift_server_transport_accept (transport, NULL);
  g_assert (client != NULL);

  tc = g_object_new (THRIFT_TYPE_COMPACT_PROTOCOL, "transport",
                     client, NULL);
  protocol = THRIFT_PROTOCOL (tc);

  g_assert (thrift_compact_protocol_read_map_begin (protocol, &key_type, &value_type,
                                                    &size, NULL) > 0);
  g_assert (thrift_compact_protocol_read_map_end (protocol, NULL) == 0);

  g_assert (thrift_compact_protocol_read_map_begin (protocol, &key_type, &value_type,
                                                    &size, NULL) == -1);
  g_assert (thrift_compact_protocol_read_map_end (protocol, NULL) == 0);

  g_assert (thrift_compact_protocol_read_list_begin (protocol, &element_type, 
                                                     &size, NULL) > 0);
  g_assert (thrift_compact_protocol_read_list_end (protocol, NULL) == 0);

  g_assert (thrift_compact_protocol_read_list_begin (protocol, &element_type, 
                                                     &size, NULL) == -1);
  g_assert (thrift_compact_protocol_read_list_end (protocol, NULL) == 0);

  g_object_unref (client);
  g_object_unref (tsocket);
  g_object_unref (tconfiguration);
}


int
main (int argc, char *argv[])
{
#if (!GLIB_CHECK_VERSION (2, 36, 0))
  g_type_init ();
#endif

  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/testthriftcompactreadcheck/CreateAndDestroy",
                   test_create_and_destroy);
  g_test_add_func ("/testthriftcompactreadcheck/Initialize", test_initialize);
  g_test_add_func ("/testthriftcompactreadcheck/ReadAndWriteComplexTypes",
                   test_read_and_write_complex_types);

  return g_test_run ();
}
