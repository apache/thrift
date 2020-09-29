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
#ifdef __GLIBC__
#include <features.h>
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
#define TEST_I64 G_GINT64_CONSTANT (123456789012345)
#define TEST_DOUBLE 1234567890.123
#define TEST_STRING "this is a test string 1234567890!@#$%^&*()"
#define TEST_PORT 51199

#define MAX_MESSAGE_SIZE 4

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
#include "../src/thrift/c_glib/protocol/thrift_binary_protocol.c"
#undef thrift_transport_read_all
#undef thrift_transport_write

static void thrift_server_complex_types (const int port);

static void
test_create_and_destroy (void)
{
    GObject *object = NULL;

    /* create an object and then destroy it */
    object = g_object_new (THRIFT_TYPE_BINARY_PROTOCOL, NULL);
    g_assert (object !=NULL);
    g_object_unref (object);
}

static void
test_initialize (void)
{
    ThriftConfiguration *tconfiguration = NULL;
    ThriftSocket *tsocket = NULL;
    ThriftBinaryProtocol *bprotocol = NULL;
    ThriftSocket *temp = NULL;
    ThriftConfiguration *tempconf = NULL;

    glong tempsize = 0;

    /* create a ThriftConfiguration */
    tconfiguration = g_object_new (THRIFT_TYPE_CONFIGURATION, "max_message_size", MAX_MESSAGE_SIZE, 
                                   "max_frame_size", MAX_MESSAGE_SIZE, NULL);
    g_assert (tconfiguration != NULL);
    /* create a ThriftTransport */
    tsocket = g_object_new (THRIFT_TYPE_SOCKET, "hostname", "localhost",
                            "port", 51188, "path", NULL, 
                            "configuration", tconfiguration, 
                            "remainingmessagesize", tconfiguration->maxMessageSize_, NULL);
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
    /* create a ThriftBinaryProtocol using Transport */
    bprotocol = g_object_new (THRIFT_TYPE_BINARY_PROTOCOL, "transport", tsocket, NULL);
    g_assert (bprotocol != NULL);
    /* fetch the properties */
    g_object_get (G_OBJECT (bprotocol), "transport", &temp, NULL);
    g_object_unref (temp);

    /* clean up memory */
    g_object_unref (bprotocol);
    g_object_unref (tsocket);
    g_object_unref (tconfiguration);
}

void
test_read_and_wirte_complex_types (void)
{  
  int status;
  pid_t pid;
  ThriftConfiguration *tconfiguration = NULL;
  ThriftSocket *tsocket = NULL;
  ThriftTransport *transport = NULL;
  ThriftBinaryProtocol *tb = NULL;
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

    /* create a ThriftConfiguration */
    tconfiguration = g_object_new (THRIFT_TYPE_CONFIGURATION, "max_message_size", MAX_MESSAGE_SIZE, 
                                   "max_frame_size", MAX_MESSAGE_SIZE, NULL);
    g_assert (tconfiguration != NULL);

    /* create a ThriftSocket */
    tsocket = g_object_new (THRIFT_TYPE_SOCKET, "hostname", "localhost",
                            "port", port, "path", NULL, 
                            "configuration", tconfiguration, NULL);
    transport = THRIFT_TRANSPORT (tsocket);
    THRIFT_TRANSPORT_GET_CLASS (tsocket)->resetConsumedMessageSize(THRIFT_TRANSPORT (tsocket), -1, NULL);
    thrift_transport_open (transport, NULL);
    g_assert (thrift_transport_is_open (transport));

    /* create a ThriftBinaryTransport */
    tb = g_object_new (THRIFT_TYPE_BINARY_PROTOCOL, "transport",
                       tsocket, NULL);
    protocol = THRIFT_PROTOCOL (tb);
    g_assert (protocol != NULL);

    /* test 1st write failure on a map */
    g_assert (thrift_binary_protocol_write_map_begin (protocol, T_VOID, T_BYTE,
                                                      1, NULL) > 0);
    g_assert (thrift_binary_protocol_write_map_end (protocol, NULL) == 0);

    g_assert (thrift_binary_protocol_write_map_begin (protocol, T_I32, T_BYTE,
                                                      1, NULL) > 0);
    g_assert (thrift_binary_protocol_write_map_end (protocol, NULL) == 0);

    /* test list operations */
    g_assert (thrift_binary_protocol_write_list_begin (protocol, T_BYTE,
                                                       1, NULL) > 0);
    g_assert (thrift_binary_protocol_write_list_end (protocol, NULL) == 0);

    g_assert (thrift_binary_protocol_write_list_begin (protocol, T_I32,
                                                       10, NULL) > 0);
    g_assert (thrift_binary_protocol_write_list_end (protocol, NULL) == 0);

    /* clean up */
    thrift_transport_close (transport, NULL);
    g_object_unref (tsocket);
    g_object_unref (protocol);
    g_object_unref (tconfiguration);
    g_assert (wait (&status) == pid);
    g_assert (status == 0);
  }
}

static void
thrift_server_complex_types (const int port)
{
    ThriftServerTransport *transport = NULL;
    ThriftTransport *client = NULL;
    ThriftBinaryProtocol *tbp = NULL;
    ThriftProtocol *protocol = NULL;
    ThriftType element_type = T_VOID, 
               key_type = T_VOID, 
               value_type = T_VOID; 
    guint32 size = 0;

    ThriftConfiguration *tconfiguration = g_object_new (THRIFT_TYPE_CONFIGURATION,
                                                        "max_message_size", MAX_MESSAGE_SIZE,
                                                        "max_frame_size", MAX_MESSAGE_SIZE, NULL);

    ThriftServerSocket *tsocket = g_object_new (THRIFT_TYPE_SERVER_SOCKET, "port", port,
                                                "configuration", tconfiguration, NULL);
    transport = THRIFT_SERVER_TRANSPORT (tsocket);
    THRIFT_SERVER_TRANSPORT_GET_CLASS (tsocket)->resetConsumedMessageSize(transport, -1, NULL);
    thrift_server_transport_listen (transport, NULL);
    client = thrift_server_transport_accept (transport, NULL);
    g_assert (client != NULL);

    tbp = g_object_new (THRIFT_TYPE_BINARY_PROTOCOL, "transport", 
                        client, NULL);
    protocol = THRIFT_PROTOCOL(tbp);

    g_assert (thrift_binary_protocol_read_map_begin (protocol, &key_type, &value_type,
                                                     &size, NULL) > 0);
    g_assert (thrift_binary_protocol_read_map_end (protocol, NULL) == 0);

    g_assert (thrift_binary_protocol_read_map_begin (protocol, &key_type, &value_type,
                                                     &size, NULL) == -1);
    g_assert (thrift_binary_protocol_read_map_end (protocol, NULL) == 0);

    /* test read failure */
    g_assert (thrift_binary_protocol_read_list_begin (protocol, &element_type,
                                                      &size, NULL) > 0);
    g_assert (thrift_binary_protocol_read_list_end(protocol, NULL) == 0);

    g_assert (thrift_binary_protocol_read_list_begin (protocol, &element_type,
                                                      &size, NULL) == -1);
    g_assert (thrift_binary_protocol_read_list_end(protocol, NULL) == 0);

    g_object_unref (client);
    /* TODO: investigate g_object_unref (tbp); */
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

     g_test_add_func ("/testthriftbinaryreadcheck/CreateAndDestroy", test_create_and_destroy);
     g_test_add_func ("/testthriftbinaryreadcheck/Initialize", test_initialize);
     g_test_add_func ("/testthriftbinaryreadcheck/test_read_and_write_complex_types", test_read_and_wirte_complex_types);

     return g_test_run ();
}
