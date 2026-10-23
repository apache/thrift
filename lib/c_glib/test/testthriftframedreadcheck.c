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
#include <sys/wait.h>

#include <thrift/c_glib/transport/thrift_transport.h>
#include <thrift/c_glib/transport/thrift_socket.h>
#include <thrift/c_glib/transport/thrift_server_transport.h>
#include <thrift/c_glib/transport/thrift_server_socket.h>

#define TEST_DATA { 'a', 'b' }
#define MAX_MESSAGE_SIZE 2

#include "../src/thrift/c_glib/transport/thrift_framed_transport.c"
#include "testserversocket.h"

static void thrift_server (ThriftServerSocket *tsocket);
static void thrift_socket_server_open (ThriftServerSocket *tsocket, int times);

static void
test_open_and_close(void)
{
  ThriftSocket *tsocket = NULL;
  ThriftTransport *transport = NULL;
  GError *err = NULL;
  pid_t pid;
  ThriftServerSocket *server = NULL;
  ThriftConfiguration *server_configuration = NULL;
  guint port = 0;
  int status;

  server_configuration = g_object_new (THRIFT_TYPE_CONFIGURATION,
                                       "max_message_size", MAX_MESSAGE_SIZE,
                                       "max_frame_size", MAX_MESSAGE_SIZE,
                                       NULL);
  server = server_socket_new_on_free_port (server_configuration, &port);
  g_object_unref (server_configuration);

  pid = fork ();
  g_assert ( pid >= 0 );

  if ( pid == 0 )
  {
    /* child accepts */
    thrift_socket_server_open (server,1);
    exit (0);
  } else {
    /* parent connects; the child owns the listening socket now */
    g_object_unref (server);
    /* create a ThriftSocket */
    tsocket = g_object_new (THRIFT_TYPE_SOCKET, "hostname", "localhost",
                            "port", port, NULL);

    /* create a BufferedTransport wrapper of the Socket */
    transport = g_object_new (THRIFT_TYPE_FRAMED_TRANSPORT,
                              "transport", THRIFT_TRANSPORT (tsocket), NULL);

    /* this shouldn't work */
    g_assert (thrift_framed_transport_open (transport, NULL) == TRUE);
    g_assert (thrift_framed_transport_is_open (transport) == TRUE);
    g_assert (thrift_framed_transport_close (transport, NULL) == TRUE);
    g_object_unref (transport);
    g_object_unref (tsocket);

    /* try and underlying socket failure */
    tsocket = g_object_new (THRIFT_TYPE_SOCKET, "hostname", "localhost.broken",
                            NULL);

    /* create a BufferedTransport wrapper of the Socket */
    transport = g_object_new (THRIFT_TYPE_FRAMED_TRANSPORT,
                              "transport", THRIFT_TRANSPORT (tsocket), NULL);

    g_assert (thrift_framed_transport_open (transport, &err) == FALSE);
    g_object_unref (transport);
    g_object_unref (tsocket);
    g_error_free (err);
    err = NULL;

    g_assert ( wait (&status) == pid );
    g_assert ( status == 0 );
  }
}

static void
test_read_and_write(void)
{
  int status;
  pid_t pid;
  ThriftSocket *tsocket = NULL;
  ThriftTransport *transport = NULL;
  ThriftServerSocket *server = NULL;
  guint port = 0;
  guchar buf[10] = TEST_DATA; /* a buffer */

  server = server_socket_new_on_free_port (NULL, &port);

  pid = fork ();
  g_assert ( pid >= 0 );

  if ( pid == 0 )
  {
    /* child accepts */
    thrift_server (server);
    exit (0);
  } else {
    /* parent connects; the child owns the listening socket now */
    g_object_unref (server);

    tsocket = g_object_new (THRIFT_TYPE_SOCKET, "hostname", "localhost",
                            "port", port, NULL);
    transport = g_object_new (THRIFT_TYPE_FRAMED_TRANSPORT,
                              "transport", THRIFT_TRANSPORT (tsocket),
                              "w_buf_size", 4, NULL);

    g_assert (thrift_framed_transport_open (transport, NULL) == TRUE);
    g_assert (thrift_framed_transport_is_open (transport));

    /* write 2 bytes */
    thrift_framed_transport_write (transport, buf, 2, NULL);
    thrift_framed_transport_flush (transport, NULL);

    thrift_framed_transport_write (transport, buf, 3, NULL);
    thrift_framed_transport_flush (transport, NULL);

    thrift_framed_transport_write_end (transport, NULL);
    thrift_framed_transport_flush (transport, NULL);
    thrift_framed_transport_close (transport, NULL);

    g_object_unref (transport);
    g_object_unref (tsocket);

    g_assert ( wait (&status) == pid );
    g_assert ( status == 0 );
  }
}

static void
thrift_socket_server_open (ThriftServerSocket *tsocket, int times)
{
  ThriftServerTransport *transport = NULL;
  ThriftTransport *client = NULL;
  int i;


  transport = THRIFT_SERVER_TRANSPORT (tsocket);
  for(i=0;i<times;i++){
      client = thrift_server_transport_accept (transport, NULL);
      g_assert (client != NULL);
      thrift_socket_close (client, NULL);
      g_object_unref (client);
  }
  g_object_unref (tsocket);
}

static void
thrift_server (ThriftServerSocket *tsocket)
{
  int bytes = 0;
  ThriftServerTransport *transport = NULL;
  ThriftTransport *client = NULL;
  guchar buf[12]; /* a buffer */
  guchar match[10] = TEST_DATA;

  ThriftConfiguration *tconfiguration = g_object_new (THRIFT_TYPE_CONFIGURATION,
                                                      "max_message_size", MAX_MESSAGE_SIZE,
                                                      "max_frame_size", MAX_MESSAGE_SIZE,
                                                      NULL);

  transport = THRIFT_SERVER_TRANSPORT (tsocket);

  /* wrap the client in a BufferedTransport */
  client = g_object_new (THRIFT_TYPE_FRAMED_TRANSPORT, "transport",
                         thrift_server_transport_accept (transport, NULL),
                         "r_buf_size", 5, "configuration", tconfiguration, 
                         "remainingmessagesize", MAX_MESSAGE_SIZE, NULL);
  g_assert (client != NULL);

  /* read 2 bytes */
  bytes = thrift_framed_transport_read (client, buf, 2, NULL);
  g_assert (bytes == 2); /* make sure we've read 2 bytes */
  g_assert ( memcmp (buf, match, 1) == 0 ); /* make sure what we got matches */

  bytes = thrift_framed_transport_read (client, buf, 3, NULL);
  g_assert (bytes == -1);

  thrift_framed_transport_read_end (client, NULL);
  thrift_framed_transport_close (client, NULL);
  g_object_unref (client);
  g_object_unref (tsocket);
  g_object_unref (tconfiguration);
}

int
main(int argc, char *argv[])
{
#if (!GLIB_CHECK_VERSION (2, 36, 0))
  g_type_init();
#endif

  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/testframedtransport/OpenAndClose", test_open_and_close);
  g_test_add_func ("/testframedtransport/ReadAndWrite", test_read_and_write);

  return g_test_run ();
}
