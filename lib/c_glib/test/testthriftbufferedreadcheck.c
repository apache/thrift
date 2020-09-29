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
#include <signal.h>
#include <sys/wait.h>

#include <thrift/c_glib/transport/thrift_transport.h>
#include <thrift/c_glib/transport/thrift_socket.h>
#include <thrift/c_glib/transport/thrift_server_transport.h>
#include <thrift/c_glib/transport/thrift_server_socket.h>

#define TEST_DATA { 'a', 'b', 'c' }

#define MAX_MESSAGE_SIZE 3

#include "../src/thrift/c_glib/transport/thrift_buffered_transport.c"

static void thrift_server (const int port);
static void thrift_socket_server_open (const int port, int times);

static void
test_open_and_close(void)
{
  ThriftSocket *tsocket = NULL;
  ThriftTransport *transport = NULL;
  GError *err = NULL;
  pid_t pid;
  int port = 51199;
  int status;

  pid = fork ();
  g_assert ( pid >= 0 );

  if ( pid == 0 )
  {
    /* child listens */
    thrift_socket_server_open (port,1);
    exit (0);
  } else {
    /* parent connects, wait a bit for the socket to be created */
    sleep (1);
    /* create a ThriftSocket */
    tsocket = g_object_new (THRIFT_TYPE_SOCKET, "hostname", "localhost",
                            "port", port, NULL);

    /* create a BufferedTransport wrapper of the Socket */
    transport = g_object_new (THRIFT_TYPE_BUFFERED_TRANSPORT,
                              "transport", THRIFT_TRANSPORT (tsocket), 
                              NULL);

    /* this shouldn't work */
    g_assert (thrift_buffered_transport_open (transport, NULL) == TRUE);
    g_assert (thrift_buffered_transport_is_open (transport) == TRUE);
    g_assert (thrift_buffered_transport_close (transport, NULL) == TRUE);
    g_object_unref (transport);
    g_object_unref (tsocket);

    /* try and underlying socket failure */
    tsocket = g_object_new (THRIFT_TYPE_SOCKET, "hostname", "localhost.broken",
                            NULL);

    /* create a BufferedTransport wrapper of the Socket */
    transport = g_object_new (THRIFT_TYPE_BUFFERED_TRANSPORT,
                              "transport", THRIFT_TRANSPORT (tsocket), NULL);

    g_assert (thrift_buffered_transport_open (transport, &err) == FALSE);
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
  int port = 51199;
  guchar buf[3] = TEST_DATA; /* a buffer */

  pid = fork ();
  g_assert ( pid >= 0 );

  if ( pid == 0 )
  {
    /* child listens */
    thrift_server (port);
    exit (0);
  } else {
    /* parent connects, wait a bit for the socket to be created */
    sleep (1);

    tsocket = g_object_new (THRIFT_TYPE_SOCKET, "hostname", "localhost",
                            "port", port, NULL);
    transport = g_object_new (THRIFT_TYPE_BUFFERED_TRANSPORT,
                              "transport", THRIFT_TRANSPORT (tsocket),
                              "w_buf_size", 4, NULL);

    g_assert (thrift_buffered_transport_open (transport, NULL) == TRUE);
    g_assert (thrift_buffered_transport_is_open (transport));

    /* write 3 bytes */
    thrift_buffered_transport_write (transport, buf, 3, NULL);

    /* write 4 bytes */
    thrift_buffered_transport_write (transport, buf, 4, NULL);

    thrift_buffered_transport_write_end (transport, NULL);
    thrift_buffered_transport_flush (transport, NULL);
    thrift_buffered_transport_close (transport, NULL);

    g_object_unref (transport);
    g_object_unref (tsocket);

    g_assert ( wait (&status) == pid );
    g_assert ( status == 0 );
  }
}


static void
thrift_socket_server_open (const int port, int times)
{
  ThriftServerTransport *transport = NULL;
  ThriftTransport *client = NULL;
  int i;

  ThriftConfiguration *tconfiguration = g_object_new (THRIFT_TYPE_CONFIGURATION, "max_message_size", MAX_MESSAGE_SIZE,
                                                      "max_frame_size", MAX_MESSAGE_SIZE, NULL);

  ThriftServerSocket *tsocket = g_object_new (THRIFT_TYPE_SERVER_SOCKET, "port", port, 
                                              "configuration", tconfiguration, NULL);

  transport = THRIFT_SERVER_TRANSPORT (tsocket);
  THRIFT_SERVER_TRANSPORT_GET_CLASS (tsocket)->resetConsumedMessageSize(transport, -1, NULL);
  thrift_server_transport_listen (transport, NULL);
  for(i=0;i<times;i++){
      client = thrift_server_transport_accept (transport, NULL);
      g_assert (client != NULL);
      thrift_socket_close (client, NULL);
      g_object_unref (client);
  }
  g_object_unref (tsocket);
  g_object_unref (tconfiguration);
  g_assert(tconfiguration != NULL);
}

static void
thrift_server (const int port)
{
  int bytes = 0;
  ThriftServerTransport *transport = NULL;
  ThriftTransport *client = NULL;
  guchar buf[3]; /* a buffer */
  guchar match[3] = TEST_DATA;

  ThriftConfiguration *tconfiguration = g_object_new (THRIFT_TYPE_CONFIGURATION, "max_message_size", MAX_MESSAGE_SIZE,
                                                      "max_frame_size", MAX_MESSAGE_SIZE, NULL);

  ThriftServerSocket *tsocket = g_object_new (THRIFT_TYPE_SERVER_SOCKET,
                                              "port", port, "configuration", tconfiguration, NULL);

  transport = THRIFT_SERVER_TRANSPORT (tsocket);
  thrift_server_transport_listen (transport, NULL);

  /* wrap the client in a BufferedTransport */
  client = g_object_new (THRIFT_TYPE_BUFFERED_TRANSPORT, "transport",
                         thrift_server_transport_accept (transport, NULL),
                         "r_buf_size", 5, NULL);
  g_assert (client != NULL);

  /* read 3 bytes */
  bytes = thrift_buffered_transport_read (client, buf, 3, NULL);
  g_assert (bytes == 3); /* make sure we've read 10 bytes */
  g_assert ( memcmp (buf, match, 3) == 0 ); /* make sure what we got matches */

  bytes = thrift_buffered_transport_read (client, buf, 4, NULL);
  g_assert (bytes == -1);

  thrift_buffered_transport_read_end (client, NULL);
  thrift_buffered_transport_close (client, NULL);
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

  g_test_add_func ("/testthriftbufferedreadcheck/OpenAndClose", test_open_and_close);
  g_test_add_func ("/testthriftbufferedreadcheck/ReadAndWrite", test_read_and_write);

  return g_test_run ();
}

