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

#include <assert.h>
#include <netdb.h>

#include <thrift/c_glib/transport/thrift_transport.h>
#include <thrift/c_glib/transport/thrift_socket.h>
#include <thrift/c_glib/transport/thrift_server_transport.h>
#include <thrift/c_glib/transport/thrift_server_socket.h>

#define TEST_DATA { 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j' }

#include "../src/thrift/c_glib/transport/thrift_framed_transport.c"

static const char TEST_ADDRESS[] = "localhost";
static const short TEST_PORT = 64444;

static void thrift_server (const int port);

/* test object creation and destruction */
static void
test_create_and_destroy(void)
{
  ThriftTransport *transport = NULL;
  guint r_buf_size = 0;
  guint w_buf_size = 0;

  GObject *object = NULL;
  object = g_object_new (THRIFT_TYPE_FRAMED_TRANSPORT, NULL);
  assert (object != NULL);
  g_object_get (G_OBJECT (object), "transport", &transport,
                "r_buf_size", &r_buf_size,
                "w_buf_size", &w_buf_size, NULL);
  g_object_unref (object);
}

static void
test_open_and_close(void)
{
  ThriftSocket *tsocket = NULL;
  ThriftTransport *transport = NULL;
  GError *err = NULL;

  /* create a ThriftSocket */
  tsocket = g_object_new (THRIFT_TYPE_SOCKET, "hostname", "localhost",
                          "port", 51188, NULL); 

  /* create a BufferedTransport wrapper of the Socket */
  transport = g_object_new (THRIFT_TYPE_FRAMED_TRANSPORT,
                            "transport", THRIFT_TRANSPORT (tsocket), NULL);

  /* this shouldn't work */
  assert (thrift_framed_transport_open (transport, NULL) == FALSE);
  assert (thrift_framed_transport_is_open (transport) == TRUE);
  assert (thrift_framed_transport_close (transport, NULL) == TRUE);
  g_object_unref (transport);
  g_object_unref (tsocket);

  /* try and underlying socket failure */
  tsocket = g_object_new (THRIFT_TYPE_SOCKET, "hostname", "localhost.broken",
                          NULL);

  /* create a BufferedTransport wrapper of the Socket */
  transport = g_object_new (THRIFT_TYPE_FRAMED_TRANSPORT,
                            "transport", THRIFT_TRANSPORT (tsocket), NULL);

  assert (thrift_framed_transport_open (transport, &err) == FALSE);
  g_object_unref (transport);
  g_object_unref (tsocket);
  g_error_free (err);
  err = NULL;
}

static void
test_read_and_write(void)
{
  int status;
  pid_t pid;
  ThriftSocket *tsocket = NULL;
  ThriftTransport *transport = NULL;
  int port = 51199;
  guchar buf[10] = TEST_DATA; /* a buffer */

  pid = fork ();
  assert ( pid >= 0 );

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
    transport = g_object_new (THRIFT_TYPE_FRAMED_TRANSPORT,
                              "transport", THRIFT_TRANSPORT (tsocket),
                              "w_buf_size", 4, NULL);

    assert (thrift_framed_transport_open (transport, NULL) == TRUE);
    assert (thrift_framed_transport_is_open (transport));

    /* write 10 bytes */
    thrift_framed_transport_write (transport, buf, 10, NULL);
    thrift_framed_transport_flush (transport, NULL);

    thrift_framed_transport_write (transport, buf, 1, NULL);
    thrift_framed_transport_flush (transport, NULL);

    thrift_framed_transport_write (transport, buf, 10, NULL);
    thrift_framed_transport_flush (transport, NULL);

    thrift_framed_transport_write (transport, buf, 10, NULL);
    thrift_framed_transport_flush (transport, NULL);

    thrift_framed_transport_write_end (transport, NULL);
    thrift_framed_transport_flush (transport, NULL);
    thrift_framed_transport_close (transport, NULL);

    g_object_unref (transport);
    g_object_unref (tsocket);

    assert ( wait (&status) == pid );
    assert ( status == 0 );
  }
}

static void
thrift_server (const int port)
{
  int bytes = 0;
  ThriftServerTransport *transport = NULL;
  ThriftTransport *client = NULL;
  guchar buf[12]; /* a buffer */
  guchar match[10] = TEST_DATA;

  ThriftServerSocket *tsocket = g_object_new (THRIFT_TYPE_SERVER_SOCKET,
                                              "port", port, NULL);

  transport = THRIFT_SERVER_TRANSPORT (tsocket);
  thrift_server_transport_listen (transport, NULL);

  /* wrap the client in a BufferedTransport */
  client = g_object_new (THRIFT_TYPE_FRAMED_TRANSPORT, "transport",
                         thrift_server_transport_accept (transport, NULL),
                         "r_buf_size", 5, NULL);
  assert (client != NULL);

  /* read 10 bytes */
  bytes = thrift_framed_transport_read (client, buf, 10, NULL);
  assert (bytes == 10); /* make sure we've read 10 bytes */
  assert ( memcmp (buf, match, 10) == 0 ); /* make sure what we got matches */

  bytes = thrift_framed_transport_read (client, buf, 6, NULL);
  bytes = thrift_framed_transport_read (client, buf, 5, NULL);
  bytes = thrift_framed_transport_read (client, buf, 1, NULL);

  bytes = thrift_framed_transport_read (client, buf, 12, NULL);

  thrift_framed_transport_read_end (client, NULL);
  thrift_framed_transport_close (client, NULL);
  g_object_unref (client);
  g_object_unref (tsocket);
}

int
main(int argc, char *argv[])
{
  g_type_init();
  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/testframedtransport/CreateAndDestroy", test_create_and_destroy);
  g_test_add_func ("/testframedtransport/OpenAndClose", test_open_and_close);
  g_test_add_func ("/testframedtransport/ReadAndWrite", test_read_and_write);

  return g_test_run ();
}
