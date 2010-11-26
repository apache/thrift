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

#include "transport/thrift_transport.h"
#include "transport/thrift_server_transport.h"
#include "transport/thrift_server_socket.h"

#define TEST_DATA { 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j' }

/* substituted functions to test failures of system and library calls */
static int socket_error = 0;
int
my_socket(int domain, int type, int protocol)
{
  if (socket_error == 0)
  {
    return socket (domain, type, protocol);
  }
  return -1;
}

static int recv_error = 0;
ssize_t
my_recv(int socket, void *buffer, size_t length, int flags)
{
  if (recv_error == 0)
  {
    return recv (socket, buffer, length, flags);
  }
  return -1;
}

static int send_error = 0;
ssize_t
my_send(int socket, const void *buffer, size_t length, int flags)
{
  if (send_error == 0)
  {
    return send (socket, buffer, length, flags);
  }
  return -1;
}

#define socket my_socket
#define recv my_recv
#define send my_send
#include "../src/transport/thrift_socket.c"
#undef socket
#undef recv
#undef send

static const char TEST_ADDRESS[] = "localhost";
static const short TEST_PORT = 64444;

static void thrift_socket_server (const int port);

/* test object creation and destruction */
static void
test_create_and_destroy(void)
{
  gchar *hostname = NULL;
  guint port = 0;

  GObject *object = NULL;
  object = g_object_new (THRIFT_TYPE_SOCKET, NULL);
  assert (object != NULL);
  g_object_get (G_OBJECT(object), "hostname", &hostname, "port", &port, NULL);
  g_free (hostname);

  g_object_unref (object);
}

static void
test_open_and_close(void)
{
  ThriftSocket *tsocket = NULL;
  ThriftTransport *transport = NULL;
  GError *err = NULL;

  /* open a connection and close it */
  tsocket = g_object_new (THRIFT_TYPE_SOCKET, "hostname", "localhost",
                          "port", 51188, NULL); 
  transport = THRIFT_TRANSPORT (tsocket);
  thrift_socket_open (transport, NULL);
  assert (thrift_socket_is_open (transport) == TRUE);
  thrift_socket_close (transport, NULL);
  assert (thrift_socket_is_open (transport) == FALSE);

  /* test close failure */
  tsocket->sd = -1;
  thrift_socket_close (transport, NULL);
  g_object_unref (tsocket);

  /* try a hostname lookup failure */
  tsocket = g_object_new (THRIFT_TYPE_SOCKET, "hostname", "localhost.broken",
                          NULL);
  transport = THRIFT_TRANSPORT (tsocket);
  assert (thrift_socket_open (transport, &err) == FALSE);
  g_object_unref (tsocket);
  g_error_free (err);
  err = NULL;

  /* try an error call to socket() */
  tsocket = g_object_new (THRIFT_TYPE_SOCKET, "hostname", "localhost", NULL);
  transport = THRIFT_TRANSPORT (tsocket);
  socket_error = 1;
  assert (thrift_socket_open (transport, &err) == FALSE);
  socket_error = 0;
  g_object_unref (tsocket);
  g_error_free (err);
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
    thrift_socket_server (port);
    exit (0);
  } else {
    /* parent connects, wait a bit for the socket to be created */
    sleep (1);

    tsocket = g_object_new (THRIFT_TYPE_SOCKET, "hostname", "localhost",
                            "port", port, NULL);
    transport = THRIFT_TRANSPORT (tsocket);
    assert (thrift_socket_open (transport, NULL) == TRUE);
    assert (thrift_socket_is_open (transport));
    thrift_socket_write (transport, buf, 10, NULL);

    /* write fail */
    send_error = 1;
    thrift_socket_write (transport, buf, 1, NULL);
    send_error = 0;

    thrift_socket_write_end (transport, NULL);
    thrift_socket_flush (transport, NULL);
    thrift_socket_close (transport, NULL);
    g_object_unref (tsocket);

    assert ( wait (&status) == pid );
    assert ( status == 0 );
  }
}

static void
thrift_socket_server (const int port)
{
  int bytes = 0;
  ThriftServerTransport *transport = NULL;
  ThriftTransport *client = NULL;
  guchar buf[10]; /* a buffer */
  guchar match[10] = TEST_DATA;

  ThriftServerSocket *tsocket = g_object_new (THRIFT_TYPE_SERVER_SOCKET,
                                              "port", port, NULL);

  transport = THRIFT_SERVER_TRANSPORT (tsocket);
  thrift_server_transport_listen (transport, NULL);
  client = thrift_server_transport_accept (transport, NULL);
  assert (client != NULL);

  /* read 10 bytes */
  bytes = thrift_socket_read (client, buf, 10, NULL);
  assert (bytes == 10); /* make sure we've read 10 bytes */
  assert ( memcmp(buf, match, 10) == 0 ); /* make sure what we got matches */

  /* failed read */
  recv_error = 1;
  thrift_socket_read (client, buf, 1, NULL);
  recv_error = 0;

  thrift_socket_read_end (client, NULL);
  thrift_socket_close (client, NULL);
  g_object_unref (tsocket);
  g_object_unref (client);
}

int
main(int argc, char *argv[])
{
  g_type_init();
  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/testtransportsocket/CreateAndDestroy", test_create_and_destroy);
  g_test_add_func ("/testtransportsocket/OpenAndClose", test_open_and_close);
  g_test_add_func ("/testtransportsocket/ReadAndWrite", test_read_and_write);

  return g_test_run ();
}

