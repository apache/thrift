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
#include <sys/wait.h>

#include <thrift/c_glib/transport/thrift_transport.h>
#include <thrift/c_glib/transport/thrift_buffered_transport.h>
#include <thrift/c_glib/transport/thrift_server_transport.h>
#include <thrift/c_glib/transport/thrift_server_socket.h>
#include <thrift/c_glib/transport/thrift_ssl_socket.h>

//#define TEST_DATA { 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j' }
#define TEST_DATA { "GET / HTTP/1.1\n\n" }


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
#include "../src/thrift/c_glib/transport/thrift_ssl_socket.c"
#undef socket
#undef recv
#undef send

static void thrift_ssl_socket_server (const int port);

/* test object creation and destruction */
static void
test_ssl_create_and_destroy(void)
{
  gchar *hostname = NULL;
  guint port = 0;

  GObject *object = NULL;
  object = g_object_new (THRIFT_TYPE_SSL_SOCKET, NULL);
  assert (object != NULL);
  g_object_get (G_OBJECT(object), "hostname", &hostname, "port", &port, NULL);
  g_free (hostname);
  g_object_unref (object);
}

static void
test_ssl_create_and_set_properties(void)
{
  gchar *hostname = NULL;
  guint port = 0;
  SSL_CTX* ssl_ctx= NULL;
  GError *error=NULL;

  GObject *object = NULL;
  object = thrift_ssl_socket_new(SSLTLS, &error);
  g_object_get (G_OBJECT(object), "hostname", &hostname, "port", &port, "ssl_context", &ssl_ctx, NULL);
  assert (ssl_ctx!=NULL);

  g_free (hostname);
  g_object_unref (object);
}

static void
test_ssl_open_and_close(void)
{
  ThriftSSLSocket *tSSLSocket = NULL;
  ThriftTransport *transport = NULL;
  GError *error=NULL;

  /* open a connection and close it */
  tSSLSocket = thrift_ssl_socket_new_with_host(SSLTLS, "localhost", 51188, &error);

  transport = THRIFT_TRANSPORT (tSSLSocket);
  thrift_ssl_socket_open (transport, NULL);
  assert (thrift_ssl_socket_is_open (transport) == TRUE);
  thrift_ssl_socket_close (transport, NULL);
  assert (thrift_ssl_socket_is_open (transport) == FALSE);

  /* test close failure */
  THRIFT_SOCKET(tSSLSocket)->sd = -1;
  thrift_ssl_socket_close (transport, NULL);
  g_object_unref (tSSLSocket);

  /* try a hostname lookup failure */
  tSSLSocket = thrift_ssl_socket_new_with_host(SSLTLS, "localhost.broken", 51188, &error);
  transport = THRIFT_TRANSPORT (tSSLSocket);
  assert (thrift_ssl_socket_open (transport, &error) == FALSE);
  g_object_unref (tSSLSocket);
  g_error_free (error);
  error = NULL;

  /* try an error call to socket() */
  tSSLSocket = thrift_ssl_socket_new_with_host(SSLTLS, "localhost", 51188, &error);
  transport = THRIFT_TRANSPORT (tSSLSocket);
  socket_error = 1;
  assert (thrift_ssl_socket_open (transport, &error) == FALSE);
  socket_error = 0;
  g_object_unref (tSSLSocket);
  g_error_free (error);
}

static void
test_ssl_read_and_write(void)
{
  int status=0;
  pid_t pid;
  ThriftSSLSocket *tSSLsocket = NULL;
  ThriftTransport *transport = NULL;
//  int port = 51199;
  int port = 443;
  GError *error=NULL;

  guchar buf[17] = TEST_DATA; /* a buffer */

//  pid = fork ();
//  assert ( pid >= 0 );
//
//  if ( pid == 0 )
//  {
//    /* child listens */
//    thrift_ssl_socket_server (port);
//    exit (0);
//  } else {
    /* parent connects, wait a bit for the socket to be created */
    sleep (1);

    tSSLsocket = thrift_ssl_socket_new_with_host(SSLTLS, "owncloud.level2crm.com", port, &error);

    transport = THRIFT_TRANSPORT (tSSLsocket);
    assert (thrift_ssl_socket_open (transport, NULL) == TRUE);
    assert (thrift_ssl_socket_is_open (transport));
    thrift_ssl_socket_write (transport, buf, 17, NULL);

    /* write fail */
    send_error = 1;
    thrift_ssl_socket_write (transport, buf, 1, NULL);
    send_error = 0;

    thrift_ssl_socket_write_end (transport, NULL);
    thrift_ssl_socket_flush (transport, NULL);
    thrift_ssl_socket_close (transport, NULL);
    g_object_unref (tSSLsocket);

//    assert ( wait (&status) == pid );
    assert ( status == 0 );
//  }
}

/* test ThriftSocket's peek() implementation */
//static void
//test_ssl_peek(void)
//{
//  gint status;
//  pid_t pid;
//  guint port = 51199;
//  gchar data = 'A';
//  ThriftTransport *client_transport;
//  GError *error = NULL;
//
//  client_transport = g_object_new (THRIFT_TYPE_SSL_SOCKET,
//                                   "hostname", "localhost",
//                                   "port",     port,
//                                   NULL);
//
//  /* thrift_transport_peek returns FALSE when the socket is closed */
//  g_assert (thrift_transport_is_open (client_transport) == FALSE);
//  g_assert (thrift_transport_peek (client_transport, &error) == FALSE);
//  g_assert (error == NULL);
//
//  pid = fork ();
//  g_assert (pid >= 0);
//
//  if (pid == 0)
//  {
//    ThriftServerTransport *server_transport = NULL;
//
//    g_object_unref (client_transport);
//
//    /* child listens */
//    server_transport = g_object_new (THRIFT_TYPE_SERVER_SOCKET,
//                                     "port", port,
//                                     NULL);
//    g_assert (server_transport != NULL);
//
//    thrift_server_transport_listen (server_transport, &error);
//    g_assert (error == NULL);
//
//    client_transport = g_object_new
//      (THRIFT_TYPE_BUFFERED_TRANSPORT,
//       "transport",  thrift_server_transport_accept (server_transport, &error),
//       "r_buf_size", 0,
//       "w_buf_size", sizeof data,
//       NULL);
//    g_assert (error == NULL);
//    g_assert (client_transport != NULL);
//
//    /* write exactly one character to the client */
//    g_assert (thrift_transport_write (client_transport,
//                                      &data,
//                                      sizeof data,
//                                      &error) == TRUE);
//
//    thrift_transport_flush (client_transport, &error);
//    thrift_transport_write_end (client_transport, &error);
//    thrift_transport_close (client_transport, &error);
//
//    g_object_unref (client_transport);
//    g_object_unref (server_transport);
//
//    exit (0);
//  }
//  else {
//    /* parent connects, wait a bit for the socket to be created */
//    sleep (1);
//
//    /* connect to the child */
//    thrift_transport_open (client_transport, &error);
//    g_assert (error == NULL);
//    g_assert (thrift_transport_is_open (client_transport) == TRUE);
//
//    /* thrift_transport_peek returns TRUE when the socket is open and there is
//       data available to be read */
//    g_assert (thrift_transport_peek (client_transport, &error) == TRUE);
//    g_assert (error == NULL);
//
//    /* read exactly one character from the server */
//    g_assert_cmpint (thrift_transport_read (client_transport,
//                                            &data,
//                                            sizeof data,
//                                            &error), ==, sizeof data);
//
//    /* thrift_transport_peek returns FALSE when the socket is open but there is
//       no (more) data available to be read */
//    g_assert (thrift_transport_is_open (client_transport) == TRUE);
//    g_assert (thrift_transport_peek (client_transport, &error) == FALSE);
//    g_assert (error == NULL);
//
//    thrift_transport_read_end (client_transport, &error);
//    thrift_transport_close (client_transport, &error);
//
//    g_object_unref (client_transport);
//
//    g_assert (wait (&status) == pid);
//    g_assert (status == 0);
//  }
//}

static void
thrift_ssl_socket_server (const int port)
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
  bytes = thrift_ssl_socket_read (client, buf, 10, NULL);
  assert (bytes == 10); /* make sure we've read 10 bytes */
  assert ( memcmp(buf, match, 10) == 0 ); /* make sure what we got matches */

  /* failed read */
  recv_error = 1;
  thrift_ssl_socket_read (client, buf, 1, NULL);
  recv_error = 0;

  thrift_ssl_socket_read_end (client, NULL);
  thrift_ssl_socket_close (client, NULL);
  g_object_unref (tsocket);
  g_object_unref (client);
}

int
main(int argc, char *argv[])
{
  int retval;
#if (!GLIB_CHECK_VERSION (2, 36, 0))
  g_type_init();
#endif

  g_test_init (&argc, &argv, NULL);

  thrift_ssl_socket_initialize_openssl();

  g_test_add_func ("/testtransportsslsocket/CreateAndDestroy", test_ssl_create_and_destroy);
  g_test_add_func ("/testtransportsslsocket/CreateAndSetProperties", test_ssl_create_and_set_properties);
  g_test_add_func ("/testtransportsslsocket/OpenAndClose", test_ssl_open_and_close);
  g_test_add_func ("/testtransportsslsocket/ReadAndWrite", test_ssl_read_and_write);
//  g_test_add_func ("/testtransportsslsocket/Peek", test_ssl_peek);

  retval = g_test_run ();

  thrift_ssl_socket_finalize_openssl();

  return retval;
}

