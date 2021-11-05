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

#define TEST_DATA  \
                    { 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j',  \
                      'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't',  \
                      'u', 'v', 'w', 'x', 'y', 'z', '1', '2', '3', '4',  \
                      '5', '6', '7', '8', '9', '0' }

#include "../src/thrift/c_glib/transport/thrift_zlib_transport.c"

static void thrift_server (const int port);
static void thrift_socket_server_open (const int port, int times);

/* test object creation and destruction */
static void
test_create_and_destroy(void)
{
  ThriftTransport *transport = NULL;
  gint urbuf_size = 0;
  gint crbuf_size = 0;
  gint uwbuf_size = 0;
  gint cwbuf_size = 0;
  gint comp_level = 0;

  GObject *object = NULL;
  object = g_object_new (THRIFT_TYPE_ZLIB_TRANSPORT, NULL);
  g_assert (object != NULL);
  g_object_get (G_OBJECT (object), "transport", &transport,
                "urbuf_size", &urbuf_size,
                "crbuf_size", &crbuf_size, 
                "uwbuf_size", &uwbuf_size,
                "cwbuf_size", &cwbuf_size,
                "comp_level", &comp_level, NULL);
  g_object_unref (object);
}

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
        
        /* create a ZlibTransport wrapper of the Socket */
        transport = g_object_new (THRIFT_TYPE_ZLIB_TRANSPORT,
                                  "transport", THRIFT_TRANSPORT (tsocket), NULL);

        /* this shouldn't work */
        g_assert (thrift_zlib_transport_open (transport, NULL) == TRUE);
        g_assert (thrift_zlib_transport_is_open (transport) == TRUE);
        g_assert (thrift_zlib_transport_close (transport, NULL) == TRUE);
        g_object_unref (transport);
        g_object_unref (tsocket);

        /* try and underlying socket failure */
        tsocket = g_object_new (THRIFT_TYPE_SOCKET, "hostname", "localhost_broken",
                                NULL);
       
        /* create a ZlibTransport wrapper of the Socket */
        transport = g_object_new (THRIFT_TYPE_ZLIB_TRANSPORT,
                                  "transport", THRIFT_TRANSPORT (tsocket), NULL);

        g_assert (thrift_zlib_transport_open (transport, &err) == FALSE);
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
  gchar buf[36] = TEST_DATA;

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
       transport = g_object_new (THRIFT_TYPE_ZLIB_TRANSPORT,
                                 "transport", THRIFT_TRANSPORT (tsocket), NULL);

       g_assert (thrift_zlib_transport_open (transport, NULL) == TRUE);
       g_assert (thrift_zlib_transport_is_open (transport));

       thrift_zlib_transport_write (transport, buf, 36, NULL);
       thrift_zlib_transport_flush (transport, NULL);
       thrift_zlib_transport_write_end (transport, NULL);

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

  ThriftServerSocket *tsocket = g_object_new (THRIFT_TYPE_SERVER_SOCKET,
                                              "port", port, NULL);

  transport = THRIFT_SERVER_TRANSPORT (tsocket);
  thrift_server_transport_listen (transport, NULL);
  for(i=0;i<times;i++){
      client = thrift_server_transport_accept (transport, NULL);
      g_assert (client != NULL);
      thrift_socket_close (client, NULL);
      g_object_unref (client);
  }
  g_object_unref (tsocket);
}

static void
thrift_server (const int port)
{
  int bytes = 0;
  gboolean check_sum = FALSE;
  ThriftServerTransport *transport = NULL;
  ThriftTransport *client = NULL;
  gchar buf[36];  /* a buffer */
  gchar match[36] = TEST_DATA;
  
  ThriftServerSocket *tsocket = g_object_new (THRIFT_TYPE_SERVER_SOCKET,
                                              "port", port, NULL);

  transport = THRIFT_SERVER_TRANSPORT (tsocket);
  thrift_server_transport_listen (transport, NULL);

  /* wrap the client in a ZlibTransport */
  client = g_object_new (THRIFT_TYPE_ZLIB_TRANSPORT, "transport",
                         thrift_server_transport_accept (transport, NULL),
                         NULL);
  g_assert (client != NULL);

  /* read 36 bytes */
  thrift_zlib_transport_read (client, buf, 36, NULL);
  g_assert (memcmp(buf, match, 36) == 0 );

  thrift_zlib_transport_read_end (client, NULL);

  check_sum = thrift_zlib_transport_verify_checksum (client, NULL);
  g_assert (!check_sum);

  thrift_zlib_transport_close (client, NULL);
  g_object_unref (client);
  g_object_unref (tsocket);
}

int
main(int argc, char *argv[])
{
#if (!GLIB_CHECK_VERSION (2, 36, 0))
  g_type_init();
#endif

  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/testzlibtransport/CreateAndDestroy", test_create_and_destroy);
  g_test_add_func ("/testzlibtransport/OpenAndClose", test_open_and_close);
  g_test_add_func ("/testzlibtransport/ReadAndWrite", test_read_and_write);

  return g_test_run ();
}
