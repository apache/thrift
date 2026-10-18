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

#include <glib.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <arpa/inet.h>

#include <thrift/c_glib/thrift.h>
#include <thrift/c_glib/processor/thrift_processor.h>
#include <thrift/c_glib/transport/thrift_server_transport.h>
#include <thrift/c_glib/transport/thrift_server_socket.h>
#include <thrift/c_glib/transport/thrift_transport_factory.h>
#include <thrift/c_glib/protocol/thrift_binary_protocol_factory.h>

#define TEST_PORT 51199

#include <thrift/c_glib/server/thrift_simple_server.c>

/* create a rudimentary processor */
#define TEST_PROCESSOR_TYPE (test_processor_get_type ())

struct _TestProcessor
{
  ThriftProcessor parent;

  /* When max is greater than zero the processor stops the server after it
   * has handled that many connections, so that serve() returns on its own. */
  ThriftServer *server;
  guint count;
  guint max;
};
typedef struct _TestProcessor TestProcessor;

struct _TestProcessorClass
{
  ThriftProcessorClass parent;
};
typedef struct _TestProcessorClass TestProcessorClass;

G_DEFINE_TYPE(TestProcessor, test_processor, THRIFT_TYPE_PROCESSOR)

gboolean
test_processor_process (ThriftProcessor *processor, ThriftProtocol *in,
                        ThriftProtocol *out, GError **error)
{
  TestProcessor *self = (TestProcessor *) processor;

  THRIFT_UNUSED_VAR (in);
  THRIFT_UNUSED_VAR (out);
  THRIFT_UNUSED_VAR (error);

  self->count++;
  if (self->max > 0 && self->count >= self->max && self->server != NULL)
    thrift_server_stop (self->server);

  return FALSE;
}

static void
test_processor_init (TestProcessor *p)
{
  p->server = NULL;
  p->count = 0;
  p->max = 0;
}

static void
test_processor_class_init (TestProcessorClass *proc)
{
  (THRIFT_PROCESSOR_CLASS(proc))->process = test_processor_process;
}

/* A ThriftServerSocket that attaches a weak reference to every transport it
 * accepts, so a test can observe how many of them are released. */
#define TEST_TYPE_SERVER_SOCKET (test_server_socket_get_type ())

struct _TestServerSocket
{
  ThriftServerSocket parent;
};
typedef struct _TestServerSocket TestServerSocket;

struct _TestServerSocketClass
{
  ThriftServerSocketClass parent;
};
typedef struct _TestServerSocketClass TestServerSocketClass;

G_DEFINE_TYPE(TestServerSocket, test_server_socket, THRIFT_TYPE_SERVER_SOCKET)

/* incremented (in the server child process) each time an accepted transport
 * reaches a zero reference count and is finalized */
static guint sockets_released;

static void
on_socket_released (gpointer data, GObject *where_the_object_was)
{
  THRIFT_UNUSED_VAR (data);
  THRIFT_UNUSED_VAR (where_the_object_was);
  sockets_released++;
}

static ThriftTransport *
test_server_socket_accept (ThriftServerTransport *transport, GError **error)
{
  ThriftServerTransportClass *parent_class =
      THRIFT_SERVER_TRANSPORT_CLASS (test_server_socket_parent_class);
  ThriftTransport *t = parent_class->accept (transport, error);

  if (t != NULL)
    g_object_weak_ref (G_OBJECT (t), on_socket_released, NULL);

  return t;
}

static void
test_server_socket_init (TestServerSocket *s)
{
  THRIFT_UNUSED_VAR (s);
}

static void
test_server_socket_class_init (TestServerSocketClass *klass)
{
  THRIFT_SERVER_TRANSPORT_CLASS (klass)->accept = test_server_socket_accept;
}

/* Original coverage: a server created without explicit factories starts and
 * can be stopped by a signal. */
static void
test_server (void)
{
  int status;
  pid_t pid;
  TestProcessor *p = NULL;
  ThriftServerSocket *tss = NULL;
  ThriftSimpleServer *ss = NULL;

  p = g_object_new (TEST_PROCESSOR_TYPE, NULL);
  tss = g_object_new (THRIFT_TYPE_SERVER_SOCKET, "port", TEST_PORT, NULL);
  ss = g_object_new (THRIFT_TYPE_SIMPLE_SERVER, "processor", p,
                     "server_transport", THRIFT_SERVER_TRANSPORT (tss), NULL);

  /* run the server in a child process */
  pid = fork ();
  g_assert (pid >= 0);

  if (pid == 0)
  {
    THRIFT_SERVER_GET_CLASS (THRIFT_SERVER (ss))->serve (THRIFT_SERVER (ss),
                                                         NULL);
    exit (0);
  } else {
    sleep (5);
    kill (pid, SIGINT);

    g_object_unref (ss);
    g_object_unref (tss);
    g_object_unref (p);
    g_assert (wait (&status) == pid);
    g_assert (status == SIGINT);
  }
}

/* Open a TCP connection to the server, optionally send a short request-like
 * payload, then close it.  Retries until the child has started listening. */
static void
client_connect_and_close (gboolean send_payload)
{
  int fd;
  int tries;
  struct sockaddr_in addr;

  memset (&addr, 0, sizeof (addr));
  addr.sin_family = AF_INET;
  addr.sin_port = htons (TEST_PORT);
  addr.sin_addr.s_addr = htonl (INADDR_LOOPBACK);

  for (tries = 0; tries < 500; tries++)
  {
    fd = socket (AF_INET, SOCK_STREAM, 0);
    g_assert (fd >= 0);
    if (connect (fd, (struct sockaddr *) &addr, sizeof (addr)) == 0)
      break;
    close (fd);
    fd = -1;
    g_usleep (10000); /* 10 ms */
  }
  g_assert (fd >= 0);

  if (send_payload)
  {
    /* A small binary-protocol message header ("ping"), so the connection
     * carries bytes before it is closed. */
    static const unsigned char request[] = {
      0x80, 0x01, 0x00, 0x01, 0x00, 0x00, 0x00, 0x04,
      'p', 'i', 'n', 'g', 0x00, 0x00, 0x00, 0x00
    };
    ssize_t written = write (fd, request, sizeof (request));
    THRIFT_UNUSED_VAR (written);
  }

  close (fd);
}

/* Start a ThriftSimpleServer in a child process, let a client connect (and
 * close) a fixed number of times, and verify that the server releases each
 * accepted transport exactly once and keeps serving until it is stopped.
 *
 * Two configurations are exercised:
 *   - explicit_base_factory TRUE: the base ThriftTransportFactory is set for
 *     both directions (with binary protocol factories, so the connection
 *     reaches the cleanup path). The base factory must return an owned
 *     reference for the server's per-connection releases to balance.
 *   - explicit_base_factory FALSE: no factories are supplied, so the server
 *     must fall back to a default transport and protocol factory on its own.
 *
 * The server child makes GLib CRITICAL messages fatal, so an unbalanced
 * reference count on the accepted transport does not pass silently. */
static void
run_lifetime_case (gboolean explicit_base_factory, gboolean send_payload)
{
  const guint num_connections = 3;
  int status;
  pid_t pid;
  guint i;
  TestProcessor *p = NULL;
  TestServerSocket *tss = NULL;
  ThriftSimpleServer *ss = NULL;
  ThriftTransportFactory *tf_in = NULL;
  ThriftTransportFactory *tf_out = NULL;
  ThriftProtocolFactory *pf_in = NULL;
  ThriftProtocolFactory *pf_out = NULL;

  p = g_object_new (TEST_PROCESSOR_TYPE, NULL);
  tss = g_object_new (TEST_TYPE_SERVER_SOCKET, "port", TEST_PORT, NULL);

  if (explicit_base_factory)
  {
    tf_in = g_object_new (THRIFT_TYPE_TRANSPORT_FACTORY, NULL);
    tf_out = g_object_new (THRIFT_TYPE_TRANSPORT_FACTORY, NULL);
    pf_in = g_object_new (THRIFT_TYPE_BINARY_PROTOCOL_FACTORY, NULL);
    pf_out = g_object_new (THRIFT_TYPE_BINARY_PROTOCOL_FACTORY, NULL);
    ss = g_object_new (THRIFT_TYPE_SIMPLE_SERVER, "processor", p,
                       "server_transport", THRIFT_SERVER_TRANSPORT (tss),
                       "input_transport_factory", tf_in,
                       "output_transport_factory", tf_out,
                       "input_protocol_factory", pf_in,
                       "output_protocol_factory", pf_out, NULL);
  }
  else
  {
    ss = g_object_new (THRIFT_TYPE_SIMPLE_SERVER, "processor", p,
                       "server_transport", THRIFT_SERVER_TRANSPORT (tss), NULL);
  }

  p->server = THRIFT_SERVER (ss);
  p->max = num_connections;

  pid = fork ();
  g_assert (pid >= 0);

  if (pid == 0)
  {
    GError *serve_error = NULL;

    g_log_set_always_fatal (G_LOG_LEVEL_CRITICAL);
    sockets_released = 0;

    thrift_server_serve (THRIFT_SERVER (ss), &serve_error);
    g_clear_error (&serve_error);

    status = (sockets_released == num_connections) ? 0 : 1;

    g_object_unref (ss);
    g_object_unref (tss);
    g_object_unref (p);
    if (tf_in != NULL)
      g_object_unref (tf_in);
    if (tf_out != NULL)
      g_object_unref (tf_out);
    if (pf_in != NULL)
      g_object_unref (pf_in);
    if (pf_out != NULL)
      g_object_unref (pf_out);

    exit (status);
  }
  else
  {
    for (i = 0; i < num_connections; i++)
      client_connect_and_close (send_payload);

    g_object_unref (ss);
    g_object_unref (tss);
    g_object_unref (p);
    if (tf_in != NULL)
      g_object_unref (tf_in);
    if (tf_out != NULL)
      g_object_unref (tf_out);
    if (pf_in != NULL)
      g_object_unref (pf_in);
    if (pf_out != NULL)
      g_object_unref (pf_out);

    g_assert (waitpid (pid, &status, 0) == pid);
    g_assert (WIFEXITED (status));
    g_assert_cmpint (WEXITSTATUS (status), ==, 0);
  }
}

static void
test_base_factory_lifetime (void)
{
  run_lifetime_case (TRUE, FALSE);
}

static void
test_base_factory_payload (void)
{
  run_lifetime_case (TRUE, TRUE);
}

static void
test_default_factory_lifetime (void)
{
  run_lifetime_case (FALSE, FALSE);
}

static void
test_default_factory_payload (void)
{
  run_lifetime_case (FALSE, TRUE);
}

int
main(int argc, char *argv[])
{
#if (!GLIB_CHECK_VERSION (2, 36, 0))
  g_type_init();
#endif

  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/testsimpleserver/SimpleServer", test_server);
  g_test_add_func ("/testsimpleserver/BaseFactoryTransportLifetime",
                   test_base_factory_lifetime);
  g_test_add_func ("/testsimpleserver/BaseFactoryTransportLifetimeWithPayload",
                   test_base_factory_payload);
  g_test_add_func ("/testsimpleserver/DefaultFactoryTransportLifetime",
                   test_default_factory_lifetime);
  g_test_add_func ("/testsimpleserver/DefaultFactoryTransportLifetimeWithPayload",
                   test_default_factory_payload);

  return g_test_run ();
}
