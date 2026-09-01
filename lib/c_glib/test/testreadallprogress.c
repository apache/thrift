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

/* thrift_transport_read_all() fills a caller buffer by asking the transport's
 * read() for the bytes it still needs.  A read() that produces no bytes is not
 * progress, and repeating it cannot become progress: the loop has to stop.
 *
 * Two transports could hand read_all() a zero.  thrift_fd_transport_read()
 * returned read(2)'s 0 at end of file, and thrift_ssl_socket_read() returned
 * SSL_read()'s 0 on a closed session.  thrift_socket_read() has always treated
 * a non-positive result as an error, and that is the behaviour the other two
 * should share.
 *
 * The two loop tests run as subprocesses with a time limit, so a loop that
 * fails to terminate is reported as a failing test rather than hanging the
 * suite.
 */

#include <errno.h>
#include <fcntl.h>
#include <glib-object.h>
#include <glib/gstdio.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include <thrift/c_glib/thrift.h>
#include <thrift/c_glib/transport/thrift_transport.h>
#include <thrift/c_glib/transport/thrift_fd_transport.h>

#define TEST_TIMEOUT_US (10 * G_USEC_PER_SEC)

/* A transport whose read() always reports that it read nothing, without
 * touching the caller's buffer and without setting an error.  This is the
 * shape a cleanly closed peer presents to read_all(). */

#define TEST_TYPE_STALLED_TRANSPORT (test_stalled_transport_get_type ())

struct _TestStalledTransport
{
  ThriftTransport parent;
  guint read_calls;
};
typedef struct _TestStalledTransport TestStalledTransport;

struct _TestStalledTransportClass
{
  ThriftTransportClass parent;
};
typedef struct _TestStalledTransportClass TestStalledTransportClass;

GType test_stalled_transport_get_type (void);

G_DEFINE_TYPE (TestStalledTransport, test_stalled_transport,
               THRIFT_TYPE_TRANSPORT)

static gboolean
test_stalled_transport_is_open (ThriftTransport *transport)
{
  THRIFT_UNUSED_VAR (transport);
  return TRUE;
}

static gboolean
test_stalled_transport_peek (ThriftTransport *transport, GError **error)
{
  THRIFT_UNUSED_VAR (transport);
  THRIFT_UNUSED_VAR (error);
  return TRUE;
}

static gboolean
test_stalled_transport_open (ThriftTransport *transport, GError **error)
{
  THRIFT_UNUSED_VAR (transport);
  THRIFT_UNUSED_VAR (error);
  return TRUE;
}

static gboolean
test_stalled_transport_close (ThriftTransport *transport, GError **error)
{
  THRIFT_UNUSED_VAR (transport);
  THRIFT_UNUSED_VAR (error);
  return TRUE;
}

static gint32
test_stalled_transport_read (ThriftTransport *transport, gpointer buf,
                             guint32 len, GError **error)
{
  THRIFT_UNUSED_VAR (buf);
  THRIFT_UNUSED_VAR (len);
  THRIFT_UNUSED_VAR (error);

  ((TestStalledTransport *) transport)->read_calls++;
  return 0;
}

static gboolean
test_stalled_transport_read_end (ThriftTransport *transport, GError **error)
{
  THRIFT_UNUSED_VAR (transport);
  THRIFT_UNUSED_VAR (error);
  return TRUE;
}

static gboolean
test_stalled_transport_write (ThriftTransport *transport, const gpointer buf,
                              const guint32 len, GError **error)
{
  THRIFT_UNUSED_VAR (transport);
  THRIFT_UNUSED_VAR (buf);
  THRIFT_UNUSED_VAR (len);
  THRIFT_UNUSED_VAR (error);
  return TRUE;
}

static gboolean
test_stalled_transport_write_end (ThriftTransport *transport, GError **error)
{
  THRIFT_UNUSED_VAR (transport);
  THRIFT_UNUSED_VAR (error);
  return TRUE;
}

static gboolean
test_stalled_transport_flush (ThriftTransport *transport, GError **error)
{
  THRIFT_UNUSED_VAR (transport);
  THRIFT_UNUSED_VAR (error);
  return TRUE;
}

static void
test_stalled_transport_init (TestStalledTransport *transport)
{
  transport->read_calls = 0;
}

static void
test_stalled_transport_class_init (TestStalledTransportClass *klass)
{
  ThriftTransportClass *ttc = THRIFT_TRANSPORT_CLASS (klass);

  ttc->is_open = test_stalled_transport_is_open;
  ttc->peek = test_stalled_transport_peek;
  ttc->open = test_stalled_transport_open;
  ttc->close = test_stalled_transport_close;
  ttc->read = test_stalled_transport_read;
  ttc->read_end = test_stalled_transport_read_end;
  ttc->write = test_stalled_transport_write;
  ttc->write_end = test_stalled_transport_write_end;
  ttc->flush = test_stalled_transport_flush;
}

/* read_all() over a source that never produces a byte must give up with an
 * error instead of asking again forever. */
static void
read_all_over_a_stalled_source (void)
{
  ThriftTransport *transport;
  GError *error = NULL;
  gchar buf[16];
  gint32 got;

  transport = THRIFT_TRANSPORT (g_object_new (TEST_TYPE_STALLED_TRANSPORT,
                                              NULL));

  got = thrift_transport_read_all (transport, buf, sizeof (buf), &error);

  g_assert_cmpint (got, ==, -1);
  g_assert (error != NULL);
  g_error_free (error);

  g_object_unref (transport);
}

static void
test_read_all_stops_when_the_source_makes_no_progress (void)
{
  if (g_test_subprocess ())
    {
      read_all_over_a_stalled_source ();
      return;
    }

  g_test_trap_subprocess (NULL, TEST_TIMEOUT_US, 0);
  g_test_trap_assert_passed ();
}

/* The fd transport's read() at end of file: read(2) returns 0, which is not a
 * short read the caller can retry.  Report it the way thrift_socket_read()
 * reports a closed peer. */
static void
test_fd_transport_read_reports_end_of_file (void)
{
  ThriftTransport *transport;
  GError *error = NULL;
  gchar buf[8];
  gint fd;
  gchar *filename = NULL;
  gint32 got;

  fd = g_file_open_tmp (NULL, &filename, &error);
  g_assert (fd >= 0);
  g_assert_no_error (error);

  /* Nothing was written, so the very first read is at end of file. */
  transport = THRIFT_TRANSPORT (g_object_new (THRIFT_TYPE_FD_TRANSPORT,
                                              "fd", fd,
                                              NULL));

  got = thrift_transport_read (transport, buf, sizeof (buf), &error);

  g_assert_cmpint (got, ==, -1);
  g_assert (error != NULL);
  g_clear_error (&error);

  g_object_unref (transport);

  close (fd);
  g_remove (filename);
  g_free (filename);
}

/* The same source driven through read_all(): a file holding fewer bytes than
 * asked for must end the call, not spin on the zero that follows. */
static void
fd_transport_read_all_past_the_end (void)
{
  ThriftTransport *transport;
  GError *error = NULL;
  gchar buf[64];
  gint fd;
  gchar *filename = NULL;
  gint32 got;

  fd = g_file_open_tmp (NULL, &filename, &error);
  g_assert (fd >= 0);
  g_assert_no_error (error);

  g_assert_cmpint (write (fd, "partial", 7), ==, 7);
  g_assert_cmpint (lseek (fd, 0, SEEK_SET), ==, 0);

  transport = THRIFT_TRANSPORT (g_object_new (THRIFT_TYPE_FD_TRANSPORT,
                                              "fd", fd,
                                              NULL));

  got = thrift_transport_read_all (transport, buf, sizeof (buf), &error);

  g_assert_cmpint (got, ==, -1);
  g_assert (error != NULL);
  g_clear_error (&error);

  g_object_unref (transport);

  close (fd);
  g_remove (filename);
  g_free (filename);
}

static void
test_fd_transport_read_all_stops_at_end_of_file (void)
{
  if (g_test_subprocess ())
    {
      fd_transport_read_all_past_the_end ();
      return;
    }

  g_test_trap_subprocess (NULL, TEST_TIMEOUT_US, 0);
  g_test_trap_assert_passed ();
}

int
main (int argc, char *argv[])
{
#if (!GLIB_CHECK_VERSION (2, 36, 0))
  g_type_init ();
#endif

  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/testreadallprogress/ReadAllStopsWithoutProgress",
                   test_read_all_stops_when_the_source_makes_no_progress);
  g_test_add_func ("/testreadallprogress/FdReadReportsEndOfFile",
                   test_fd_transport_read_reports_end_of_file);
  g_test_add_func ("/testreadallprogress/FdReadAllStopsAtEndOfFile",
                   test_fd_transport_read_all_stops_at_end_of_file);

  return g_test_run ();
}
