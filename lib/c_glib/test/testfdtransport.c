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


#include <string.h>
#include <fcntl.h>
#include <errno.h>
#include <unistd.h>

#include <glib.h>
#include <glib/gstdio.h>

#include <thrift/c_glib/transport/thrift_transport.h>
#include <thrift/c_glib/transport/thrift_fd_transport.h>

/* substituted function to test failures of system calls */
static int write_error = 0;
static int write_zero = 0;
static int write_calls = 0;
ssize_t
my_write(int fd, const void *buf, size_t count)
{
  write_calls++;
  if (write_zero > 0)
    {
      /* report that nothing was written, write_zero times in a row */
      write_zero--;
      return 0;
    }
  if (write_error == 0)
    {
      return write (fd, buf, count);
    }
  return -1;
}

/* function-like, so that the write member of the transport class is left
   alone */
#define write(fd, buf, count) my_write (fd, buf, count)
#include "../src/thrift/c_glib/transport/thrift_fd_transport.c"
#undef write

static const gchar TEST_DATA[12] = "abcde01234!";

static void
test_create_and_destroy (void)
{
  GObject *object;
  object = g_object_new (THRIFT_TYPE_FD_TRANSPORT, "fd", -1, NULL);
  g_assert (object != NULL);
  g_object_unref (object);
}

static void
test_open_and_close (void)
{
  ThriftTransport *transport;
  ThriftTransportClass *klass;
  GError *error;
  gint fd;
  gchar *filename;

  error = NULL;
  filename = NULL;

  fd = g_file_open_tmp (NULL, &filename, &error);
  g_assert (fd >= 0);

  transport = THRIFT_TRANSPORT (g_object_new (THRIFT_TYPE_FD_TRANSPORT,
                                              "fd", fd,
                                              NULL));
  klass = THRIFT_TRANSPORT_GET_CLASS (transport);

  /* open is no-op */
  g_assert (klass->is_open (transport));
  g_assert (klass->peek (transport, &error));
  g_assert (klass->open (transport, &error));
  g_assert (klass->is_open (transport));
  g_assert (klass->peek (transport, &error));

  g_assert (klass->close (transport, &error));
  g_assert (! klass->open (transport, &error));
  g_assert (! klass->is_open (transport));
  g_assert (! klass->peek (transport, &error));

  /* already closed */
  g_assert (close (fd) != 0);
  g_assert (errno == EBADF);

  g_object_unref (transport);

  g_remove (filename);
  g_free (filename);

  /* test bad fd */
  transport = THRIFT_TRANSPORT (g_object_new (THRIFT_TYPE_FD_TRANSPORT,
                                              "fd", -1,
                                              NULL));
  klass = THRIFT_TRANSPORT_GET_CLASS (transport);

  g_assert (! klass->is_open (transport));
  error = NULL;
  g_assert (! klass->peek (transport, &error));
  error = NULL;
  g_assert (! klass->open (transport, &error));
  error = NULL;
  g_assert (! klass->close (transport, &error));

  g_object_unref (transport);
}

static void
test_read_and_write (void)
{
  gchar out_buf[8];
  gchar *b;
  gint want, got;
  ThriftTransport *transport;
  ThriftTransportClass *klass;
  GError *error;
  gint fd;
  gchar *filename;

  error = NULL;
  filename = NULL;

  fd = g_file_open_tmp (NULL, &filename, &error);
  g_assert (fd >= 0);

  /* write */
  transport = THRIFT_TRANSPORT (g_object_new (THRIFT_TYPE_FD_TRANSPORT,
                                              "fd", fd,
                                              NULL));
  klass = THRIFT_TRANSPORT_GET_CLASS (transport);
  g_assert (klass->is_open (transport));
  g_assert (klass->write (transport, (gpointer) TEST_DATA, 11, &error));
  g_assert (klass->flush (transport, &error));
  g_assert (klass->close (transport, &error));
  g_object_unref (transport);

  /* read */
  fd = open(filename, O_RDONLY, S_IRUSR | S_IWUSR);
  g_assert (fd >= 0);

  transport = THRIFT_TRANSPORT (g_object_new (THRIFT_TYPE_FD_TRANSPORT,
                                              "fd", fd,
                                              NULL));
  klass = THRIFT_TRANSPORT_GET_CLASS (transport);

  memset(out_buf, 0, 8);
  b = out_buf;
  want = 7;
  while (want > 0) {
    got = klass->read (transport, (gpointer) b, want, &error);
    g_assert (got > 0 && got <= want);
    b += got;
    want -= got;
  }
  g_assert (memcmp (out_buf, TEST_DATA, 7) == 0);

  memset(out_buf, 0, 8);
  b = out_buf;
  want = 4;
  while (want > 0) {
    got = klass->read (transport, (gpointer) b, want, &error);
    g_assert (got > 0 && got <= want);
    b += got;
    want -= got;
  }
  g_assert (memcmp (out_buf, TEST_DATA + 7, 4) == 0);

  g_assert (klass->close (transport, &error));
  g_object_unref (transport);

  /* clean up */

  g_remove (filename);
  g_free (filename);
}

/* write(2) answering 0 for a non-empty buffer has written nothing, and asking
   again cannot change that, so the write has to fail on the first 0.
   my_write() answers 0 a bounded number of times and then fails, so that a
   write which keeps asking still returns; the number of calls tells the two
   apart. */
static void
test_write_fails_when_write_returns_zero (void)
{
  ThriftTransport *transport;
  ThriftTransportClass *klass;
  GError *error;
  gint fd;
  gchar *filename;

  error = NULL;
  filename = NULL;

  fd = g_file_open_tmp (NULL, &filename, &error);
  g_assert (fd >= 0);

  transport = THRIFT_TRANSPORT (g_object_new (THRIFT_TYPE_FD_TRANSPORT,
                                              "fd", fd,
                                              NULL));
  klass = THRIFT_TRANSPORT_GET_CLASS (transport);

  write_calls = 0;
  write_zero = 100;
  write_error = 1;
  g_assert (! klass->write (transport, (gpointer) TEST_DATA, 11, &error));
  g_assert_cmpint (write_calls, ==, 1);
  g_assert_error (error, THRIFT_TRANSPORT_ERROR, THRIFT_TRANSPORT_ERROR_SEND);
  g_clear_error (&error);
  write_zero = 0;
  write_error = 0;

  g_assert (klass->close (transport, &error));
  g_object_unref (transport);

  g_remove (filename);
  g_free (filename);
}

int
main (int argc, char *argv[])
{
#if (!GLIB_CHECK_VERSION (2, 36, 0))
  g_type_init ();
#endif

  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/testfdtransport/CreateAndDestroy", test_create_and_destroy);
  g_test_add_func ("/testfdtransport/OpenAndClose", test_open_and_close);
  g_test_add_func ("/testfdtransport/ReadAndWrite", test_read_and_write);
  g_test_add_func ("/testfdtransport/WriteFailsWhenWriteReturnsZero", test_write_fails_when_write_returns_zero);

  return g_test_run ();
}
