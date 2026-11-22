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
#include <time.h>

#include <thrift/c_glib/transport/thrift_transport.h>
#include <thrift/c_glib/transport/thrift_socket.h>
#include <thrift/c_glib/transport/thrift_server_transport.h>
#include <thrift/c_glib/transport/thrift_server_socket.h>

static const gchar TEST_DATA[11] = "abcdefghij";

#include "../src/thrift/c_glib/transport/thrift_memory_buffer.c"

/* test object creation and destruction */
static void
test_create_and_destroy (void)
{
  GObject *object = NULL;
  object = g_object_new (THRIFT_TYPE_MEMORY_BUFFER,
                         "buf_size", 10,
                         NULL);
  g_assert (object != NULL);
  g_object_unref (object);
}

static void
test_create_and_destroy_large (void)
{
  GObject *object = NULL;
  object = g_object_new (THRIFT_TYPE_MEMORY_BUFFER,
                         "buf_size", 10 * 1024 * 1024,
                         NULL);
  g_assert (object != NULL);
  g_object_unref (object);
}

static void
test_create_and_destroy_default (void)
{
  GObject *object = NULL;
  object = g_object_new (THRIFT_TYPE_MEMORY_BUFFER, NULL);
  g_assert (object != NULL);
  g_object_unref (object);
}

static void
test_create_and_destroy_external (void)
{
  GObject *object = NULL;
  GByteArray *buf = g_byte_array_new ();
  g_assert (buf != NULL);
  object = g_object_new (THRIFT_TYPE_MEMORY_BUFFER,
                         "buf", buf,
                         NULL);
  g_assert (object != NULL);
  g_object_unref (object);
}

static void
test_create_and_destroy_unowned (void)
{
  GObject *object = NULL;
  GValue val = G_VALUE_INIT;
  GByteArray *buf;

  object = g_object_new (THRIFT_TYPE_MEMORY_BUFFER,
                         "owner", FALSE,
                         NULL);
  g_assert (object != NULL);

  g_value_init (&val, G_TYPE_POINTER);
  g_object_get_property (object, "buf", &val);
  buf = (GByteArray*) g_value_get_pointer (&val);
  g_assert (buf != NULL);

  g_byte_array_unref (buf);
  g_value_unset (&val);
  g_object_unref (object);
}

static void
test_open_and_close (void)
{
  ThriftMemoryBuffer *tbuffer = NULL;

  /* create a ThriftMemoryBuffer */
  tbuffer = g_object_new (THRIFT_TYPE_MEMORY_BUFFER, NULL);

  /* no-ops */
  g_assert (thrift_memory_buffer_open (THRIFT_TRANSPORT (tbuffer), NULL) == TRUE);
  g_assert (thrift_memory_buffer_is_open (THRIFT_TRANSPORT (tbuffer)) == TRUE);
  g_assert (thrift_memory_buffer_close (THRIFT_TRANSPORT (tbuffer), NULL) == TRUE);

  g_object_unref (tbuffer);
}

static void
test_read_and_write (void)
{
  ThriftMemoryBuffer *tbuffer = NULL;
  gint got, want;
  gchar read[10];
  gchar *b;
  GError *error = NULL;

  tbuffer = g_object_new (THRIFT_TYPE_MEMORY_BUFFER, "buf_size", 5, NULL);
  g_assert (thrift_memory_buffer_write (THRIFT_TRANSPORT (tbuffer),
                                      (gpointer) TEST_DATA,
                                      10, &error) == FALSE);
  g_assert (error != NULL);
  g_error_free (error);
  error = NULL;
  g_object_unref (tbuffer);

  tbuffer = g_object_new (THRIFT_TYPE_MEMORY_BUFFER, "buf_size", 15, NULL);
  g_assert (thrift_memory_buffer_write (THRIFT_TRANSPORT (tbuffer),
                                      (gpointer) TEST_DATA, 10, &error) == TRUE);
  g_assert (error == NULL);

  memset(read, 0, 10);
  b = read;
  want = 10;
  while (want > 0) {
    got = thrift_memory_buffer_read (THRIFT_TRANSPORT (tbuffer),
                                     (gpointer) b, want, &error);
    g_assert (got > 0 && got <= want);
    g_assert (error == NULL);
    b += got;
    want -= got;
  }
  g_assert (memcmp (read, TEST_DATA, 10) == 0);
  g_object_unref (tbuffer);
}

static void
test_read_and_write_default (void)
{
  ThriftMemoryBuffer *tbuffer = NULL;
  gint got, want, i;
  gchar read[10];
  gchar *b;
  GError *error = NULL;

  tbuffer = g_object_new (THRIFT_TYPE_MEMORY_BUFFER, NULL);
  for (i = 0; i < 100; ++i) {
    g_assert (thrift_memory_buffer_write (THRIFT_TRANSPORT (tbuffer),
                                        (gpointer) TEST_DATA, 10, &error) == TRUE);
    g_assert (error == NULL);
  }

  for (i = 0; i < 100; ++i) {
    memset(read, 0, 10);
    b = read;
    want = 10;
    while (want > 0) {
      got = thrift_memory_buffer_read (THRIFT_TRANSPORT (tbuffer),
                                       (gpointer) b, want, &error);
      g_assert (got > 0 && got <= want);
      g_assert (error == NULL);
      b += got;
      want -= got;
    }
    g_assert (memcmp (read, TEST_DATA, 10) == 0);
  }
  g_object_unref (tbuffer);
}

static void
test_read_and_write_external (void)
{
  ThriftMemoryBuffer *tbuffer = NULL;
  GError *error = NULL;
  GByteArray *buf = g_byte_array_new ();
  g_assert (buf != NULL);

  tbuffer = g_object_new (THRIFT_TYPE_MEMORY_BUFFER, "buf", buf, NULL);
  g_assert (thrift_memory_buffer_write (THRIFT_TRANSPORT (tbuffer),
                                      (gpointer) TEST_DATA, 10, &error) == TRUE);
  g_assert (error == NULL);

  g_assert (memcmp (buf->data, TEST_DATA, 10) == 0);
  g_object_unref (tbuffer);
}

/* Reading a buffer a few bytes at a time hands out every byte once and in
   order; once everything has been read, the buffer is empty. */
static void
test_read_in_pieces (void)
{
  static const guint32 pieces[] = { 1, 2, 3, 5, 8, 13, 21 };
  ThriftMemoryBuffer *tbuffer = g_object_new (THRIFT_TYPE_MEMORY_BUFFER, NULL);
  guchar data[10000];
  guchar buf[21];
  guint32 done = 0;
  guint i, n = 0;

  for (i = 0; i < sizeof (data); i++)
    data[i] = (guchar) (i * 7 + i / 256);
  g_assert (thrift_memory_buffer_write (THRIFT_TRANSPORT (tbuffer), data,
                                        sizeof (data), NULL) == TRUE);

  while (done < sizeof (data))
    {
      guint32 want = MIN (pieces[n++ % G_N_ELEMENTS (pieces)],
                          sizeof (data) - done);

      g_assert_cmpint (thrift_memory_buffer_read (THRIFT_TRANSPORT (tbuffer),
                                                  buf, want, NULL),
                       ==, (gint32) want);
      g_assert (memcmp (buf, data + done, want) == 0);
      done += want;
    }
  g_assert_cmpuint (tbuffer->buf->len, ==, 0);
  g_assert_cmpint (thrift_memory_buffer_read (THRIFT_TRANSPORT (tbuffer),
                                              buf, 1, NULL), ==, -1);

  g_object_unref (tbuffer);
}

/* Bytes written between reads come after the ones not read yet. */
static void
test_writes_between_reads (void)
{
  ThriftMemoryBuffer *tbuffer = g_object_new (THRIFT_TYPE_MEMORY_BUFFER, NULL);
  guchar data[6000];
  guchar buf[7];
  guint32 written = 3000;
  guint32 done = 0;
  guint i;

  for (i = 0; i < sizeof (data); i++)
    data[i] = (guchar) (i * 13 + i / 256);
  g_assert (thrift_memory_buffer_write (THRIFT_TRANSPORT (tbuffer), data,
                                        written, NULL) == TRUE);

  while (done < sizeof (data))
    {
      guint32 want = MIN (7, written - done);

      g_assert_cmpint (thrift_memory_buffer_read (THRIFT_TRANSPORT (tbuffer),
                                                  buf, want, NULL),
                       ==, (gint32) want);
      g_assert (memcmp (buf, data + done, want) == 0);
      done += want;

      if (written < sizeof (data))
        {
          guint32 more = MIN (5, sizeof (data) - written);

          g_assert (thrift_memory_buffer_write (THRIFT_TRANSPORT (tbuffer),
                                                data + written, more,
                                                NULL) == TRUE);
          written += more;
        }
    }

  g_object_unref (tbuffer);
}

/* The bytes that have been read do not count against buf_size. */
static void
test_read_bytes_leave_room_for_writes (void)
{
  ThriftMemoryBuffer *tbuffer = g_object_new (THRIFT_TYPE_MEMORY_BUFFER,
                                              "buf_size", 8, NULL);
  GError *error = NULL;
  gchar buf[8];

  g_assert (thrift_memory_buffer_write (THRIFT_TRANSPORT (tbuffer),
                                        (gpointer) TEST_DATA, 8,
                                        NULL) == TRUE);
  g_assert_cmpint (thrift_memory_buffer_read (THRIFT_TRANSPORT (tbuffer),
                                              buf, 2, NULL), ==, 2);
  g_assert (memcmp (buf, TEST_DATA, 2) == 0);

  /* six bytes are left to read, so two more fit, and no more than that */
  g_assert (thrift_memory_buffer_write (THRIFT_TRANSPORT (tbuffer),
                                        (gpointer) TEST_DATA, 2,
                                        NULL) == TRUE);
  g_assert (thrift_memory_buffer_write (THRIFT_TRANSPORT (tbuffer),
                                        (gpointer) TEST_DATA, 1,
                                        &error) == FALSE);
  g_assert (error != NULL);
  g_clear_error (&error);

  g_assert_cmpint (thrift_memory_buffer_read (THRIFT_TRANSPORT (tbuffer),
                                              buf, 8, NULL), ==, 8);
  g_assert (memcmp (buf, TEST_DATA + 2, 6) == 0);
  g_assert (memcmp (buf + 6, TEST_DATA, 2) == 0);

  g_object_unref (tbuffer);
}

/* A buffer that is read as fast as it is written holds what has not been
   read yet; the bytes read so far do not pile up in it. */
static void
test_rolling_use_keeps_the_buffer_small (void)
{
  ThriftMemoryBuffer *tbuffer = g_object_new (THRIFT_TYPE_MEMORY_BUFFER, NULL);
  gchar buf[4];
  guint i;

  g_assert (thrift_memory_buffer_write (THRIFT_TRANSPORT (tbuffer),
                                        (gpointer) TEST_DATA, 8,
                                        NULL) == TRUE);
  for (i = 0; i < 100000; i++)
    {
      g_assert (thrift_memory_buffer_write (THRIFT_TRANSPORT (tbuffer),
                                            (gpointer) TEST_DATA, 4,
                                            NULL) == TRUE);
      g_assert_cmpint (thrift_memory_buffer_read (THRIFT_TRANSPORT (tbuffer),
                                                  buf, 4, NULL), ==, 4);
    }

  /* eight bytes are not read yet */
  g_assert_cmpuint (tbuffer->buf->len, <=, 64);

  g_object_unref (tbuffer);
}

/* A caller that passed its own array in may shorten it between reads.  The
   next read then starts at the front of what the array holds now, and does
   not reach past its end. */
static void
test_shortened_external_array (void)
{
  GByteArray *buf = g_byte_array_new ();
  ThriftMemoryBuffer *tbuffer;
  guchar data[100];
  guchar out[16];
  guint i;

  for (i = 0; i < sizeof (data); i++)
    data[i] = (guchar) i;
  tbuffer = g_object_new (THRIFT_TYPE_MEMORY_BUFFER, "buf", buf, NULL);
  g_assert (thrift_memory_buffer_write (THRIFT_TRANSPORT (tbuffer), data,
                                        sizeof (data), NULL) == TRUE);
  g_assert_cmpint (thrift_memory_buffer_read (THRIFT_TRANSPORT (tbuffer),
                                              out, 10, NULL), ==, 10);

  g_byte_array_set_size (buf, 0);
  g_byte_array_append (buf, (const guint8 *) TEST_DATA, 5);

  g_assert_cmpint (thrift_memory_buffer_read (THRIFT_TRANSPORT (tbuffer),
                                              out, sizeof (out), NULL), ==, 5);
  g_assert (memcmp (out, TEST_DATA, 5) == 0);
  g_assert_cmpint (thrift_memory_buffer_read (THRIFT_TRANSPORT (tbuffer),
                                              out, 1, NULL), ==, -1);

  g_object_unref (tbuffer);
}

/* the CPU time this process has used, in seconds */
static gdouble
cpu_seconds (void)
{
  struct timespec now;

  g_assert (clock_gettime (CLOCK_PROCESS_CPUTIME_ID, &now) == 0);
  return (gdouble) now.tv_sec + (gdouble) now.tv_nsec / 1e9;
}

/* reads a buffer of size bytes back one byte at a time and returns the CPU
   time that took */
static gdouble
time_bytewise_read (guint32 size)
{
  ThriftMemoryBuffer *tbuffer = g_object_new (THRIFT_TYPE_MEMORY_BUFFER, NULL);
  guchar *data = g_malloc0 (size);
  gdouble start, elapsed;
  guchar byte;
  guint32 i;

  g_assert (thrift_memory_buffer_write (THRIFT_TRANSPORT (tbuffer), data,
                                        size, NULL) == TRUE);
  g_free (data);

  start = cpu_seconds ();
  for (i = 0; i < size; i++)
    {
      if (thrift_memory_buffer_read (THRIFT_TRANSPORT (tbuffer), &byte, 1,
                                     NULL) != 1)
        g_error ("reading byte %u of %u failed", i, size);
    }
  elapsed = cpu_seconds () - start;

  g_object_unref (tbuffer);
  return elapsed;
}

/* A protocol reads in many small pieces.  Reading eight times the bytes that
   way should take about eight times as long.  The smaller run is made long
   enough to be timed, the larger one gets a few tries so that a busy machine
   does not decide the outcome, and three times the linear ratio is
   allowed. */
static void
test_bytewise_reads_take_time_in_proportion_to_the_buffer (void)
{
  guint32 size = 64 * 1024;
  gdouble base;
  gdouble larger = 0.0;
  guint attempt;

  base = time_bytewise_read (size);
  while (base < 0.002 && size < 1024 * 1024)
    {
      size *= 2;
      base = time_bytewise_read (size);
    }
  g_assert_cmpfloat (base, >, 0.0);

  for (attempt = 0; attempt < 3; attempt++)
    {
      larger = time_bytewise_read (8 * size);
      g_test_message ("%u bytes: %.4f s, %u bytes: %.4f s (x%.1f)",
                      size, base, 8 * size, larger, larger / base);
      if (larger <= 24 * base)
        break;
    }
  g_assert_cmpfloat (larger, <=, 24 * base);
}

int
main(int argc, char *argv[])
{
#if (!GLIB_CHECK_VERSION (2, 36, 0))
  g_type_init ();
#endif

  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/testmemorybuffer/CreateAndDestroy", test_create_and_destroy);
  g_test_add_func ("/testmemorybuffer/CreateAndDestroyLarge", test_create_and_destroy_large);
  g_test_add_func ("/testmemorybuffer/CreateAndDestroyUnlimited", test_create_and_destroy_default);
  g_test_add_func ("/testmemorybuffer/CreateAndDestroyExternal", test_create_and_destroy_external);
  g_test_add_func ("/testmemorybuffer/CreateAndDestroyUnowned", test_create_and_destroy_unowned);
  g_test_add_func ("/testmemorybuffer/OpenAndClose", test_open_and_close);
  g_test_add_func ("/testmemorybuffer/ReadAndWrite", test_read_and_write);
  g_test_add_func ("/testmemorybuffer/ReadAndWriteUnlimited", test_read_and_write_default);
  g_test_add_func ("/testmemorybuffer/ReadAndWriteExternal", test_read_and_write_external);
  g_test_add_func ("/testmemorybuffer/ReadInPieces", test_read_in_pieces);
  g_test_add_func ("/testmemorybuffer/WritesBetweenReads", test_writes_between_reads);
  g_test_add_func ("/testmemorybuffer/ReadBytesLeaveRoomForWrites",
                   test_read_bytes_leave_room_for_writes);
  g_test_add_func ("/testmemorybuffer/RollingUseKeepsTheBufferSmall",
                   test_rolling_use_keeps_the_buffer_small);
  g_test_add_func ("/testmemorybuffer/ShortenedExternalArray",
                   test_shortened_external_array);
  g_test_add_func ("/testmemorybuffer/BytewiseReadsTakeTimeInProportionToTheBuffer",
                   test_bytewise_reads_take_time_in_proportion_to_the_buffer);

  return g_test_run ();
}
