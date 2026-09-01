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

/* A transport's read() answers with the number of bytes it put in the caller's
 * buffer.  The zlib transport can produce fewer bytes than were asked for --
 * the compressed stream ends, or the transport underneath has nothing more --
 * and in that case the answer has to be the number produced, not the number
 * requested.  A caller that is told its buffer was filled goes on to parse
 * whatever was already in the part that was not written.
 *
 * The C++ implementation this one was ported from returns `len - need` at
 * every one of those exits (TZlibTransport.cpp:155-184).
 */

#include <glib-object.h>
#include <string.h>
#include <zlib.h>

#include <thrift/c_glib/thrift.h>
#include <thrift/c_glib/transport/thrift_transport.h>
#include <thrift/c_glib/transport/thrift_memory_buffer.h>
#include <thrift/c_glib/transport/thrift_zlib_transport.h>

#define PAYLOAD_BYTES 11
#define SENTINEL 0x5a

/* Builds a complete zlib stream holding PAYLOAD_BYTES bytes and returns a zlib
 * transport reading it out of a memory buffer.  compress() finishes the stream,
 * so inflate() reaches Z_STREAM_END while the caller is still asking for more:
 * exactly the premature end this is about. */
static ThriftTransport *
zlib_over_a_complete_stream (ThriftMemoryBuffer **buffer_out,
                             guchar *payload_out)
{
  ThriftMemoryBuffer *buffer;
  ThriftTransport *zlib_transport;
  guchar payload[PAYLOAD_BYTES];
  guchar *compressed;
  uLongf compressed_len;
  guint i;

  for (i = 0; i < PAYLOAD_BYTES; i++)
    payload[i] = (guchar) ('a' + i);
  memcpy (payload_out, payload, PAYLOAD_BYTES);

  compressed_len = compressBound (PAYLOAD_BYTES);
  compressed = g_malloc (compressed_len);
  g_assert_cmpint (compress (compressed, &compressed_len,
                             payload, PAYLOAD_BYTES), ==, Z_OK);

  buffer = g_object_new (THRIFT_TYPE_MEMORY_BUFFER,
                         "buf_size", (guint32) compressed_len,
                         NULL);
  g_assert (thrift_transport_write (THRIFT_TRANSPORT (buffer), compressed,
                                    (guint32) compressed_len, NULL) == TRUE);
  g_free (compressed);

  zlib_transport = g_object_new (THRIFT_TYPE_ZLIB_TRANSPORT,
                                 "transport", THRIFT_TRANSPORT (buffer),
                                 NULL);

  *buffer_out = buffer;
  return zlib_transport;
}

/* Asking for more than the stream holds must be answered with what the stream
 * held. */
static void
test_read_past_the_end_of_the_stream_reports_what_it_produced (void)
{
  ThriftMemoryBuffer *buffer = NULL;
  ThriftTransport *zlib_transport;
  guchar payload[PAYLOAD_BYTES];
  guchar got[PAYLOAD_BYTES * 4];
  gint32 ret;

  zlib_transport = zlib_over_a_complete_stream (&buffer, payload);

  memset (got, SENTINEL, sizeof (got));
  ret = thrift_transport_read (zlib_transport, got, sizeof (got), NULL);

  g_assert_cmpint (ret, ==, PAYLOAD_BYTES);

  g_object_unref (zlib_transport);
  g_object_unref (buffer);
}

/* And the bytes it did not produce must still be the caller's own: a read that
 * claims a full buffer hands the caller whatever was in it before. */
static void
test_read_past_the_end_does_not_claim_bytes_it_never_wrote (void)
{
  ThriftMemoryBuffer *buffer = NULL;
  ThriftTransport *zlib_transport;
  guchar payload[PAYLOAD_BYTES];
  guchar got[PAYLOAD_BYTES * 4];
  gint32 ret;
  guint i;

  zlib_transport = zlib_over_a_complete_stream (&buffer, payload);

  memset (got, SENTINEL, sizeof (got));
  ret = thrift_transport_read (zlib_transport, got, sizeof (got), NULL);

  g_assert_cmpint (ret, >=, 0);
  g_assert_cmpint ((guint32) ret, <=, sizeof (got));
  g_assert_cmpint (memcmp (got, payload, PAYLOAD_BYTES), ==, 0);

  /* The stream held PAYLOAD_BYTES, so nothing past that was written -- the
     buffer there still holds what the caller put in it. */
  for (i = PAYLOAD_BYTES; i < sizeof (got); i++)
    g_assert_cmpint (got[i], ==, SENTINEL);

  /* Which is why the count must not reach past it.  A caller told that its
     whole buffer was filled goes on to read those untouched bytes as if the
     peer had sent them. */
  g_assert_cmpint (ret, <=, PAYLOAD_BYTES);

  g_object_unref (zlib_transport);
  g_object_unref (buffer);
}

/* A read that fits inside the stream is unaffected. */
static void
test_read_within_the_stream_is_unchanged (void)
{
  ThriftMemoryBuffer *buffer = NULL;
  ThriftTransport *zlib_transport;
  guchar payload[PAYLOAD_BYTES];
  guchar got[PAYLOAD_BYTES];
  gint32 ret;

  zlib_transport = zlib_over_a_complete_stream (&buffer, payload);

  memset (got, SENTINEL, sizeof (got));
  ret = thrift_transport_read (zlib_transport, got, PAYLOAD_BYTES - 4, NULL);

  g_assert_cmpint (ret, ==, PAYLOAD_BYTES - 4);
  g_assert_cmpint (memcmp (got, payload, PAYLOAD_BYTES - 4), ==, 0);
  g_assert_cmpint (got[PAYLOAD_BYTES - 4], ==, SENTINEL);

  g_object_unref (zlib_transport);
  g_object_unref (buffer);
}

int
main (int argc, char *argv[])
{
#if (!GLIB_CHECK_VERSION (2, 36, 0))
  g_type_init ();
#endif

  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/testzlibshortread/ReportsWhatItProduced",
                   test_read_past_the_end_of_the_stream_reports_what_it_produced);
  g_test_add_func ("/testzlibshortread/DoesNotClaimUnwrittenBytes",
                   test_read_past_the_end_does_not_claim_bytes_it_never_wrote);
  g_test_add_func ("/testzlibshortread/ReadWithinTheStreamIsUnchanged",
                   test_read_within_the_stream_is_unchanged);

  return g_test_run ();
}
