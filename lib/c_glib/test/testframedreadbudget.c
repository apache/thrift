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

/* The read budget a framed transport offers to the protocol should describe
 * the frame it is currently handing out, not the configured maximum.  A
 * protocol asks checkReadBytesAvailable() before it sizes a container or a
 * string body from a number it has just read off the wire, and the answer is
 * only useful if it accounts for how much the frame can actually deliver.
 *
 * These tests drive a framed transport over a memory buffer holding
 * pre-built frames, which keeps them free of sockets, forks and timing.
 */

#include <netdb.h>

#include <thrift/c_glib/thrift_configuration.h>
#include <thrift/c_glib/transport/thrift_transport.h>
#include <thrift/c_glib/transport/thrift_memory_buffer.h>

#include "../src/thrift/c_glib/transport/thrift_framed_transport.c"

/* Writes count frames of the given payload sizes into a fresh memory buffer
 * and returns a framed transport reading from it.  The buffer is returned
 * through buffer_out so the caller can drop its reference afterwards. */
static ThriftTransport *
framed_over_frames (ThriftMemoryBuffer **buffer_out,
                    const guint32 *frame_sizes, guint count)
{
  ThriftMemoryBuffer *buffer = NULL;
  ThriftTransport *framed = NULL;
  guint32 total = 0;
  guint32 largest = 0;
  guchar *payload = NULL;
  guint i;

  for (i = 0; i < count; i++)
    {
      total += frame_sizes[i] + (guint32) sizeof (guint32);
      if (frame_sizes[i] > largest)
        largest = frame_sizes[i];
    }

  buffer = g_object_new (THRIFT_TYPE_MEMORY_BUFFER, "buf_size", total, NULL);

  payload = g_new0 (guchar, largest > 0 ? largest : 1);
  memset (payload, 'x', largest);

  for (i = 0; i < count; i++)
    {
      guint32 nbo = htonl (frame_sizes[i]);

      g_assert (thrift_transport_write (THRIFT_TRANSPORT (buffer), &nbo,
                                        sizeof (nbo), NULL) == TRUE);
      if (frame_sizes[i] > 0)
        g_assert (thrift_transport_write (THRIFT_TRANSPORT (buffer), payload,
                                          frame_sizes[i], NULL) == TRUE);
    }

  g_free (payload);

  framed = g_object_new (THRIFT_TYPE_FRAMED_TRANSPORT,
                         "transport", THRIFT_TRANSPORT (buffer), NULL);

  *buffer_out = buffer;
  return framed;
}

/* What the protocol asks before it allocates. */
static gboolean
may_read (ThriftTransport *transport, gint64 num_bytes)
{
  ThriftTransportClass *ttc = THRIFT_TRANSPORT_GET_CLASS (transport);
  GError *error = NULL;
  gboolean allowed;

  allowed = ttc->checkReadBytesAvailable (transport, num_bytes, &error);
  if (!allowed)
    {
      g_assert (error != NULL);
      g_error_free (error);
    }

  return allowed;
}

/* The defect: a small frame leaves the budget at the configured maximum, so a
 * protocol reading that frame can be talked into sizing an allocation from a
 * number the frame comes nowhere near carrying. */
static void
test_budget_is_bound_to_the_frame (void)
{
  const guint32 sizes[] = { 8 };
  ThriftMemoryBuffer *buffer = NULL;
  ThriftTransport *framed = NULL;
  guchar buf[8];

  framed = framed_over_frames (&buffer, sizes, 1);

  /* Read the message header out of the frame, as a protocol would. */
  g_assert (thrift_framed_transport_read (framed, buf, 4, NULL) == 4);

  /* Eight bytes arrived.  Anything past that cannot come out of this frame. */
  g_assert (may_read (framed, 8) == TRUE);
  g_assert (may_read (framed, 9) == FALSE);
  g_assert (may_read (framed, 1024 * 1024) == FALSE);

  g_object_unref (framed);
  g_object_unref (buffer);
}

/* Binding the budget must not leave it stuck at the smallest frame seen so
 * far: a budget is allowed to shrink but not to grow, so binding without
 * discarding the previous frame's bound first makes the next larger frame
 * unreadable. */
static void
test_a_larger_frame_may_follow_a_smaller_one (void)
{
  const guint32 sizes[] = { 4, 64 };
  ThriftMemoryBuffer *buffer = NULL;
  ThriftTransport *framed = NULL;
  guchar buf[64];

  framed = framed_over_frames (&buffer, sizes, 2);

  g_assert (thrift_framed_transport_read (framed, buf, 4, NULL) == 4);
  g_assert (thrift_framed_transport_read (framed, buf, 64, NULL) == 64);

  /* And the bound now describes the second frame, not the first. */
  g_assert (may_read (framed, 64) == TRUE);
  g_assert (may_read (framed, 65) == FALSE);

  g_object_unref (framed);
  g_object_unref (buffer);
}

/* Asking for more than the current frame holds is a short read, which is what
 * this transport has always answered -- one frame is as far as a single read
 * goes.  The bound must not turn that into an error, which is what checking
 * the requested length against a frame-sized budget would do from the second
 * frame onwards. */
static void
test_a_read_larger_than_the_frame_is_short_not_refused (void)
{
  const guint32 sizes[] = { 8, 8 };
  ThriftMemoryBuffer *buffer = NULL;
  ThriftTransport *framed = NULL;
  guchar buf[16];

  framed = framed_over_frames (&buffer, sizes, 2);

  /* First frame: nothing is bound yet, so this would pass either way. */
  g_assert (thrift_framed_transport_read (framed, buf, 16, NULL) == 8);

  /* Second frame, now with an eight-byte bound in place from the first. */
  g_assert (thrift_framed_transport_read (framed, buf, 16, NULL) == 8);

  g_object_unref (framed);
  g_object_unref (buffer);
}

/* The bound is per frame.  Reading many frames must not accumulate into an
 * exhausted budget, which is the failure a bind-without-reset produces. */
static void
test_many_frames_do_not_exhaust_the_budget (void)
{
  guint32 sizes[64];
  ThriftMemoryBuffer *buffer = NULL;
  ThriftTransport *framed = NULL;
  guchar buf[16];
  guint i;

  for (i = 0; i < G_N_ELEMENTS (sizes); i++)
    sizes[i] = 16;

  framed = framed_over_frames (&buffer, sizes, G_N_ELEMENTS (sizes));

  for (i = 0; i < G_N_ELEMENTS (sizes); i++)
    {
      g_assert (thrift_framed_transport_read (framed, buf, 16, NULL) == 16);
      g_assert (may_read (framed, 16) == TRUE);
      g_assert (may_read (framed, 17) == FALSE);
    }

  g_object_unref (framed);
  g_object_unref (buffer);
}

/* A frame the configuration does not allow is still rejected as a frame,
 * before any of the above comes into play. */
static void
test_an_oversized_frame_is_still_refused (void)
{
  const guint32 sizes[] = { 32 };
  ThriftConfiguration *configuration = NULL;
  ThriftMemoryBuffer *buffer = NULL;
  ThriftTransport *framed = NULL;
  ThriftTransport *inner = NULL;
  GError *error = NULL;
  guchar buf[32];

  framed = framed_over_frames (&buffer, sizes, 1);
  inner = THRIFT_FRAMED_TRANSPORT (framed)->transport;

  /* Re-wrap the same buffer with a configuration that will not have it. */
  g_object_unref (framed);
  configuration = g_object_new (THRIFT_TYPE_CONFIGURATION,
                                "max_message_size", 1024,
                                "max_frame_size", 16,
                                NULL);
  framed = g_object_new (THRIFT_TYPE_FRAMED_TRANSPORT,
                         "transport", inner,
                         "configuration", configuration,
                         NULL);

  g_assert (thrift_framed_transport_read (framed, buf, 32, &error) == -1);
  g_assert (error != NULL);
  g_error_free (error);

  g_object_unref (framed);
  g_object_unref (configuration);
  g_object_unref (buffer);
}

int
main (int argc, char *argv[])
{
#if (!GLIB_CHECK_VERSION (2, 36, 0))
  g_type_init ();
#endif

  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/testframedreadbudget/BudgetIsBoundToTheFrame",
                   test_budget_is_bound_to_the_frame);
  g_test_add_func ("/testframedreadbudget/ALargerFrameMayFollowASmallerOne",
                   test_a_larger_frame_may_follow_a_smaller_one);
  g_test_add_func ("/testframedreadbudget/AReadLargerThanTheFrameIsShortNotRefused",
                   test_a_read_larger_than_the_frame_is_short_not_refused);
  g_test_add_func ("/testframedreadbudget/ManyFramesDoNotExhaustTheBudget",
                   test_many_frames_do_not_exhaust_the_budget);
  g_test_add_func ("/testframedreadbudget/AnOversizedFrameIsStillRefused",
                   test_an_oversized_frame_is_still_refused);

  return g_test_run ();
}
