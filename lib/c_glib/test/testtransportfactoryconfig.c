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

/* A transport factory wraps a transport that is already set up, such as the
 * socket ThriftServerSocket hands out for a connection it accepted.  The
 * limits that transport was configured with have to carry over to the wrapper:
 * the wrapper is what the protocol reads from, so its budget is the one sizes
 * are checked against, and its configuration is where the protocol finds the
 * recursion limit.
 *
 * A memory buffer stands in for the accepted socket, which keeps these tests
 * free of sockets, forks and timing.
 */

#include <arpa/inet.h>
#include <glib-object.h>
#include <string.h>
#include <zlib.h>

#include <thrift/c_glib/thrift.h>
#include <thrift/c_glib/thrift_configuration.h>
#include <thrift/c_glib/protocol/thrift_binary_protocol.h>
#include <thrift/c_glib/protocol/thrift_protocol.h>
#include <thrift/c_glib/transport/thrift_buffered_transport_factory.h>
#include <thrift/c_glib/transport/thrift_framed_transport_factory.h>
#include <thrift/c_glib/transport/thrift_memory_buffer.h>
#include <thrift/c_glib/transport/thrift_transport.h>
#include <thrift/c_glib/transport/thrift_transport_factory.h>
#include <thrift/c_glib/transport/thrift_zlib_transport_factory.h>

#define MESSAGE_LIMIT 1024
#define FRAME_LIMIT 1000
#define RECURSION_LIMIT 4

#define BUFFER_BYTES (64 * 1024)

/* Builds a transport the way ThriftServerSocket builds the accepted socket.
 * A negative limit leaves that limit at its default. */
static ThriftConfiguration *
new_configuration (gint message_limit, gint frame_limit, gint recursion_limit)
{
  return g_object_new (THRIFT_TYPE_CONFIGURATION,
                       "max_message_size",
                       message_limit < 0 ? DEFAULT_MAX_MESSAGE_SIZE
                                         : message_limit,
                       "max_frame_size",
                       frame_limit < 0 ? DEFAULT_MAX_FRAME_SIZE : frame_limit,
                       "recursion_limit",
                       recursion_limit < 0 ? DEFAULT_RECURSION_DEPTH
                                           : recursion_limit,
                       NULL);
}

static ThriftMemoryBuffer *
new_accepted_buffer (ThriftConfiguration *configuration)
{
  return g_object_new (THRIFT_TYPE_MEMORY_BUFFER,
                       "buf_size", BUFFER_BYTES,
                       "configuration", configuration,
                       "remainingmessagesize",
                       (glong) configuration->maxMessageSize_,
                       "knowmessagesize",
                       (glong) configuration->maxMessageSize_,
                       NULL);
}

/* Wraps the transport the way ThriftSimpleServer does.  The factory goes away
 * again here: what it created must not depend on it. */
static ThriftTransport *
wrap (GType factory_type, ThriftTransport *transport)
{
  ThriftTransportFactory *factory = g_object_new (factory_type, NULL);
  ThriftTransport *wrapper =
    THRIFT_TRANSPORT_FACTORY_GET_CLASS (factory)
      ->get_transport (factory, transport);

  g_object_unref (factory);
  return wrapper;
}

/* Bytes that do not compress, so that every compressed byte the zlib transport
 * takes in has room in its output buffer. */
static guchar *
random_bytes (guint32 size)
{
  guchar *bytes = g_malloc (size);
  GRand *rand = g_rand_new_with_seed (7);
  guint32 i;

  for (i = 0; i < size; i++)
    bytes[i] = (guchar) g_rand_int_range (rand, 0, 256);

  g_rand_free (rand);
  return bytes;
}

/* Appends size bytes to the buffer as one deflate stream with a sync flush at
 * the end, which is how a peer's zlib transport leaves a message on the wire:
 * everything can be inflated and the stream goes on. */
static void
write_deflated (ThriftMemoryBuffer *buffer, guint32 size)
{
  guchar *plain = random_bytes (size);
  gulong capacity = compressBound (size) + 64;
  guchar *compressed = g_malloc (capacity);
  z_stream stream;

  memset (&stream, 0, sizeof (stream));
  g_assert_cmpint (deflateInit (&stream, Z_DEFAULT_COMPRESSION), ==, Z_OK);
  stream.next_in = plain;
  stream.avail_in = size;
  stream.next_out = compressed;
  stream.avail_out = (uInt) capacity;
  g_assert_cmpint (deflate (&stream, Z_SYNC_FLUSH), ==, Z_OK);
  g_assert_cmpuint (stream.avail_in, ==, 0);
  g_assert (thrift_transport_write (THRIFT_TRANSPORT (buffer), compressed,
                                    (guint32) (capacity - stream.avail_out),
                                    NULL) == TRUE);
  deflateEnd (&stream);
  g_free (compressed);
  g_free (plain);
}

static void
write_frame (ThriftMemoryBuffer *buffer, guint32 size)
{
  guint32 nbo = htonl (size);
  guchar *payload = g_malloc0 (size);

  g_assert (thrift_transport_write (THRIFT_TRANSPORT (buffer), &nbo,
                                    sizeof (nbo), NULL) == TRUE);
  g_assert (thrift_transport_write (THRIFT_TRANSPORT (buffer), payload, size,
                                    NULL) == TRUE);
  g_free (payload);
}

/* Writes a binary-protocol struct whose single field nests structs of the same
 * shape, depth levels deep in all. */
static void
write_nested_structs (ThriftMemoryBuffer *buffer, guint depth)
{
  static const guchar field_header[] = { T_STRUCT, 0, 1 };
  static const guchar field_stop = T_STOP;
  guint i;

  for (i = 0; i + 1 < depth; i++)
    g_assert (thrift_transport_write (THRIFT_TRANSPORT (buffer),
                                      (gpointer) field_header,
                                      sizeof (field_header), NULL) == TRUE);
  for (i = 0; i < depth; i++)
    g_assert (thrift_transport_write (THRIFT_TRANSPORT (buffer),
                                      (gpointer) &field_stop, 1,
                                      NULL) == TRUE);
}

static const GType *
wrapping_factories (void)
{
  static GType types[4];

  types[0] = THRIFT_TYPE_BUFFERED_TRANSPORT_FACTORY;
  types[1] = THRIFT_TYPE_FRAMED_TRANSPORT_FACTORY;
  types[2] = THRIFT_TYPE_ZLIB_TRANSPORT_FACTORY;
  types[3] = G_TYPE_INVALID;
  return types;
}

static void
test_factories_carry_the_configuration_over (void)
{
  const GType *type;

  for (type = wrapping_factories (); *type != G_TYPE_INVALID; type++)
    {
      ThriftConfiguration *configuration =
        new_configuration (MESSAGE_LIMIT, FRAME_LIMIT, RECURSION_LIMIT);
      ThriftMemoryBuffer *buffer = new_accepted_buffer (configuration);
      guint references = G_OBJECT (configuration)->ref_count;
      ThriftTransport *wrapper = wrap (*type, THRIFT_TRANSPORT (buffer));

      g_assert (wrapper->configuration == configuration);
      g_assert_cmpint (wrapper->remainingMessageSize_, ==, MESSAGE_LIMIT);
      g_assert_cmpint (wrapper->knowMessageSize_, ==, MESSAGE_LIMIT);

      /* The wrapper holds a reference of its own, and gives it back. */
      g_assert_cmpuint (G_OBJECT (configuration)->ref_count, ==,
                        references + 1);
      g_object_unref (wrapper);
      g_assert_cmpuint (G_OBJECT (configuration)->ref_count, ==, references);

      g_object_unref (buffer);
      g_object_unref (configuration);
    }
}

static void
test_factories_keep_the_defaults_without_a_configuration (void)
{
  const GType *type;

  for (type = wrapping_factories (); *type != G_TYPE_INVALID; type++)
    {
      ThriftMemoryBuffer *buffer =
        g_object_new (THRIFT_TYPE_MEMORY_BUFFER, "buf_size", BUFFER_BYTES,
                      NULL);
      ThriftTransport *wrapper = wrap (*type, THRIFT_TRANSPORT (buffer));

      g_assert (wrapper->configuration == NULL);
      g_assert_cmpint (wrapper->remainingMessageSize_, ==,
                       DEFAULT_MAX_MESSAGE_SIZE);
      g_assert_cmpint (wrapper->knowMessageSize_, ==,
                       DEFAULT_MAX_MESSAGE_SIZE);

      g_object_unref (wrapper);
      g_object_unref (buffer);
    }
}

static void
test_framed_factory_applies_the_frame_size (void)
{
  ThriftConfiguration *configuration =
    new_configuration (-1, FRAME_LIMIT, -1);
  ThriftMemoryBuffer *buffer = new_accepted_buffer (configuration);
  ThriftTransport *wrapper;
  guchar *out = g_malloc (FRAME_LIMIT + 1);
  GError *error = NULL;

  write_frame (buffer, FRAME_LIMIT);
  write_frame (buffer, FRAME_LIMIT + 1);
  wrapper = wrap (THRIFT_TYPE_FRAMED_TRANSPORT_FACTORY,
                  THRIFT_TRANSPORT (buffer));

  g_assert_cmpint (thrift_transport_read (wrapper, out, FRAME_LIMIT, &error),
                   ==, FRAME_LIMIT);
  g_assert_no_error (error);

  /* One byte over the limit: the frame is refused unread. */
  g_assert_cmpint (thrift_transport_read (wrapper, out, 1, &error), <, 0);
  g_assert_error (error, THRIFT_TRANSPORT_ERROR,
                  THRIFT_TRANSPORT_ERROR_MAX_MESSAGE_SIZE_REACHED);
  g_assert_cmpuint (buffer->buf->len, ==, FRAME_LIMIT + 1);

  g_clear_error (&error);
  g_free (out);
  g_object_unref (wrapper);
  g_object_unref (buffer);
  g_object_unref (configuration);
}

static void
test_zlib_factory_applies_the_message_size (void)
{
  ThriftConfiguration *configuration =
    new_configuration (MESSAGE_LIMIT, -1, -1);
  ThriftMemoryBuffer *buffer = new_accepted_buffer (configuration);
  ThriftTransport *wrapper;
  guchar out[MESSAGE_LIMIT];
  GError *error = NULL;

  write_deflated (buffer, MESSAGE_LIMIT * 2);
  wrapper = wrap (THRIFT_TYPE_ZLIB_TRANSPORT_FACTORY,
                  THRIFT_TRANSPORT (buffer));

  g_assert_cmpint (thrift_transport_read (wrapper, out, 1000, &error), ==,
                   1000);
  g_assert_no_error (error);
  g_assert_cmpint (wrapper->remainingMessageSize_, ==, MESSAGE_LIMIT - 1000);

  /* 24 bytes are left of this message, so 25 are refused. */
  g_assert_cmpint (thrift_transport_read (wrapper, out, 25, &error), <, 0);
  g_assert_error (error, THRIFT_TRANSPORT_ERROR,
                  THRIFT_TRANSPORT_ERROR_MAX_MESSAGE_SIZE_REACHED);

  g_clear_error (&error);
  g_object_unref (wrapper);
  g_object_unref (buffer);
  g_object_unref (configuration);
}

/* On a server, reading and writing go through separate transports, so flush()
 * never reaches the one that reads.  The end of a message has to return the
 * budget there, or the limit applies to the whole connection. */
static void
test_zlib_read_budget_applies_per_message (void)
{
  ThriftConfiguration *configuration =
    new_configuration (MESSAGE_LIMIT, -1, -1);
  ThriftMemoryBuffer *buffer = new_accepted_buffer (configuration);
  ThriftTransport *wrapper;
  guchar out[MESSAGE_LIMIT];
  GError *error = NULL;
  guint message;

  write_deflated (buffer, 800 * 3);
  wrapper = wrap (THRIFT_TYPE_ZLIB_TRANSPORT_FACTORY,
                  THRIFT_TRANSPORT (buffer));

  for (message = 0; message < 3; message++)
    {
      g_assert_cmpint (thrift_transport_read (wrapper, out, 800, &error), ==,
                       800);
      g_assert_no_error (error);
      g_assert_cmpint (wrapper->remainingMessageSize_, ==,
                       MESSAGE_LIMIT - 800);

      g_assert (thrift_transport_read_end (wrapper, &error) == TRUE);
      g_assert_no_error (error);
      g_assert_cmpint (wrapper->remainingMessageSize_, ==, MESSAGE_LIMIT);
    }

  g_object_unref (wrapper);
  g_object_unref (buffer);
  g_object_unref (configuration);
}

static void
test_protocol_finds_the_recursion_limit (void)
{
  const GType *type;

  for (type = wrapping_factories (); *type != G_TYPE_INVALID; type++)
    {
      ThriftConfiguration *configuration =
        new_configuration (-1, -1, RECURSION_LIMIT);
      ThriftMemoryBuffer *buffer = new_accepted_buffer (configuration);
      ThriftTransport *wrapper = wrap (*type, THRIFT_TRANSPORT (buffer));
      ThriftProtocol *protocol =
        g_object_new (THRIFT_TYPE_BINARY_PROTOCOL, "transport", wrapper, NULL);

      /* Where thrift_protocol_skip() and the generated readers look it up. */
      g_assert (protocol->transport->configuration == configuration);
      g_assert_cmpint (protocol->transport->configuration->recursionLimit_, ==,
                       RECURSION_LIMIT);

      g_object_unref (protocol);
      g_object_unref (wrapper);
      g_object_unref (buffer);
      g_object_unref (configuration);
    }
}

static void
test_buffered_factory_applies_the_recursion_limit (void)
{
  ThriftConfiguration *configuration =
    new_configuration (-1, -1, RECURSION_LIMIT);
  ThriftMemoryBuffer *buffer = new_accepted_buffer (configuration);
  ThriftTransport *wrapper = wrap (THRIFT_TYPE_BUFFERED_TRANSPORT_FACTORY,
                                   THRIFT_TRANSPORT (buffer));
  ThriftProtocol *protocol =
    g_object_new (THRIFT_TYPE_BINARY_PROTOCOL, "transport", wrapper, NULL);
  GError *error = NULL;

  write_nested_structs (buffer, RECURSION_LIMIT + 1);
  g_assert_cmpint (thrift_protocol_skip (protocol, T_STRUCT, &error), >, 0);
  g_assert_no_error (error);

  write_nested_structs (buffer, RECURSION_LIMIT + 2);
  g_assert_cmpint (thrift_protocol_skip (protocol, T_STRUCT, &error), <, 0);
  g_assert_error (error, THRIFT_PROTOCOL_ERROR,
                  THRIFT_PROTOCOL_ERROR_DEPTH_LIMIT);

  g_clear_error (&error);
  g_object_unref (protocol);
  g_object_unref (wrapper);
  g_object_unref (buffer);
  g_object_unref (configuration);
}

int
main (int argc, char *argv[])
{
#if (!GLIB_CHECK_VERSION (2, 36, 0))
  g_type_init ();
#endif

  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/testtransportfactoryconfig/CarryTheConfigurationOver",
                   test_factories_carry_the_configuration_over);
  g_test_add_func ("/testtransportfactoryconfig/KeepTheDefaultsWithoutAConfiguration",
                   test_factories_keep_the_defaults_without_a_configuration);
  g_test_add_func ("/testtransportfactoryconfig/FramedFactoryAppliesTheFrameSize",
                   test_framed_factory_applies_the_frame_size);
  g_test_add_func ("/testtransportfactoryconfig/ZlibFactoryAppliesTheMessageSize",
                   test_zlib_factory_applies_the_message_size);
  g_test_add_func ("/testtransportfactoryconfig/ZlibReadBudgetAppliesPerMessage",
                   test_zlib_read_budget_applies_per_message);
  g_test_add_func ("/testtransportfactoryconfig/ProtocolFindsTheRecursionLimit",
                   test_protocol_finds_the_recursion_limit);
  g_test_add_func ("/testtransportfactoryconfig/BufferedFactoryAppliesTheRecursionLimit",
                   test_buffered_factory_applies_the_recursion_limit);

  return g_test_run ();
}
