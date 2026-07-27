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
#include <glib-object.h>

#include <thrift/c_glib/thrift.h>
#include <thrift/c_glib/processor/thrift_multiplexed_processor.h>
#include <thrift/c_glib/protocol/thrift_binary_protocol.h>
#include <thrift/c_glib/protocol/thrift_multiplexed_protocol.h>
#include <thrift/c_glib/transport/thrift_memory_buffer.h>

#define TEST_SERVICE_NAME "aService"

/* A protocol that reports success for a message header without ever writing
   the method name, the shape a protocol takes when it does not handle every
   message encoding. */

#define TEST_TYPE_NAMELESS_PROTOCOL (test_nameless_protocol_get_type ())

struct _TestNamelessProtocol
{
  ThriftProtocol parent;
};
typedef struct _TestNamelessProtocol TestNamelessProtocol;

struct _TestNamelessProtocolClass
{
  ThriftProtocolClass parent;
};
typedef struct _TestNamelessProtocolClass TestNamelessProtocolClass;

G_DEFINE_TYPE (TestNamelessProtocol, test_nameless_protocol,
               THRIFT_TYPE_PROTOCOL)

static gint32
test_nameless_protocol_read_message_begin (ThriftProtocol *protocol,
                                           gchar **name,
                                           ThriftMessageType *message_type,
                                           gint32 *seqid, GError **error)
{
  THRIFT_UNUSED_VAR (protocol);
  THRIFT_UNUSED_VAR (name);
  THRIFT_UNUSED_VAR (error);

  *message_type = T_CALL;
  *seqid = 1;

  return 4;
}

/* The base class points every unimplemented read at its own dispatcher, so the
   one call the test can reach beyond the message header is answered here. */
static gint32
test_nameless_protocol_read_struct_begin (ThriftProtocol *protocol,
                                          gchar **name, GError **error)
{
  THRIFT_UNUSED_VAR (protocol);
  THRIFT_UNUSED_VAR (name);

  g_set_error (error, THRIFT_PROTOCOL_ERROR,
               THRIFT_PROTOCOL_ERROR_INVALID_DATA,
               "stub protocol reads no message body");
  return -1;
}

static void
test_nameless_protocol_init (TestNamelessProtocol *protocol)
{
  THRIFT_UNUSED_VAR (protocol);
}

static void
test_nameless_protocol_class_init (TestNamelessProtocolClass *klass)
{
  THRIFT_PROTOCOL_CLASS (klass)->read_message_begin =
    test_nameless_protocol_read_message_begin;
  THRIFT_PROTOCOL_CLASS (klass)->read_struct_begin =
    test_nameless_protocol_read_struct_begin;
}

/* A processor that records whether it was handed the call */

static gboolean sub_processor_called = FALSE;

#define TEST_TYPE_SUB_PROCESSOR (test_sub_processor_get_type ())

struct _TestSubProcessor
{
  ThriftProcessor parent;
};
typedef struct _TestSubProcessor TestSubProcessor;

struct _TestSubProcessorClass
{
  ThriftProcessorClass parent;
};
typedef struct _TestSubProcessorClass TestSubProcessorClass;

G_DEFINE_TYPE (TestSubProcessor, test_sub_processor, THRIFT_TYPE_PROCESSOR)

static gboolean
test_sub_processor_process (ThriftProcessor *processor, ThriftProtocol *in,
                            ThriftProtocol *out, GError **error)
{
  THRIFT_UNUSED_VAR (processor);
  THRIFT_UNUSED_VAR (in);
  THRIFT_UNUSED_VAR (out);
  THRIFT_UNUSED_VAR (error);

  sub_processor_called = TRUE;

  return TRUE;
}

static void
test_sub_processor_init (TestSubProcessor *processor)
{
  THRIFT_UNUSED_VAR (processor);
}

static void
test_sub_processor_class_init (TestSubProcessorClass *klass)
{
  THRIFT_PROCESSOR_CLASS (klass)->process = test_sub_processor_process;
}

/* Runs one message through a multiplexed processor that has a single service
   registered under TEST_SERVICE_NAME. When message_name is NULL a header the
   protocol cannot decode is written instead of a well-formed one. */
static gboolean
run_message (const gchar *message_name, GError **error)
{
  ThriftMultiplexedProcessor *processor = NULL;
  TestSubProcessor *sub_processor = NULL;
  ThriftMemoryBuffer *in_transport = NULL;
  ThriftMemoryBuffer *out_transport = NULL;
  ThriftBinaryProtocol *in_protocol = NULL;
  ThriftBinaryProtocol *out_protocol = NULL;
  gboolean result;

  sub_processor_called = FALSE;

  processor = g_object_new (THRIFT_TYPE_MULTIPLEXED_PROCESSOR, NULL);
  sub_processor = g_object_new (TEST_TYPE_SUB_PROCESSOR, NULL);
  g_assert (THRIFT_MULTIPLEXED_PROCESSOR_GET_CLASS (processor)
            ->register_processor (THRIFT_PROCESSOR (processor),
                                  TEST_SERVICE_NAME,
                                  THRIFT_PROCESSOR (sub_processor), NULL));

  in_transport = g_object_new (THRIFT_TYPE_MEMORY_BUFFER, "buf_size", 1024,
                               NULL);
  out_transport = g_object_new (THRIFT_TYPE_MEMORY_BUFFER, "buf_size", 1024,
                                NULL);
  in_protocol = g_object_new (THRIFT_TYPE_BINARY_PROTOCOL, "transport",
                              in_transport, NULL);
  out_protocol = g_object_new (THRIFT_TYPE_BINARY_PROTOCOL, "transport",
                               out_transport, NULL);

  if (message_name != NULL)
    {
      g_assert (thrift_protocol_write_message_begin
                (THRIFT_PROTOCOL (in_protocol), message_name, T_CALL, 1,
                 NULL) > 0);
    }
  else
    {
      /* A version word the protocol rejects */
      g_assert (thrift_protocol_write_i32 (THRIFT_PROTOCOL (in_protocol),
                                           (gint32) 0x80020000, NULL) > 0);
    }
  /* an empty struct as the message body */
  g_assert (thrift_protocol_write_byte (THRIFT_PROTOCOL (in_protocol),
                                        T_STOP, NULL) > 0);

  result = thrift_processor_process (THRIFT_PROCESSOR (processor),
                                     THRIFT_PROTOCOL (in_protocol),
                                     THRIFT_PROTOCOL (out_protocol),
                                     error);

  g_object_unref (in_protocol);
  g_object_unref (out_protocol);
  g_object_unref (in_transport);
  g_object_unref (out_transport);
  g_object_unref (sub_processor);
  g_object_unref (processor);

  return result;
}

/* Runs one message through a multiplexed processor whose input protocol
   reports success for the header without naming the method. */
static gboolean
run_nameless_message (GError **error)
{
  ThriftMultiplexedProcessor *processor = NULL;
  TestSubProcessor *sub_processor = NULL;
  TestNamelessProtocol *in_protocol = NULL;
  ThriftMemoryBuffer *out_transport = NULL;
  ThriftBinaryProtocol *out_protocol = NULL;
  gboolean result;

  sub_processor_called = FALSE;

  processor = g_object_new (THRIFT_TYPE_MULTIPLEXED_PROCESSOR, NULL);
  sub_processor = g_object_new (TEST_TYPE_SUB_PROCESSOR, NULL);
  g_assert (THRIFT_MULTIPLEXED_PROCESSOR_GET_CLASS (processor)
            ->register_processor (THRIFT_PROCESSOR (processor),
                                  TEST_SERVICE_NAME,
                                  THRIFT_PROCESSOR (sub_processor), NULL));

  in_protocol = g_object_new (TEST_TYPE_NAMELESS_PROTOCOL, NULL);
  out_transport = g_object_new (THRIFT_TYPE_MEMORY_BUFFER, "buf_size", 1024,
                                NULL);
  out_protocol = g_object_new (THRIFT_TYPE_BINARY_PROTOCOL, "transport",
                               out_transport, NULL);

  result = thrift_processor_process (THRIFT_PROCESSOR (processor),
                                     THRIFT_PROTOCOL (in_protocol),
                                     THRIFT_PROTOCOL (out_protocol),
                                     error);

  g_object_unref (in_protocol);
  g_object_unref (out_protocol);
  g_object_unref (out_transport);
  g_object_unref (sub_processor);
  g_object_unref (processor);

  return result;
}

/* A message the protocol did not name carries no service to route it to, so
   it must be rejected rather than split into tokens. */
static void
test_message_without_name (void)
{
  GError *error = NULL;

  g_assert (run_nameless_message (&error) == FALSE);
  g_assert (sub_processor_called == FALSE);
  g_assert (error != NULL);
  g_assert (error->domain == THRIFT_MULTIPLEXED_PROCESSOR_ERROR);
  g_assert_cmpint (error->code, ==,
                   THRIFT_MULTIPLEXED_PROCESSOR_ERROR_MESSAGE_WRONGLY_MULTIPLEXED);

  g_clear_error (&error);
}

/* A method name carrying both the service and the function is routed to the
   registered service. */
static void
test_multiplexed_message (void)
{
  GError *error = NULL;

  g_assert (run_message (TEST_SERVICE_NAME
                         THRIFT_MULTIPLEXED_PROTOCOL_DEFAULT_SEPARATOR
                         "aMethod", &error));
  g_assert (sub_processor_called);
  g_assert (error == NULL);

  g_clear_error (&error);
}

/* A method name that names the service but no function cannot be routed. The
   processor answers with an application exception, so it has to describe the
   reason itself instead of quoting an error it never set. */
static void
test_message_without_function_name (void)
{
  GError *error = NULL;

  g_assert (run_message (TEST_SERVICE_NAME, &error));
  g_assert (sub_processor_called == FALSE);
  g_assert (error != NULL);
  g_assert (error->domain == THRIFT_MULTIPLEXED_PROCESSOR_ERROR);
  g_assert_cmpint (error->code, ==,
                   THRIFT_MULTIPLEXED_PROCESSOR_ERROR_SERVICE_UNAVAILABLE);

  g_clear_error (&error);
}

/* A header the protocol rejects leaves the message type and sequence id
   unread, so the failure has to be taken from the return value rather than
   from whatever those happen to hold. */
static void
test_unreadable_message_header (void)
{
  GError *error = NULL;

  g_assert (run_message (NULL, &error) == FALSE);
  g_assert (sub_processor_called == FALSE);
  g_assert (error != NULL);
  g_assert (error->domain == THRIFT_PROTOCOL_ERROR);

  g_clear_error (&error);
}

int
main (int argc, char *argv[])
{
#if (!GLIB_CHECK_VERSION (2, 36, 0))
  g_type_init ();
#endif

  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/testmultiplexedprocessor/MessageWithoutName",
                   test_message_without_name);
  g_test_add_func ("/testmultiplexedprocessor/MultiplexedMessage",
                   test_multiplexed_message);
  g_test_add_func ("/testmultiplexedprocessor/MessageWithoutFunctionName",
                   test_message_without_function_name);
  g_test_add_func ("/testmultiplexedprocessor/UnreadableMessageHeader",
                   test_unreadable_message_header);

  return g_test_run ();
}
