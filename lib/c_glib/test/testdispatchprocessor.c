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
#include <thrift/c_glib/processor/thrift_dispatch_processor.h>
#include <thrift/c_glib/protocol/thrift_binary_protocol.h>
#include <thrift/c_glib/transport/thrift_memory_buffer.h>
#include <thrift/c_glib/thrift_application_exception.h>
#include <thrift/c_glib/thrift_struct.h>

#include "gen-c_glib/t_test_srv.h"

/* A protocol whose read_message_begin behaviour can be selected per test, so
   the dispatch processor can be driven through the results a protocol
   implementation may produce for a message header. */

typedef enum
{
  STUB_MESSAGE_WITHOUT_NAME,   /* reports success but never sets *name    */
  STUB_MESSAGE_NOT_A_CALL,     /* sets a name, message type is not a call */
  STUB_FAILURE_AFTER_NAME      /* sets a name, then reports failure       */
} StubProtocolMode;

#define TEST_TYPE_STUB_PROTOCOL (test_stub_protocol_get_type ())

struct _TestStubProtocol
{
  ThriftProtocol parent;
  StubProtocolMode mode;
};
typedef struct _TestStubProtocol TestStubProtocol;

struct _TestStubProtocolClass
{
  ThriftProtocolClass parent;
};
typedef struct _TestStubProtocolClass TestStubProtocolClass;

G_DEFINE_TYPE (TestStubProtocol, test_stub_protocol, THRIFT_TYPE_PROTOCOL)

static gint32
test_stub_protocol_read_message_begin (ThriftProtocol *protocol, gchar **name,
                                       ThriftMessageType *message_type,
                                       gint32 *seqid, GError **error)
{
  TestStubProtocol *self = (TestStubProtocol *) protocol;

  switch (self->mode)
    {
    case STUB_MESSAGE_WITHOUT_NAME:
      /* Success is reported without the name ever being written - the shape a
         protocol takes when it does not handle every message encoding. */
      *message_type = T_CALL;
      *seqid = 1;
      return 4;

    case STUB_MESSAGE_NOT_A_CALL:
      *name = g_strdup ("someMethod");
      *message_type = T_REPLY;
      *seqid = 1;
      return 20;

    case STUB_FAILURE_AFTER_NAME:
    default:
      *name = g_strdup ("someMethod");
      *message_type = T_CALL;
      g_set_error (error, THRIFT_PROTOCOL_ERROR,
                   THRIFT_PROTOCOL_ERROR_INVALID_DATA,
                   "stub failure after the name was read");
      return -1;
    }
}

/* The base class points every unimplemented read at its own dispatcher, so the
   one call the tests can reach beyond the message header is answered here. */
static gint32
test_stub_protocol_read_struct_begin (ThriftProtocol *protocol, gchar **name,
                                      GError **error)
{
  THRIFT_UNUSED_VAR (protocol);
  THRIFT_UNUSED_VAR (name);

  g_set_error (error, THRIFT_PROTOCOL_ERROR,
               THRIFT_PROTOCOL_ERROR_INVALID_DATA,
               "stub protocol reads no message body");
  return -1;
}

static void
test_stub_protocol_init (TestStubProtocol *protocol)
{
  protocol->mode = STUB_MESSAGE_WITHOUT_NAME;
}

static void
test_stub_protocol_class_init (TestStubProtocolClass *klass)
{
  THRIFT_PROTOCOL_CLASS (klass)->read_message_begin =
    test_stub_protocol_read_message_begin;
  THRIFT_PROTOCOL_CLASS (klass)->read_struct_begin =
    test_stub_protocol_read_struct_begin;
}

/* A dispatch processor that records whether the call was dispatched, instead
   of answering it. */

static gboolean dispatch_call_reached = FALSE;

#define TEST_TYPE_RECORDING_PROCESSOR (test_recording_processor_get_type ())

struct _TestRecordingProcessor
{
  ThriftDispatchProcessor parent;
};
typedef struct _TestRecordingProcessor TestRecordingProcessor;

struct _TestRecordingProcessorClass
{
  ThriftDispatchProcessorClass parent;
};
typedef struct _TestRecordingProcessorClass TestRecordingProcessorClass;

G_DEFINE_TYPE (TestRecordingProcessor, test_recording_processor,
               THRIFT_TYPE_DISPATCH_PROCESSOR)

static gboolean
test_recording_processor_dispatch_call (ThriftDispatchProcessor *self,
                                        ThriftProtocol *in,
                                        ThriftProtocol *out,
                                        gchar *fname,
                                        gint32 seqid,
                                        GError **error)
{
  THRIFT_UNUSED_VAR (self);
  THRIFT_UNUSED_VAR (in);
  THRIFT_UNUSED_VAR (out);
  THRIFT_UNUSED_VAR (seqid);
  THRIFT_UNUSED_VAR (error);

  g_free (fname);

  dispatch_call_reached = TRUE;

  return FALSE;
}

static void
test_recording_processor_init (TestRecordingProcessor *processor)
{
  THRIFT_UNUSED_VAR (processor);
}

static void
test_recording_processor_class_init (TestRecordingProcessorClass *klass)
{
  THRIFT_DISPATCH_PROCESSOR_CLASS (klass)->dispatch_call =
    test_recording_processor_dispatch_call;
}

/* A dispatch processor that keeps the inherited dispatch_call, which answers
   unknown method names with an application exception. */

#define TEST_TYPE_DEFAULT_PROCESSOR (test_default_processor_get_type ())

struct _TestDefaultProcessor
{
  ThriftDispatchProcessor parent;
};
typedef struct _TestDefaultProcessor TestDefaultProcessor;

struct _TestDefaultProcessorClass
{
  ThriftDispatchProcessorClass parent;
};
typedef struct _TestDefaultProcessorClass TestDefaultProcessorClass;

G_DEFINE_TYPE (TestDefaultProcessor, test_default_processor,
               THRIFT_TYPE_DISPATCH_PROCESSOR)

static void
test_default_processor_init (TestDefaultProcessor *processor)
{
  THRIFT_UNUSED_VAR (processor);
}

static void
test_default_processor_class_init (TestDefaultProcessorClass *klass)
{
  THRIFT_UNUSED_VAR (klass);
}

static gboolean
run_stub_message (StubProtocolMode mode, GError **error)
{
  TestRecordingProcessor *processor = NULL;
  TestStubProtocol *protocol = NULL;
  ThriftMemoryBuffer *transport = NULL;
  gboolean result;

  dispatch_call_reached = FALSE;

  processor = g_object_new (TEST_TYPE_RECORDING_PROCESSOR, NULL);
  transport = g_object_new (THRIFT_TYPE_MEMORY_BUFFER, "buf_size", 1024, NULL);
  protocol = g_object_new (TEST_TYPE_STUB_PROTOCOL, "transport", transport,
                           NULL);
  protocol->mode = mode;

  result = thrift_dispatch_processor_process (THRIFT_PROCESSOR (processor),
                                              THRIFT_PROTOCOL (protocol),
                                              THRIFT_PROTOCOL (protocol),
                                              error);

  g_object_unref (protocol);
  g_object_unref (transport);
  g_object_unref (processor);

  return result;
}

/* A protocol that returns success without writing the method name leaves the
   caller with nothing to dispatch on. The call must not be dispatched with the
   pointer the protocol never wrote. */
static void
test_process_message_without_name (void)
{
  GError *error = NULL;

  g_test_expect_message (NULL, G_LOG_LEVEL_WARNING, "*method name*");
  g_assert (run_stub_message (STUB_MESSAGE_WITHOUT_NAME, &error) == FALSE);
  g_test_assert_expected_messages ();

  g_assert (dispatch_call_reached == FALSE);

  g_clear_error (&error);
}

/* A message that is not a call is rejected, and the method name the protocol
   allocated for it is released rather than left behind. */
static void
test_process_message_not_a_call (void)
{
  GError *error = NULL;

  g_test_expect_message (NULL, G_LOG_LEVEL_WARNING, "*invalid message type*");
  g_assert (run_stub_message (STUB_MESSAGE_NOT_A_CALL, &error) == FALSE);
  g_test_assert_expected_messages ();

  g_assert (dispatch_call_reached == FALSE);

  g_clear_error (&error);
}

/* A protocol that fails part way through the header may already have allocated
   the method name. The processor owns it from that point on. */
static void
test_process_failed_message_begin (void)
{
  GError *error = NULL;

  g_test_expect_message (NULL, G_LOG_LEVEL_WARNING, "*start of message*");
  g_assert (run_stub_message (STUB_FAILURE_AFTER_NAME, &error) == FALSE);
  g_test_assert_expected_messages ();

  g_assert (dispatch_call_reached == FALSE);
  g_assert (error != NULL);

  g_clear_error (&error);
}

/* dispatch_call is a public entry point of the class, so it must cope with a
   caller that has no method name to give it, and still send back a complete
   exception rather than one that was cut short where the name would have
   gone. */
static void
test_dispatch_call_without_name (void)
{
  TestDefaultProcessor *processor = NULL;
  ThriftMemoryBuffer *in_transport = NULL;
  ThriftMemoryBuffer *out_transport = NULL;
  ThriftBinaryProtocol *in_protocol = NULL;
  ThriftBinaryProtocol *out_protocol = NULL;
  ThriftApplicationException *xception = NULL;
  GError *error = NULL;
  gchar *reply_name = NULL;
  gchar *reply_message = NULL;
  ThriftMessageType reply_type = 0;
  gint32 reply_seqid = 0;
  guint8 empty_struct[] = { 0x00 };   /* T_STOP */

  processor = g_object_new (TEST_TYPE_DEFAULT_PROCESSOR, NULL);

  in_transport = g_object_new (THRIFT_TYPE_MEMORY_BUFFER, "buf_size", 1024,
                               NULL);
  out_transport = g_object_new (THRIFT_TYPE_MEMORY_BUFFER, "buf_size", 1024,
                                NULL);
  in_protocol = g_object_new (THRIFT_TYPE_BINARY_PROTOCOL, "transport",
                              in_transport, NULL);
  out_protocol = g_object_new (THRIFT_TYPE_BINARY_PROTOCOL, "transport",
                               out_transport, NULL);

  g_assert (thrift_transport_write (THRIFT_TRANSPORT (in_transport),
                                    empty_struct, sizeof (empty_struct), NULL));

  g_assert (THRIFT_DISPATCH_PROCESSOR_GET_CLASS (processor)
            ->dispatch_call (THRIFT_DISPATCH_PROCESSOR (processor),
                             THRIFT_PROTOCOL (in_protocol),
                             THRIFT_PROTOCOL (out_protocol),
                             NULL,
                             1,
                             &error));
  g_assert (error == NULL);

  /* Read the exception back off the wire */
  g_assert (thrift_protocol_read_message_begin (THRIFT_PROTOCOL (out_protocol),
                                                &reply_name, &reply_type,
                                                &reply_seqid, &error) > 0);
  g_assert_cmpint (reply_type, ==, T_EXCEPTION);
  g_assert_cmpint (reply_seqid, ==, 1);

  xception = g_object_new (THRIFT_TYPE_APPLICATION_EXCEPTION, NULL);
  g_assert (thrift_struct_read (THRIFT_STRUCT (xception),
                                THRIFT_PROTOCOL (out_protocol), &error) > 0);
  g_object_get (xception, "message", &reply_message, NULL);
  g_assert_cmpstr (reply_message, ==, "Invalid method name: ''");

  g_free (reply_message);
  g_free (reply_name);
  g_object_unref (xception);
  g_object_unref (in_protocol);
  g_object_unref (out_protocol);
  g_object_unref (in_transport);
  g_object_unref (out_transport);
  g_object_unref (processor);
}

/* A generated processor reaches its handler through the dispatch_call slot the
   base class defines, so a service that has no parent must claim that slot for
   itself. Srv is such a service: it extends nothing, so if its class_init only
   sets its own dispatch_call member and leaves the inherited slot alone, every
   call is answered by the base class' default with "Invalid method name". */

static gboolean srv_handler_reached = FALSE;

#define TEST_TYPE_SRV_HANDLER (test_srv_handler_get_type ())

struct _TestSrvHandler
{
  TTestSrvHandler parent;
};
typedef struct _TestSrvHandler TestSrvHandler;

struct _TestSrvHandlerClass
{
  TTestSrvHandlerClass parent;
};
typedef struct _TestSrvHandlerClass TestSrvHandlerClass;

G_DEFINE_TYPE (TestSrvHandler, test_srv_handler, T_TEST_TYPE_SRV_HANDLER)

static gboolean
test_srv_handler_primitive_method (TTestSrvIf *iface, gint32 *_return,
                                   GError **error)
{
  THRIFT_UNUSED_VAR (iface);
  THRIFT_UNUSED_VAR (error);

  srv_handler_reached = TRUE;
  *_return = 42;

  return TRUE;
}

static void
test_srv_handler_init (TestSrvHandler *handler)
{
  THRIFT_UNUSED_VAR (handler);
}

static void
test_srv_handler_class_init (TestSrvHandlerClass *klass)
{
  T_TEST_SRV_HANDLER_CLASS (klass)->primitive_method =
    test_srv_handler_primitive_method;
}

static void
test_generated_base_service_dispatches (void)
{
  TestSrvHandler *handler = NULL;
  TTestSrvProcessor *processor = NULL;
  ThriftMemoryBuffer *transport = NULL;
  ThriftBinaryProtocol *protocol = NULL;
  GError *error = NULL;
  gchar *reply_name = NULL;
  ThriftMessageType reply_type = 0;
  gint32 reply_seqid = 0;
  gchar *field_name = NULL;
  ThriftType field_type = 0;
  gint16 field_id = 0;
  gint32 result = 0;

  srv_handler_reached = FALSE;

  handler = g_object_new (TEST_TYPE_SRV_HANDLER, NULL);
  processor = g_object_new (T_TEST_TYPE_SRV_PROCESSOR, "handler", handler,
                            NULL);
  transport = g_object_new (THRIFT_TYPE_MEMORY_BUFFER, "buf_size", 1024, NULL);
  protocol = g_object_new (THRIFT_TYPE_BINARY_PROTOCOL, "transport", transport,
                           NULL);

  /* Write a primitiveMethod call. It takes no arguments, so the argument
     struct is empty. */
  g_assert (thrift_protocol_write_message_begin (THRIFT_PROTOCOL (protocol),
                                                 "primitiveMethod", T_CALL, 1,
                                                 &error) > 0);
  g_assert (thrift_protocol_write_struct_begin (THRIFT_PROTOCOL (protocol),
                                                "primitiveMethod_args",
                                                &error) >= 0);
  g_assert (thrift_protocol_write_field_stop (THRIFT_PROTOCOL (protocol),
                                              &error) > 0);
  g_assert (thrift_protocol_write_struct_end (THRIFT_PROTOCOL (protocol),
                                              &error) >= 0);
  g_assert (thrift_protocol_write_message_end (THRIFT_PROTOCOL (protocol),
                                               &error) >= 0);
  g_assert (error == NULL);

  g_assert (thrift_processor_process (THRIFT_PROCESSOR (processor),
                                      THRIFT_PROTOCOL (protocol),
                                      THRIFT_PROTOCOL (protocol), &error));
  g_assert (error == NULL);

  g_assert (srv_handler_reached == TRUE);

  /* The reply carries the handler's return value, not an application
     exception reporting the method name was not recognized. */
  g_assert (thrift_protocol_read_message_begin (THRIFT_PROTOCOL (protocol),
                                                &reply_name, &reply_type,
                                                &reply_seqid, &error) > 0);
  g_assert_cmpint (reply_type, ==, T_REPLY);
  g_assert_cmpint (reply_seqid, ==, 1);

  g_assert (thrift_protocol_read_struct_begin (THRIFT_PROTOCOL (protocol),
                                               &field_name, &error) >= 0);
  g_free (field_name);
  field_name = NULL;

  g_assert (thrift_protocol_read_field_begin (THRIFT_PROTOCOL (protocol),
                                              &field_name, &field_type,
                                              &field_id, &error) > 0);
  g_assert_cmpint (field_type, ==, T_I32);
  g_assert_cmpint (field_id, ==, 0);
  g_assert (thrift_protocol_read_i32 (THRIFT_PROTOCOL (protocol), &result,
                                      &error) > 0);
  g_assert_cmpint (result, ==, 42);
  g_assert (error == NULL);

  g_free (field_name);
  g_free (reply_name);
  g_object_unref (protocol);
  g_object_unref (transport);
  g_object_unref (processor);
  g_object_unref (handler);
}

int
main (int argc, char *argv[])
{
#if (!GLIB_CHECK_VERSION (2, 36, 0))
  g_type_init ();
#endif

  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/testdispatchprocessor/MessageWithoutName",
                   test_process_message_without_name);
  g_test_add_func ("/testdispatchprocessor/MessageNotACall",
                   test_process_message_not_a_call);
  g_test_add_func ("/testdispatchprocessor/FailedMessageBegin",
                   test_process_failed_message_begin);
  g_test_add_func ("/testdispatchprocessor/DispatchCallWithoutName",
                   test_dispatch_call_without_name);
  g_test_add_func ("/testdispatchprocessor/GeneratedBaseServiceDispatches",
                   test_generated_base_service_dispatches);

  return g_test_run ();
}
