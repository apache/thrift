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
#include <string.h>

#include <thrift/c_glib/thrift_application_exception.h>
#include <thrift/c_glib/thrift_struct.h>
#include <thrift/c_glib/protocol/thrift_binary_protocol.h>
#include <thrift/c_glib/protocol/thrift_compact_protocol.h>
#include <thrift/c_glib/transport/thrift_memory_buffer.h>

#if defined (__GLIBC__) && (__GLIBC__ > 2 || (__GLIBC__ == 2 && __GLIBC_MINOR__ >= 33))
#include <malloc.h>
#define HAVE_MALLINFO2 1
#endif

static void
test_create_and_destroy (void)
{
  GObject *object = NULL;

  /* A ThriftApplicationException can be created... */
  object = g_object_new (THRIFT_TYPE_APPLICATION_EXCEPTION, NULL);

  g_assert (object != NULL);
  g_assert (THRIFT_IS_APPLICATION_EXCEPTION (object));

  /* ...and destroyed */
  g_object_unref (object);
}

static void
test_initialize (void)
{
  ThriftApplicationException *xception = NULL;
  gint32 type = THRIFT_APPLICATION_EXCEPTION_ERROR_INTERNAL_ERROR;
  gchar *message = "Exception message";
  gint32 retrieved_type = 0;
  gchar *retrieved_message = NULL;

  /* A ThriftApplicationException has "type" and "message" properties that can
     be initialized at object creation */
  xception =
    g_object_new (THRIFT_TYPE_APPLICATION_EXCEPTION,
                  "type",    type,
                  "message", message,
                  NULL);

  g_assert (xception != NULL);

  /* A ThriftApplicationException's properties can be retrieved */
  g_object_get (xception,
                "type",    &retrieved_type,
                "message", &retrieved_message,
                NULL);

  g_assert (retrieved_type == type);
  g_assert (retrieved_message != NULL);
  g_assert_cmpstr (retrieved_message, ==, message);

  g_free (retrieved_message);
  g_object_unref (xception);
}

static void
test_properties_test (void)
{
  ThriftApplicationException *xception = NULL;
  gint32 retrieved_type;

  xception = g_object_new (THRIFT_TYPE_APPLICATION_EXCEPTION, NULL);

#define TEST_TYPE_VALUE(_type)                                  \
  retrieved_type = -1;                                          \
  g_object_set (xception, "type", _type, NULL);                 \
  g_object_get (xception, "type", &retrieved_type, NULL);       \
  g_assert_cmpint (retrieved_type, ==, _type);

  /* The "type" property can be set to any valid Thrift exception type */
  TEST_TYPE_VALUE (THRIFT_APPLICATION_EXCEPTION_ERROR_UNKNOWN);
  TEST_TYPE_VALUE (THRIFT_APPLICATION_EXCEPTION_ERROR_UNKNOWN_METHOD);
  TEST_TYPE_VALUE (THRIFT_APPLICATION_EXCEPTION_ERROR_INVALID_MESSAGE_TYPE);
  TEST_TYPE_VALUE (THRIFT_APPLICATION_EXCEPTION_ERROR_WRONG_METHOD_NAME);
  TEST_TYPE_VALUE (THRIFT_APPLICATION_EXCEPTION_ERROR_BAD_SEQUENCE_ID);
  TEST_TYPE_VALUE (THRIFT_APPLICATION_EXCEPTION_ERROR_MISSING_RESULT);
  TEST_TYPE_VALUE (THRIFT_APPLICATION_EXCEPTION_ERROR_INTERNAL_ERROR);
  TEST_TYPE_VALUE (THRIFT_APPLICATION_EXCEPTION_ERROR_PROTOCOL_ERROR);
  TEST_TYPE_VALUE (THRIFT_APPLICATION_EXCEPTION_ERROR_INVALID_TRANSFORM);
  TEST_TYPE_VALUE (THRIFT_APPLICATION_EXCEPTION_ERROR_INVALID_PROTOCOL);
  TEST_TYPE_VALUE (THRIFT_APPLICATION_EXCEPTION_ERROR_UNSUPPORTED_CLIENT_TYPE);

/* "g_test_expect_message" is required for the property range tests below but is
   not present in GLib before version 2.34 */
#if (GLIB_CHECK_VERSION (2, 34, 0))
  g_object_set (xception,
                "type", THRIFT_APPLICATION_EXCEPTION_ERROR_UNKNOWN,
                NULL);

  /* The "type" property cannot be set to a value too low (less than zero) */
  g_test_expect_message ("GLib-GObject",
                         G_LOG_LEVEL_WARNING,
                         "value*out of range*type*");
  g_object_set (xception, "type", -1, NULL);
  g_test_assert_expected_messages ();

  g_object_get (xception, "type", &retrieved_type, NULL);
  g_assert_cmpint (retrieved_type, !=, -1);
  g_assert_cmpint (retrieved_type,
                   ==,
                   THRIFT_APPLICATION_EXCEPTION_ERROR_UNKNOWN);

  /* The "type" property cannot be set to a value too high (greater than the
     highest defined exception-type value) */
  g_test_expect_message ("GLib-GObject",
                         G_LOG_LEVEL_WARNING,
                         "value*out of range*type*");
  g_object_set (xception, "type", THRIFT_APPLICATION_EXCEPTION_ERROR_N, NULL);
  g_test_assert_expected_messages ();

  g_object_get (xception, "type", &retrieved_type, NULL);
  g_assert_cmpint (retrieved_type, !=, THRIFT_APPLICATION_EXCEPTION_ERROR_N);
  g_assert_cmpint (retrieved_type,
                   ==,
                   THRIFT_APPLICATION_EXCEPTION_ERROR_UNKNOWN);
#endif

  g_object_unref (xception);
}

static void
test_properties_message (void)
{
  ThriftApplicationException *xception = NULL;
  gchar *message = "Exception message";
  gchar *retrieved_message;

  xception = g_object_new (THRIFT_TYPE_APPLICATION_EXCEPTION, NULL);

  /* The "message" property can be set to NULL */
  g_object_set (xception, "message", NULL, NULL);
  g_object_get (xception, "message", &retrieved_message, NULL);
  g_assert (retrieved_message == NULL);

  /* The "message" property can be set to a valid string */
  g_object_set (xception, "message", message, NULL);
  g_object_get (xception, "message", &retrieved_message, NULL);
  g_assert_cmpint (strcmp (retrieved_message, message), ==, 0);

  g_free (retrieved_message);
  g_object_unref (xception);
}

#ifdef HAVE_MALLINFO2
static size_t
heap_in_use (void)
{
  struct mallinfo2 info = mallinfo2 ();
  return info.uordblks + info.hblkhd;
}
#endif

/* Writes an application exception whose message field comes count times,
   and reads it back. */
static void
write_and_read_repeated_message (gboolean compact, gint count)
{
  GError *error = NULL;
  ThriftTransport *transport;
  ThriftProtocol *protocol;
  ThriftApplicationException *xception;
  gint i;

  transport = THRIFT_TRANSPORT (g_object_new (THRIFT_TYPE_MEMORY_BUFFER,
                                              "buf_size", 1024 * 1024,
                                              NULL));
  protocol = THRIFT_PROTOCOL (g_object_new (compact
                                            ? THRIFT_TYPE_COMPACT_PROTOCOL
                                            : THRIFT_TYPE_BINARY_PROTOCOL,
                                            "transport", transport,
                                            NULL));

  g_assert (thrift_protocol_write_struct_begin (protocol, "TApplicationException",
                                                &error) >= 0);
  for (i = 0; i < count; i++)
  {
    g_assert (thrift_protocol_write_field_begin (protocol, "message", T_STRING, 1,
                                                 &error) >= 0);
    g_assert (thrift_protocol_write_string (protocol, i + 1 < count ? "" : "last",
                                            &error) >= 0);
    g_assert (thrift_protocol_write_field_end (protocol, &error) >= 0);
  }
  g_assert (thrift_protocol_write_field_stop (protocol, &error) >= 0);
  g_assert (thrift_protocol_write_struct_end (protocol, &error) >= 0);
  g_assert_no_error (error);

  xception = g_object_new (THRIFT_TYPE_APPLICATION_EXCEPTION, NULL);
  g_assert (thrift_struct_read (THRIFT_STRUCT (xception), protocol, &error) > 0);
  g_assert_no_error (error);
  g_assert_cmpstr (xception->message, ==, "last");

  g_object_unref (xception);
  g_object_unref (protocol);
  g_object_unref (transport);
}

/* A message field that comes more than once replaces the message read
   before, which must not stay allocated. */
static void
test_read_repeated_message (gconstpointer compact)
{
#ifdef HAVE_MALLINFO2
  const gint count = 20000;
  size_t before;
  size_t after;

  /* the first round sets up what stays allocated anyway */
  write_and_read_repeated_message (GPOINTER_TO_INT (compact), 2);

  before = heap_in_use ();
  write_and_read_repeated_message (GPOINTER_TO_INT (compact), count);
  after = heap_in_use ();

  /* each message left behind would take at least 16 bytes */
  g_assert_cmpuint (after > before ? after - before : 0, <, (size_t) count * 4);
#else
  write_and_read_repeated_message (GPOINTER_TO_INT (compact), 2);
  g_test_skip ("needs mallinfo2() to measure the heap");
#endif
}

int
main (int argc, char **argv)
{
#if (!GLIB_CHECK_VERSION (2, 36, 0))
  g_type_init ();
#endif

  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/testapplicationexception/CreateAndDestroy",
    test_create_and_destroy);
  g_test_add_func ("/testapplicationexception/Initialize",
    test_initialize);
  g_test_add_func ("/testapplicationexception/Properties/test",
    test_properties_test);
  g_test_add_func ("/testapplicationexception/Properties/message",
    test_properties_message);
  g_test_add_data_func ("/testapplicationexception/Read/RepeatedMessage/Binary",
    GINT_TO_POINTER (FALSE), test_read_repeated_message);
  g_test_add_data_func ("/testapplicationexception/Read/RepeatedMessage/Compact",
    GINT_TO_POINTER (TRUE), test_read_repeated_message);

  return g_test_run ();
}
