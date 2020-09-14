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

/* Disable string-function optimizations when glibc is used, as these produce
   compiler warnings about string length when a string function is used inside
   a call to g_assert () */
#ifdef __GLIBC__
#include <features.h>
#define __NO_STRING_INLINES 1
#endif

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <netdb.h>
#include <string.h>
#include <sys/wait.h>

#include <thrift/c_glib/protocol/thrift_protocol.h>
#include <thrift/c_glib/transport/thrift_memory_buffer.h>
#include <thrift/c_glib/transport/thrift_framed_transport.h>

#define TEST_BOOL TRUE
#define TEST_BYTE 123
#define TEST_I16 12345
#define TEST_I32 1234567890
#define TEST_I64 G_GINT64_CONSTANT (123456789012345)
#define TEST_DOUBLE 1234567890.123
#define TEST_STRING "this is a test string 1234567890!@#$%^&*()"
#define TEST_PORT 51199

static int transport_read_count = 0;
static int transport_read_error = 0;
static int transport_read_error_at = -1;
gint32
my_thrift_transport_read_all (ThriftTransport *transport, gpointer buf,
                              guint32 len, GError **error)
{
  if (transport_read_count != transport_read_error_at
      && transport_read_error == 0)
  {
    transport_read_count++;
    return thrift_transport_read_all (transport, buf, len, error);
  }
  return -1;
}

static int transport_write_count = 0;
static int transport_write_error = 0;
static int transport_write_error_at = -1;
gboolean
my_thrift_transport_write (ThriftTransport *transport, const gpointer buf,
                           const guint32 len, GError **error)
{
  if (transport_write_count != transport_write_error_at
      && transport_write_error == 0)
  {
    transport_write_count++;
    return thrift_transport_write (transport, buf, len, error);
  }
  return FALSE;
}

#define thrift_transport_read_all my_thrift_transport_read_all
#define thrift_transport_write my_thrift_transport_write
#include "../src/thrift/c_glib/protocol/thrift_json_protocol.c"
#undef thrift_transport_read_all
#undef thrift_transport_write

static void
test_create_and_destroy(void)
{
  GObject *object = NULL;

  /* create an object and then destroy it*/
  object = g_object_new (THRIFT_TYPE_JSON_PROTOCOL, NULL);
  g_assert (object != NULL);
  g_object_unref (object);
}

static void
test_initialize(void)
{
  ThriftLookAheadReader *reader = NULL;
  ThriftMemoryBuffer *tbuffer = NULL;
  ThriftJsonProtocol *protocol = NULL;
  ThriftMemoryBuffer *temp = NULL;
  ThriftLookAheadReader *temp1 = NULL;
  GByteArray *buf;

  /* create a ThriftTransport */
  buf = g_byte_array_new ();
  tbuffer = g_object_new (THRIFT_TYPE_MEMORY_BUFFER,
                          "buf", buf, NULL);
  g_assert (tbuffer != NULL);

  /* create a ThriftLookAheadReader */
  reader = g_object_new (THRIFT_TYPE_LOOK_AHEAD_READER, NULL);
  g_assert (reader != NULL);

  /* create a ThriftJsonProtocol using the Transport and the look_ahead_reader */
  protocol = g_object_new (THRIFT_TYPE_JSON_PROTOCOL, 
                           "transport", tbuffer, 
                           "look ahead reader", reader,
                           NULL);
  g_assert (protocol != NULL);

  /* fetch the properties */
  g_object_get (G_OBJECT(protocol), "transport", &temp, "look ahead reader", &temp1, NULL);
  g_object_unref (temp);
  g_object_unref (temp1);

  /* clean up memory */
  g_object_unref (protocol);
  g_object_unref (reader);
  g_object_unref (tbuffer);
}

static void
test_read_and_write (void)
{
  ThriftLookAheadReader *reader = NULL;
  ThriftMemoryBuffer *tbuffer = NULL;
  ThriftJsonProtocol *protocol = NULL;
  GByteArray *buf = NULL;
  gchar read[100];
  gchar *b;
  gint16 field_id;
  guint16 c;
  gint32 ret;
  gchar *str = NULL;
  gint64 num;
  gdouble dnum;
  ThriftMessageType message_type, value_type;
  gint32 seqid;
  GError *error = NULL;

  /* create a ThriftTransport */
  buf = g_byte_array_new ();
  tbuffer = g_object_new (THRIFT_TYPE_MEMORY_BUFFER,
                          "buf", buf, NULL);
  g_assert (tbuffer != NULL);

  /* create a ThriftLookAheadReader */
  reader = g_object_new (THRIFT_TYPE_LOOK_AHEAD_READER, "transport",
                         tbuffer, NULL);
  g_assert (reader != NULL);

  /* create a ThriftJsonProtocol using the Transport and the look_ahead_reader */
  protocol = g_object_new (THRIFT_TYPE_JSON_PROTOCOL, 
                           "transport", tbuffer, 
                           "look ahead reader", reader,
                           NULL);
  g_assert (protocol != NULL);

  /* test write json escaped char */
  memset (read, 0, 100);
  b = read;
  thrift_json_protocol_escape_char (protocol, '\n', &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, 6, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("\\u000a", b) == 0);

  memset (read, 0, 100);
  thrift_json_protocol_escape_char (protocol, ' ', &error);
  g_assert (error == NULL);

  g_assert (THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, 6, &error) > 0);
  g_assert (error == NULL);
  g_assert (strcmp ("\\u0020", b) == 0);

  /* test write json char */
  memset (read, 0, 100);
  thrift_json_protocol_write_json_char (protocol, (guint8*)"\n", &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, 2, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("\\n", b) == 0);

  memset (read, 0, 100);
  thrift_json_protocol_write_json_char (protocol, (guint8*)" ", &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, 2, &error);
  g_assert (error == NULL);
  g_assert (strcmp (" ", b) == 0);

  memset (read, 0, 100);
  thrift_json_protocol_write_json_char (protocol, (guint8*)"\\", &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, 2, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("\\\\", b) == 0);

  memset (read, 0, 100);
  thrift_json_protocol_write_json_char (protocol, (guint8*)"@", &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, 2, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("@", b) == 0);

  /* test write json string */
  memset (read, 0, 100);
  thrift_json_protocol_write_json_string (protocol, "This is a \\ json\nstring", &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, 30, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("\"This is a \\\\ json\\nstring\"", b) == 0);

  /* test write json base64 */
  memset (read, 0, 100);
  ret = thrift_json_protocol_write_json_base64 (protocol, "this is a base64 string", 23, &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("\"dGhpcyBpcyBhIGJhc2U2NCBzdHJpbmc\"", b) == 0);

  /* test write json integer */
  memset (read, 0, 100);
  ret = thrift_json_protocol_write_json_integer (protocol, 48, &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp("48", b) == 0);

  memset (read, 0, 100);
  ret = thrift_json_protocol_write_json_integer (protocol, 33000, &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp("33000", b) == 0);

  memset (read, 0, 100);
  ret = thrift_json_protocol_write_json_integer (protocol, 3000000000, &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp("3000000000", b) == 0);

  memset (read, 0, 100);
  ret = thrift_json_protocol_write_json_integer (protocol, 6000000000, &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp("6000000000", b) == 0);

  /* test write json double */
  memset (read, 0, 100);
  ret = thrift_json_protocol_write_json_double (protocol, 12.123, &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp("12.123", b) == 0);

  memset (read, 0, 100);
  ret = thrift_json_protocol_write_json_double (protocol, (-3.14), &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp("-3.14", b) == 0);

  memset (read, 0, 100);
  ret = thrift_json_protocol_write_json_double (protocol, ((+1.0/0.0)/(+1.0/0.0)), &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("\"NaN\"", b) == 0);

  memset (read, 0, 100);
  ret = thrift_json_protocol_write_json_double (protocol, (+1.0/0.0), &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("\"Infinity\"", b) == 0);

  memset (read, 0, 100);
  ret = thrift_json_protocol_write_json_double (protocol, (-1.0/0.0), &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("\"-Infinity\"", b) == 0);

  /* test write json object start */
  memset (read, 0 ,100);
  ret = thrift_json_protocol_write_json_object_start (protocol, &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("{", b) == 0);

  /* test write json object end */
  memset (read, 0 ,100);
  ret = thrift_json_protocol_write_json_object_end (protocol, &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("}", b) == 0);

  /* test write json array start */
  memset (read, 0 ,100);
  ret = thrift_json_protocol_write_json_array_start (protocol, &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("[", b) == 0);

  /* test write json array end */
  memset (read, 0 ,100);
  ret = thrift_json_protocol_write_json_array_end (protocol, &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("]", b) == 0);

  /* test write message begin */
  memset (read, 0 ,100);
  ret = thrift_json_protocol_write_message_begin (THRIFT_PROTOCOL (protocol), "name", 12, 13, &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("[1,\"name\",12,13", b) == 0);

  /* test write message end */
  memset (read, 0 ,100);
  ret = thrift_json_protocol_write_message_end (THRIFT_PROTOCOL (protocol), &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("]", b) == 0);
 
  /* test write struct begin */
  memset (read, 0 ,100);
  ret = thrift_json_protocol_write_struct_begin (THRIFT_PROTOCOL (protocol), "name",  &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("{", b) == 0);

  /* test write struct end */
  memset (read, 0 ,100);
  ret = thrift_json_protocol_write_struct_end (THRIFT_PROTOCOL (protocol), &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("}", b) == 0);

  /* test write field begin */
  memset (read, 0 ,100);
  ret = thrift_json_protocol_write_field_begin (THRIFT_PROTOCOL (protocol), "name", T_STRUCT, 12, &error);
  g_assert (error == NULL);
 
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("12{\"rec\"", b) == 0);
 
  /* test write field end */
  memset (read, 0 ,100);
  ret = thrift_json_protocol_write_field_end (THRIFT_PROTOCOL (protocol), &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("}", b) == 0);

  /* test write field stop */
  memset (read, 0 ,100);
  ret = thrift_json_protocol_write_field_stop (THRIFT_PROTOCOL (protocol), &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("", b) == 0);

  /* test write map begin */
  memset (read, 0 ,100);
  ret = thrift_json_protocol_write_map_begin (THRIFT_PROTOCOL (protocol), T_STRUCT, T_LIST, 32, &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("[\"rec\",\"lst\",32,{", b) == 0);

  /* test write map end */
  memset (read, 0 ,100);
  ret = thrift_json_protocol_write_map_end (THRIFT_PROTOCOL (protocol), &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("}]", b) == 0);

  /* test write list begin */
  memset (read, 0 ,100);
  ret = thrift_json_protocol_write_list_begin (THRIFT_PROTOCOL (protocol), T_STRUCT, 32, &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("[\"rec\",32", b) == 0);

  /* test write list end */
  memset (read, 0 ,100);
  ret = thrift_json_protocol_write_list_end (THRIFT_PROTOCOL (protocol), &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("]", b) == 0);

  /* test write set begin */
  memset (read, 0 ,100);
  ret = thrift_json_protocol_write_set_begin (THRIFT_PROTOCOL (protocol), T_STRUCT, 32, &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("[\"rec\",32", b) == 0);

  /* test write set end */
  memset (read, 0 ,100);
  ret = thrift_json_protocol_write_set_end (THRIFT_PROTOCOL (protocol), &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("]", b) == 0);

  /* test write bool */
  memset (read, 0 ,100);
  ret = THRIFT_PROTOCOL_GET_CLASS (protocol)->write_bool (THRIFT_PROTOCOL (protocol), TRUE, &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("1", b) == 0);

  memset (read, 0 ,100);
  ret = THRIFT_PROTOCOL_GET_CLASS (protocol)->write_bool (THRIFT_PROTOCOL (protocol), FALSE, &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("0", b) == 0);

  /* test write byte */
  memset (read, 0 ,100);
  ret = THRIFT_PROTOCOL_GET_CLASS (protocol)->write_byte (THRIFT_PROTOCOL (protocol), 100, &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("100", b) == 0);

  /* test write i16 */
  memset (read, 0 ,100);
  ret = THRIFT_PROTOCOL_GET_CLASS (protocol)->write_i16 (THRIFT_PROTOCOL (protocol), 1000, &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("1000", b) == 0);

  /* test write i32 */
  memset (read, 0 ,100);
  ret = THRIFT_PROTOCOL_GET_CLASS (protocol)->write_i32 (THRIFT_PROTOCOL (protocol), 300000000, &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("300000000", b) == 0);

  /* test write i64 */
  memset (read, 0 ,100);
  ret = THRIFT_PROTOCOL_GET_CLASS (protocol)->write_i64 (THRIFT_PROTOCOL (protocol), 6000000000, &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("6000000000", b) == 0);

  /* test write double */
  memset (read, 0, 100);
  ret = THRIFT_PROTOCOL_GET_CLASS (protocol)->write_double (THRIFT_PROTOCOL (protocol), 12.123, &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp("12.123", b) == 0);

  memset (read, 0, 100);
  ret = THRIFT_PROTOCOL_GET_CLASS (protocol)->write_double (THRIFT_PROTOCOL (protocol), (-3.14), &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp("-3.14", b) == 0);

  memset (read, 0, 100);
  ret = THRIFT_PROTOCOL_GET_CLASS (protocol)->write_double (THRIFT_PROTOCOL (protocol), ((+1.0/0.0)/(+1.0/0.0)), &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("\"NaN\"", b) == 0);

  memset (read, 0, 100);
  ret = THRIFT_PROTOCOL_GET_CLASS (protocol)->write_double (THRIFT_PROTOCOL (protocol), (+1.0/0.0), &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("\"Infinity\"", b) == 0);

  memset (read, 0, 100);
  ret = THRIFT_PROTOCOL_GET_CLASS (protocol)->write_double (THRIFT_PROTOCOL (protocol), (-1.0/0.0), &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("\"-Infinity\"", b) == 0);
 
  /* test write string */
  memset (read, 0, 100);
  ret = THRIFT_PROTOCOL_GET_CLASS (protocol)->write_string (THRIFT_PROTOCOL (protocol), "this is a test string", &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("\"this is a test string\"", b) == 0);

  /* test write binary */
  memset (read, 0, 100);
  ret = THRIFT_PROTOCOL_GET_CLASS (protocol)->write_binary (THRIFT_PROTOCOL (protocol), "this is a base64 string", 23, &error);
  g_assert (error == NULL);

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->read (THRIFT_TRANSPORT (tbuffer), b, ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("\"dGhpcyBpcyBhIGJhc2U2NCBzdHJpbmc\"", b) == 0);

  /* test get type name for type id */
  g_assert (thrift_json_protocol_get_typename_for_typeid (protocol, T_STOP, NULL) == NULL);
  g_assert (thrift_json_protocol_get_typename_for_typeid (protocol, T_VOID, NULL) == NULL);
  g_assert (strcmp ("tf", thrift_json_protocol_get_typename_for_typeid (protocol, T_BOOL, NULL)) == 0);
  g_assert (strcmp ("i8", thrift_json_protocol_get_typename_for_typeid (protocol, T_BYTE, NULL)) == 0);
  g_assert (strcmp ("dbl", thrift_json_protocol_get_typename_for_typeid (protocol, T_DOUBLE, NULL)) == 0);
  g_assert (strcmp ("i16", thrift_json_protocol_get_typename_for_typeid (protocol, T_I16, NULL)) == 0);
  g_assert (strcmp ("i32", thrift_json_protocol_get_typename_for_typeid (protocol, T_I32, NULL)) == 0);
  g_assert (strcmp ("i64", thrift_json_protocol_get_typename_for_typeid (protocol, T_I64, NULL)) == 0);
  g_assert (strcmp ("str", thrift_json_protocol_get_typename_for_typeid (protocol, T_STRING, NULL)) == 0);
  g_assert (strcmp ("rec", thrift_json_protocol_get_typename_for_typeid (protocol, T_STRUCT, NULL)) == 0);
  g_assert (strcmp ("map", thrift_json_protocol_get_typename_for_typeid (protocol, T_MAP, NULL)) == 0);
  g_assert (strcmp ("set", thrift_json_protocol_get_typename_for_typeid (protocol, T_SET, NULL)) == 0);
  g_assert (strcmp ("lst", thrift_json_protocol_get_typename_for_typeid (protocol, T_LIST, NULL)) == 0);

  /* test get type id for type name */
  g_assert (thrift_json_protocol_get_typeid_for_typename (protocol, "pp", NULL) == T_STOP);
  g_assert (thrift_json_protocol_get_typeid_for_typename (protocol, "tf", NULL) == T_BOOL);
  g_assert (thrift_json_protocol_get_typeid_for_typename (protocol, "i8", NULL) == T_BYTE);
  g_assert (thrift_json_protocol_get_typeid_for_typename (protocol, "dbl", NULL) == T_DOUBLE);
  g_assert (thrift_json_protocol_get_typeid_for_typename (protocol, "i16", NULL) == T_I16);
  g_assert (thrift_json_protocol_get_typeid_for_typename (protocol, "i32", NULL) == T_I32);
  g_assert (thrift_json_protocol_get_typeid_for_typename (protocol, "i64", NULL) == T_I64);
  g_assert (thrift_json_protocol_get_typeid_for_typename (protocol, "str", NULL) == T_STRING);
  g_assert (thrift_json_protocol_get_typeid_for_typename (protocol, "rec", NULL) == T_STRUCT);
  g_assert (thrift_json_protocol_get_typeid_for_typename (protocol, "map", NULL) == T_MAP);
  g_assert (thrift_json_protocol_get_typeid_for_typename (protocol, "set", NULL) == T_SET);
  g_assert (thrift_json_protocol_get_typeid_for_typename (protocol, "lst", NULL) == T_LIST);

  /* test read json syntax char */
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "F", 1, &error);
  g_assert (error == NULL);

  g_assert (thrift_json_protocol_read_json_syntax_char (protocol, 'H', &error) == -1);
  g_assert (error != NULL);
  g_error_free (error);
  error = NULL;

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "H", 1, &error);
  g_assert (error == NULL);

  g_assert (thrift_json_protocol_read_syntax_char (protocol, 'H', &error) == 1);
  g_assert (error == NULL);

  /* test read json escape char */
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "0054", 4, &error);
  g_assert (error == NULL);

  thrift_json_protocol_read_json_escape_char (protocol, &c, &error);
  g_assert (error == NULL);
  g_assert (c == 'T');

  /* test read json string */
  if (str != NULL) {
    g_free (str);
    str = NULL;
  }
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "\"\\\"\"", 4, &error);
  g_assert (error == NULL);

  thrift_json_protocol_read_json_string (protocol, &str, FALSE, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("\"", str) == 0);

  if (str != NULL) {
    g_free (str);
    str = NULL;
  }
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "\"\\\\\"", 4, &error);
  g_assert (error == NULL);

  thrift_json_protocol_read_json_string (protocol, &str, FALSE, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("\\", str) == 0);

  if (str != NULL) {
    g_free (str);
    str = NULL;
  }
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "\"\\/\"", 4, &error);
  g_assert (error == NULL);

  thrift_json_protocol_read_json_string (protocol, &str, FALSE, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("\/", str) == 0);

  if (str != NULL) {
    g_free (str);
    str = NULL;
  }
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "\"\\b\"", 4, &error);
  g_assert (error == NULL);

  thrift_json_protocol_read_json_string (protocol, &str, FALSE, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("\b", str) == 0);

  if (str != NULL) {
    g_free (str);
    str = NULL;
  }
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "\"\\f\"", 4, &error);
  g_assert (error == NULL);

  thrift_json_protocol_read_json_string (protocol, &str, FALSE, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("\f", str) == 0);

  if (str != NULL) {
    g_free (str);
    str = NULL;
  }
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "\"\\n\"", 4, &error);
  g_assert (error == NULL);

  thrift_json_protocol_read_json_string (protocol, &str, FALSE, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("\n", str) == 0);

  if (str != NULL) {
    g_free (str);
    str = NULL;
  }
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "\"\\r\"", 4, &error);
  g_assert (error == NULL);

  thrift_json_protocol_read_json_string (protocol, &str, FALSE, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("\r", str) == 0);

  if (str != NULL) {
    g_free (str);
    str = NULL;
  }
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "\"\\t\"", 4, &error);
  g_assert (error == NULL);

  thrift_json_protocol_read_json_string (protocol, &str, FALSE, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("\t", str) == 0);

  if (str != NULL) {
    g_free (str);
    str = NULL;
  }
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "\"this is a test string\"", 23, &error);
  g_assert (error == NULL);

  thrift_json_protocol_read_json_string (protocol, &str, FALSE, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("this is a test string", str) == 0);

  /* test read json base64 */
  if (str != NULL) {
    g_free (str);
    str = NULL;
  }
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "\"dGhpcyBpcyBhIHRlc3Qgc3RyaW5n\"", 30, &error);

  thrift_json_protocol_read_json_base64 (protocol, &str, &ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("this is a test string", str) == 0);

  /* test read json numeric */
  g_assert (thrift_json_protocol_is_json_numeric('A') == FALSE);
  g_assert (thrift_json_protocol_is_json_numeric('+'));
  g_assert (thrift_json_protocol_is_json_numeric('-'));
  g_assert (thrift_json_protocol_is_json_numeric('.'));
  g_assert (thrift_json_protocol_is_json_numeric('0'));
  g_assert (thrift_json_protocol_is_json_numeric('1'));
  g_assert (thrift_json_protocol_is_json_numeric('2'));
  g_assert (thrift_json_protocol_is_json_numeric('3'));
  g_assert (thrift_json_protocol_is_json_numeric('4'));
  g_assert (thrift_json_protocol_is_json_numeric('5'));
  g_assert (thrift_json_protocol_is_json_numeric('6'));
  g_assert (thrift_json_protocol_is_json_numeric('7'));
  g_assert (thrift_json_protocol_is_json_numeric('8'));
  g_assert (thrift_json_protocol_is_json_numeric('9'));
  g_assert (thrift_json_protocol_is_json_numeric('E'));
  g_assert (thrift_json_protocol_is_json_numeric('e'));

  /* test read json numeric chars */
  if (str != NULL) {
    g_free (str);
    str = NULL;
  }
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "1.453E45T", 9, &error);
  g_assert (error == NULL);

  thrift_json_protocol_read_json_numeric_chars (protocol, &str, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("1.453E45", str) == 0);
  protocol->reader->hasData = FALSE;

  /* test read json integer */
  num = 0;
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "1.45\"", 5, &error);
  g_assert (error == NULL);

  thrift_json_protocol_read_json_integer (protocol, &num, &error);
  g_assert (error != NULL);
  g_error_free (error);
  error = NULL;
  protocol->reader->hasData = FALSE;

  num = 0;
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "7890T", 5, &error);
  g_assert (error == NULL);

  thrift_json_protocol_read_json_integer (protocol, &num, &error);
  g_assert (error == NULL);
  g_assert (num == 7890);
  protocol->reader->hasData = FALSE;

  /* test read json double */
  dnum = 0;
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "1.453e01\"", 9, &error);
  g_assert (error == NULL);
 
  thrift_json_protocol_read_json_double (protocol, &dnum, &error);
  g_assert (error == NULL);
  g_assert (dnum == 14.53);
  protocol->reader->hasData = FALSE;

  dnum = 0;
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "\"NaN\"", 5, &error);
  g_assert (error == NULL);
 
  thrift_json_protocol_read_json_double (protocol, &dnum, &error);
  g_assert (error == NULL);
  g_assert (TRUE);
  protocol->reader->hasData = FALSE;

  dnum = 0;
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "\"Infinity\"", 10, &error);
  g_assert (error == NULL);
 
  thrift_json_protocol_read_json_double (protocol, &dnum, &error);
  g_assert (error == NULL);
  g_assert (dnum == HUGE_VAL);
  protocol->reader->hasData = FALSE;

  dnum = 0;
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "\"-Infinity\"", 11, &error);
  g_assert (error == NULL);
 
  thrift_json_protocol_read_json_double (protocol, &dnum, &error);
  g_assert (error == NULL);
  g_assert (dnum == -HUGE_VAL);
  protocol->reader->hasData = FALSE;

  /* test read json object begin*/
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "{", 1, &error);
  g_assert (error == NULL);

  g_assert (thrift_json_protocol_read_json_object_start (protocol, &error) > 0);
  g_assert (error == NULL);

  /* test read json object end*/
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "}", 1, &error);
  g_assert (error == NULL);

  g_assert (thrift_json_protocol_read_json_object_end (protocol, &error) > 0);
  g_assert (error == NULL);

  /* test read json array begin*/
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "[", 1, &error);
  g_assert (error == NULL);

  g_assert (thrift_json_protocol_read_json_array_start (protocol, &error) > 0);
  g_assert (error == NULL);

  /* test read json array end*/
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "]", 1, &error);
  g_assert (error == NULL);

  g_assert (thrift_json_protocol_read_json_array_end (protocol, &error) > 0);
  g_assert (error == NULL);

  /* test read json message begin */
  if (str != NULL) {
    g_free (str);
    str = NULL;
  }
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "[2,", 3, &error);
  g_assert (error == NULL);

  g_assert (thrift_json_protocol_read_message_begin (THRIFT_PROTOCOL (protocol), &str, &message_type, &seqid, &error) < 0);
  g_assert (error != NULL);
  g_error_free (error);
  error = NULL;

  protocol->reader->hasData = FALSE;
  if (str != NULL) {
    g_free (str);
    str = NULL;
  }
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "[1,\"name\",12,32\"", strlen ("[1,\"name\",12,32\""), NULL);
  g_assert (error == NULL);

  g_assert (thrift_json_protocol_read_message_begin (THRIFT_PROTOCOL (protocol), &str, &message_type, &seqid, &error) > 0);
  g_assert (error == NULL);
  g_assert (strcmp ("name", str) == 0);
  g_assert (message_type == 12);
  g_assert (seqid == 32);

  /* test read message end */
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "]", 1, &error);
  g_assert (error == NULL);

  g_assert (thrift_json_protocol_read_message_end (THRIFT_PROTOCOL (protocol), &error) > 0);
  g_assert (error == NULL);

  /* test read struct begin */
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "{", 1, &error);
  g_assert (error == NULL);

  g_assert (thrift_json_protocol_read_struct_begin (THRIFT_PROTOCOL (protocol), &str, &error) > 0);
  g_assert (error == NULL);

  /* test read struct end */
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "}", 1, &error);
  g_assert (error == NULL);

  g_assert (thrift_json_protocol_read_struct_end (THRIFT_PROTOCOL (protocol), &error) > 0);
  g_assert (error == NULL);

  /* test read field begin */
  if (str != NULL) {
    g_free (str);
    str = NULL;
  }
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "1{\"rec\"", 7, &error);
  g_assert (error == NULL);

  thrift_json_protocol_read_field_begin (THRIFT_PROTOCOL (protocol), &str, &message_type, &field_id, &error);
  g_assert (str == NULL);
  g_assert (message_type == 12);
  g_assert (field_id == 1);

  /* test read field end */
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "}", 1, &error);
  g_assert (error == NULL);

  g_assert (thrift_json_protocol_read_field_end (THRIFT_PROTOCOL (protocol), &error) > 0);
  g_assert (error == NULL);

  /* test read map begin */
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "[\"rec\",\"lst\",2,{", 16, &error);
  g_assert (error == NULL);

  thrift_json_protocol_read_map_begin (THRIFT_PROTOCOL (protocol), &message_type, &value_type, &ret, &error);
  g_assert (error == NULL);
  g_assert (message_type == 12);
  g_assert (value_type == 15);
  g_assert (ret == 2);

  /* test read map end */
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "}]", 2, &error);
  g_assert (error == NULL);

  thrift_json_protocol_read_map_end (THRIFT_PROTOCOL (protocol), &error);
  g_assert (error == NULL);

  /* test read list begin */
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "[\"rec\",2\"", 9, &error);
  g_assert (error == NULL);

  thrift_json_protocol_read_list_begin (THRIFT_PROTOCOL (protocol), &message_type, &ret, &error);
  g_assert (error == NULL);
  g_assert (message_type == 12);
  g_assert (ret == 2);

  /* test read list end */
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "]", 1, &error);
  g_assert (error == NULL);

  thrift_json_protocol_read_list_end (THRIFT_PROTOCOL (protocol), &error);
  g_assert (error == NULL);

  /* test read set begin */
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "[\"rec\",2\"", 9, &error);
  g_assert (error == NULL);

  thrift_json_protocol_read_set_begin (THRIFT_PROTOCOL (protocol), &message_type, &ret, &error);
  g_assert (error == NULL);
  g_assert (message_type == 12);
  g_assert (ret == 2);

  /* test read list end */
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "]", 1, &error);
  g_assert (error == NULL);

  thrift_json_protocol_read_list_end (THRIFT_PROTOCOL (protocol), &error);
  g_assert (error == NULL);

  /* test read bool */
  gboolean boolean;
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "0\"", 2, &error);
  g_assert (error == NULL);

  THRIFT_PROTOCOL_GET_CLASS (protocol)->read_bool (THRIFT_PROTOCOL (protocol), &boolean, &error);
  g_assert (error == NULL);
  g_assert (boolean == FALSE);
  protocol->reader->hasData = FALSE;

  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "1\"", 2, &error);
  g_assert (error == NULL);

  THRIFT_PROTOCOL_GET_CLASS (protocol)->read_bool (THRIFT_PROTOCOL (protocol), &boolean, &error);
  g_assert (error == NULL);
  g_assert (boolean == TRUE);
  protocol->reader->hasData = FALSE;

  /*test read byte */
  gint8 byte;
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "60\"", 3, &error);
  g_assert (error == NULL);

  THRIFT_PROTOCOL_GET_CLASS (protocol)->read_byte (THRIFT_PROTOCOL (protocol), &byte, &error);
  g_assert (error == NULL);
  g_assert (byte == 60);
  protocol->reader->hasData = FALSE;
 
  /*test read i16 */
  gint16 i16;
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "1000\"", 5, &error);
  g_assert (error == NULL);

  THRIFT_PROTOCOL_GET_CLASS (protocol)->read_i16 (THRIFT_PROTOCOL (protocol), &i16, &error);
  g_assert (error == NULL);
  g_assert (i16 == 1000);
  protocol->reader->hasData = FALSE;

  /*test read i32 */
  gint32 i32;
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "300000000\"", 10, &error);
  g_assert (error == NULL);

  THRIFT_PROTOCOL_GET_CLASS (protocol)->read_i32 (THRIFT_PROTOCOL (protocol), &i32, &error);
  g_assert (error == NULL);
  g_assert (i32 == 300000000);
  protocol->reader->hasData = FALSE;

  /*test read i64 */
  gint64 i64;
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "6000000000\"", 11, &error);
  g_assert (error == NULL);

  THRIFT_PROTOCOL_GET_CLASS (protocol)->read_i64 (THRIFT_PROTOCOL (protocol), &i64, &error);
  g_assert (error == NULL);
  g_assert (i64 == 6000000000);
  protocol->reader->hasData = FALSE;

  /* test read double */
  dnum = 0;
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "1.453e01\"", 9, &error);
  g_assert (error == NULL);
 
  THRIFT_PROTOCOL_GET_CLASS (protocol)->read_double (THRIFT_PROTOCOL (protocol), &dnum, &error);
  g_assert (error == NULL);
  g_assert (dnum == 14.53);
  protocol->reader->hasData = FALSE;

  dnum = 0;
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "\"NaN\"", 5, &error);
  g_assert (error == NULL);
 
  THRIFT_PROTOCOL_GET_CLASS (protocol)->read_double (THRIFT_PROTOCOL (protocol), &dnum, &error);
  g_assert (error == NULL);
  g_assert (TRUE);
  protocol->reader->hasData = FALSE;

  dnum = 0;
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "\"Infinity\"", 10, &error);
  g_assert (error == NULL);
 
  THRIFT_PROTOCOL_GET_CLASS (protocol)->read_double (THRIFT_PROTOCOL (protocol), &dnum, &error);
  g_assert (error == NULL);
  g_assert (dnum == HUGE_VAL);
  protocol->reader->hasData = FALSE;

  dnum = 0;
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "\"-Infinity\"", 11, &error);
  g_assert (error == NULL);
 
  THRIFT_PROTOCOL_GET_CLASS (protocol)->read_double (THRIFT_PROTOCOL (protocol), &dnum, &error);
  g_assert (error == NULL);
  g_assert (dnum == -HUGE_VAL);
  protocol->reader->hasData = FALSE;

  /* test read string */
  if (str != NULL) {
    g_free (str);
    str = NULL;
  }
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "\"this is a json string\"", 23, &error);
  g_assert (error == NULL);

  THRIFT_PROTOCOL_GET_CLASS (protocol)->read_string (THRIFT_PROTOCOL (protocol), &str, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("this is a json string", str) == 0);

  /* test read binary */
  if (str != NULL) {
    g_free (str);
    str = NULL;
  }
  THRIFT_TRANSPORT_GET_CLASS(tbuffer)->write (THRIFT_TRANSPORT (tbuffer), "\"dGhpcyBpcyBhIHRlc3Qgc3RyaW5n\"", 30, &error);
  g_assert (error == NULL);

  THRIFT_PROTOCOL_GET_CLASS (protocol)->read_binary (THRIFT_PROTOCOL (protocol), &str, &ret, &error);
  g_assert (error == NULL);
  g_assert (strcmp ("this is a test string", str) == 0);

  /* clean up memory */
  g_object_unref (protocol);
  g_object_unref (reader);
  g_object_unref (tbuffer);
  g_free (str);
}

int
main(int argc, char *argv[])
{
#if (!GLIB_CHECK_VERSION (2, 36, 0))
  g_type_init();
#endif

  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/testjsonprotocol/CreateAndDestroy", test_create_and_destroy);
  g_test_add_func ("/testjsonprotocol/Initialize", test_initialize);
  g_test_add_func ("/testjsonprotocol/ReadAndWrite", test_read_and_write);

  return g_test_run ();
}
