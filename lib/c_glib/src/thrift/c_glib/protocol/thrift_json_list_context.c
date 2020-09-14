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

#include <thrift/c_glib/thrift.h>
#include <thrift/c_glib/protocol/thrift_json_list_context.h>

G_DEFINE_TYPE(ThriftJsonListContext, thrift_json_list_context, THRIFT_TYPE_JSON_CONTEXT)

static const gint8 KJSONElemSeparator = ',';

gint32
thrift_json_list_context_write (ThriftJsonContext *json_context, ThriftTransport *transport, GError **error)
{
  ThriftJsonListContext *json_list_context = THRIFT_JSON_LIST_CONTEXT (json_context);

  if (json_list_context->first) {
    json_list_context->first = FALSE;
    return 0;
  } else {
    thrift_transport_write (transport, (const gpointer)&KJSONElemSeparator, 1, error);
    return 1;
  }
}

gint32 
thrift_json_list_context_read (ThriftJsonContext *json_context, ThriftLookAheadReader *reader, GError **error)
{
  ThriftJsonListContext *json_list_context = THRIFT_JSON_LIST_CONTEXT (json_context);

  if (json_list_context->first) {
    json_list_context->first = FALSE;
    return 0;
  } else {
    return thrift_look_ahead_reader_read_syntax_char (reader, KJSONElemSeparator, error);
  }
}

gboolean
thrift_json_list_context_escape_num (ThriftJsonContext *json_context)
{
  THRIFT_UNUSED_VAR (json_context);
  return FALSE;
}

/* initializes the instance */
static void
thrift_json_list_context_init (ThriftJsonListContext *json_list_context)
{
  json_list_context->first = TRUE;
}

/* initializes the class */
static void
thrift_json_list_context_class_init (ThriftJsonListContextClass *cls)
{
  ThriftJsonContextClass *tjcc = THRIFT_JSON_CONTEXT_CLASS (cls);

  tjcc->write = thrift_json_list_context_write;
  tjcc->read = thrift_json_list_context_read;
  tjcc->escape_num = thrift_json_list_context_escape_num;
}
