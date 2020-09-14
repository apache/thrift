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
#include <thrift/c_glib/protocol/thrift_json_context.h>

G_DEFINE_ABSTRACT_TYPE(ThriftJsonContext, thrift_json_context, G_TYPE_OBJECT)

gint32
thrift_json_context_write (ThriftJsonContext *json_context, ThriftTransport *transport, GError **error)
{
  return THRIFT_JSON_CONTEXT_GET_CLASS (json_context)->write (json_context, transport, error);
}

gint32
thrift_json_context_read (ThriftJsonContext *json_context, ThriftLookAheadReader *reader, GError **error)
{
  return THRIFT_JSON_CONTEXT_GET_CLASS (json_context)->read (json_context, reader, error);
}

gboolean
thrift_json_context_escape_num (ThriftJsonContext *json_context)
{
  return THRIFT_JSON_CONTEXT_GET_CLASS (json_context)->escape_num (json_context);
}

/* class initializer for ThriftJsonContext */
static void
thrift_json_context_class_init (ThriftJsonContextClass *cls)
{
  cls->write = thrift_json_context_write;
  cls->read = thrift_json_context_read;
  cls->escape_num = thrift_json_context_escape_num;
}

static void
thrift_json_context_init (ThriftJsonContext *json_context)
{
  THRIFT_UNUSED_VAR (json_context);
}
