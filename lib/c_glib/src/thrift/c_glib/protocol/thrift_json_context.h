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

#ifndef _THRIFT_JSON_CONTEXT_H
#define _THRIFT_JSON_CONTEXT_H

#include <glib-object.h>
#include <thrift/c_glib/protocol/thrift_look_ahead_reader.h>

G_BEGIN_DECLS

/*! \file thrift_json_context.h
 *  \brief Abstract class for Thrift Json context.
 *
 */

/* type macros */
#define THRIFT_TYPE_JSON_CONTEXT (thrift_json_context_get_type ())
#define THRIFT_JSON_CONTEXT(obj)                               \
        (G_TYPE_CHECK_INSTANCE_CAST ((obj),                    \
                                     THRIFT_TYPE_JSON_CONTEXT, \
                                     ThriftJsonContext))
#define THRIFT_IS_JSON_CONTEXT(obj)                            \
        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), THRIFT_TYPE_JSON_CONTEXT))
#define THRIFT_JSON_CONTEXT_CLASS(c)                           \
        (G_TYPE_CHECK_CLASS_CAST ((c),                         \
                                  THRIFT_TYPE_JSON_CONTEXT,    \
                                  ThriftJsonContextClass))
#define THRIFT_IS_JSON_CONTEXT_CLASS(c)                        \
        (G_TYPE_CHECK_CLASS_TYPE ((c), THRIFT_TYPE_JSON_CONTEXT))
#define THRIFT_JSON_CONTEXT_GET_CLASS(obj)                     \
        (G_TYPE_INSTANCE_GET_CLASS ((obj),                     \
                                    THRIFT_TYPE_JSON_CONTEXT,  \
                                    ThriftJsonContextClass))

typedef struct _ThriftJsonContext ThriftJsonContext;

/*!
 * Thrift Json Context object
 */
struct _ThriftJsonContext
{
  GObject parent;
};

typedef struct _ThriftJsonContextClass ThriftJsonContextClass;

/*!
 * Thrift Json Context Class
 */
struct _ThriftJsonContextClass
{
  GObjectClass parent;

  /* vtable */
  gint32 (*write) (ThriftJsonContext *json_context, ThriftTransport *transport, GError **error);
  gint32 (*read) (ThriftJsonContext *json_context, ThriftLookAheadReader *reader, GError **error);
  gboolean (*escape_num) (ThriftJsonContext *json_context);
};

/* used by THRIFT_TYPE_JSON_CONTEXT */
GType thrift_json_context_get_type (void);

gint32 thrift_json_context_write (ThriftJsonContext *json_context, ThriftTransport *transport, GError **error);

gint32 thrift_json_context_read (ThriftJsonContext *json_context, ThriftLookAheadReader *reader, GError **error);

gboolean thrift_json_context_escape_num (ThriftJsonContext *json_context);

G_END_DECLS

#endif
