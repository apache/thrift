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

#ifndef _THRIFT_JSON_PAIR_CONTEXT_H
#define _THRIFT_JSON_PAIR_CONTEXT_H

#include <glib-object.h>
#include <thrift/c_glib/protocol/thrift_json_context.h>

G_BEGIN_DECLS

/*! \file thrift_json_pair_context.h
 *  \brief Abstract class for Thrift Json context.
 *  Context class for object member key-value pairs.
 */

/* type macros */
#define THRIFT_TYPE_JSON_PAIR_CONTEXT (thrift_json_pair_context_get_type ())
#define THRIFT_JSON_PAIR_CONTEXT(obj)                               \
        (G_TYPE_CHECK_INSTANCE_CAST ((obj),                         \
                                     THRIFT_TYPE_JSON_PAIR_CONTEXT, \
                                     ThriftJsonPairContext))
#define THRIFT_IS_JSON_PAIR_CONTEXT(obj)                            \
        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), THRIFT_TYPE_JSON_PAIR_CONTEXT))
#define THRIFT_JSON_PAIR_CONTEXT_CLASS(c)                           \
        (G_TYPE_CHECK_CLASS_CAST ((c),                              \
                                  THRIFT_TYPE_JSON_PAIR_CONTEXT,    \
                                  ThriftJsonPairContextClass))
#define THRIFT_IS_JSON_PAIR_CONTEXT_CLASS(c)                        \
        (G_TYPE_CHECK_CLASS_TYPE ((c), THRIFT_TYPE_JSON_PAIR_CONTEXT))
#define THRIFT_JSON_PAIR_CONTEXT_GET_CLASS(obj) \
        (G_TYPE_INSTANCE_GET_CLASS ((obj), \
                                    THRIFT_JSON_PAIR_CONTEXT, \
                                    ThriftJsonPairContextClass))

typedef struct _ThriftJsonPairContext ThriftJsonPairContext;

/*!
 * Thrift Json Pair Context instance.
 */
struct _ThriftJsonPairContext
{
  ThriftJsonContext parent;

  /* private */
  gboolean first;
  gboolean colon;
};

typedef struct _ThriftJsonPairContextClass ThriftJsonPairContextClass;

/*!
 * Thrift Json Pair Context Class.
 */
struct _ThriftJsonPairContextClass
{
  ThriftJsonContextClass parent;
};

/* used bt THRIFT_TYPE_JSON_PAIR_CONTEXT */
GType thrift_json_pair_context_get_type (void);

G_END_DECLS

#endif

