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

#ifndef _THRIFT_JSON_LIST_CONTEXT_H
#define _THRIFT_JSON_LIST_CONTEXT_H

#include <glib-object.h>
#include <thrift/c_glib/protocol/thrift_json_context.h>

G_BEGIN_DECLS

/*! \file thrift_json_list_context.h
 *  \brief Class for Thrift Json List context.
 *  context class fot lists.
 */

/* type macros */
#define THRIFT_TYPE_JSON_LIST_CONTEXT (thrift_json_list_context_get_type ())
#define THRIFT_JSON_LIST_CONTEXT(obj)                               \
        (G_TYPE_CHECK_INSTANCE_CAST ((obj),                         \
                                     THRIFT_TYPE_JSON_LIST_CONTEXT, \
                                     ThriftJsonListContext))
#define THRIFT_IS_JSON_LIST_CONTEXT(obj)                            \
        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), THRIFT_TYPE_JSON_LIST_CONTEXT))
#define THRIFT_JSON_LIST_CONTEXT_CLASS(c)                           \
        (G_TYPE_CHECK_CLASS_CAST ((c),                              \
                                  THRIFT_TYPE_JSON_LIST_CONTEXT,    \
                                  ThriftJsonListContextClass))
#define THRIFT_IS_JSON_LIST_CONTEXT_CLASS(c)                        \
        (G_TYPE_CHECK_CLASS_TYPE ((c), THRIFT_TYPE_JSON_LIST_CONTEXT))
#define THRIFT_JSON_LIST_CONTEXT_GET_CLASS(obj)                     \
        (G_TYPE_INSTANCE_GET_CLASS ((obj),                          \
                                    THRIFT_TYPE_JSON_LIST_CONTEXT,  \
                                    ThriftJsonListContextClass))

typedef struct _ThriftJsonListContext ThriftJsonListContext;

/*!
 * Thrift Json List Context object
 */
struct _ThriftJsonListContext
{
  ThriftJsonContext parent;

  /* private */
  gboolean first;
};

typedef struct _ThriftJsonListContextClass ThriftJsonListContextClass;

/*!
 * Thrift Json List Context class
 */
struct _ThriftJsonListContextClass
{
  ThriftJsonContextClass parent;
};

/* used by THRIFT_TYPE_JSON_LIST_CONTEXT */
GType thrift_json_list_context_get_type (void);

G_END_DECLS

#endif

