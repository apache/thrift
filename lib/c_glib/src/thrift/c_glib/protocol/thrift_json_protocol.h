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

#ifndef _THRIFT_JSON_PROTOCOL_H
#define _THRIFT_JSON_PROTOCOL_H

#include <glib-object.h>
#include <gmodule.h>

#include <thrift/c_glib/protocol/thrift_protocol.h>
#include <thrift/c_glib/transport/thrift_transport.h>
#include <thrift/c_glib/protocol/thrift_look_ahead_reader.h>
#include <thrift/c_glib/protocol/thrift_json_context.h>
#include <thrift/c_glib/protocol/thrift_json_pair_context.h>
#include <thrift/c_glib/protocol/thrift_json_list_context.h>

G_BEGIN_DECLS

/*! \file thrift_json_protocol.h
 *  \brief Binary protocol implementation of a Thrift protocol. Implements the
 *         ThriftProtocol interface.
 */

/* type macros */
#define THRIFT_TYPE_JSON_PROTOCOL (thrift_json_protocol_get_type ())
#define THRIFT_JSON_PROTOCOL(obj)                               \
        (G_TYPE_CHECK_INSTANCE_CAST ((obj),                     \
                                     THRIFT_TYPE_JSON_PROTOCOL, \
                                     ThriftJsonProtocol))
#define THRIFT_IS_JSON_PROTOCOL(obj)                            \
        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), THRIFT_TYPE_JSON_PROTOCOL))
#define THRIFT_JSON_PROTOCOL_CLASS(c)                           \
        (G_TYPE_CHECK_CLASS_CAST ((c),                          \
                                  THRIFT_TYPE_BINARY_PROTOCOL,  \
                                  ThriftJsonProtocolClass))
#define THRIFT_IS_JSON_PROTOCOL_CLASS(c)                        \
        (G_TYPE_CHECK_CLASS_TYPE ((c), THRIFT_TYPE_COMPACT_PROTOCOL))
#define THRIFT_JSON_PROTOCOL_GET_CLASS(obj)                     \
        (G_TYPE_INSTANCE_GET_CLASS ((obj),                      \
                                    THRIFT_TYPE_JSON_PROTOCOL,  \
                                    ThriftJsonProtocolClass))
typedef struct _ThriftJsonProtocol ThriftJsonProtocol;

/*!
 * Thrift Json Protocol instance.
 */
struct _ThriftJsonProtocol
{
  ThriftProtocol parent;

  /* private */
  GList *context_list;
  ThriftJsonContext *context;
  ThriftLookAheadReader *reader;
};

typedef struct _ThriftJsonProtocolClass ThriftJsonProtocolClass;

/*!
 * Thrift Json Protocol class.
 */
struct _ThriftJsonProtocolClass
{
  ThriftProtocolClass parent;
};

/* used by THRIFT_TYPE_JSON_PROTOCOL */
GType thrift_json_protocol_get_type (void);

gint
thrift_json_protocol_get_min_serialized_size(ThriftProtocol *protocol, ThriftType type, GError **error);

gint32
thrift_json_protocol_write_field_stop (ThriftProtocol *protocol, GError **error);

G_END_DECLS

#endif /* _THRIFT_JSON_PROTOCOL_H */

