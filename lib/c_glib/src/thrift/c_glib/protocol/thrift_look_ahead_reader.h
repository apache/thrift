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

#ifndef _THRIFT_LOOK_AHEAD_READER_H
#define _THRIFT_LOOK_AHEAD_READER_H

#include <glib-object.h>

#include <thrift/c_glib/transport/thrift_transport.h>

G_BEGIN_DECLS

#define THRIFT_TYPE_LOOK_AHEAD_READER (thrift_look_ahead_reader_get_type ())
#define THRIFT_LOOK_AHEAD_READER(obj)                               \
        (G_TYPE_CHECK_INSTANCE_CAST ((obj),                         \
                                     THRIFT_TYPE_LOOK_AHEAD_READER, \
                                     ThriftLookAheadReader))
#define THRIFT_IS_LOOK_AHEAD_READER(obj)                            \
        (G_TYPE_CHECK_INSTANCE_TYPE ((obj), THRIFT_TYPE_LOOK_AHEAD_READER))
#define THRIFT_LOOK_AHEAD_READER_CLASS(c)                           \
        (G_TYPE_CHECK_CLASS_CAST ((c),                              \
                                  THRIFT_TYPE_LOOK_AHEAD_READER,    \
                                  ThriftLookAheadReaderClass))
#define THRIFT_IS_LOOK_AHEAD_READER_CLASS(c)                        \
        (G_TYPE_CHECK_CLASS_TYPE ((c), THRIFT_TYPE_LOOK_AHEAD_READER))
#define THRIFT_LOOK_AHEAD_READER_GET_CLASS(c)                       \
        (G_TYPE_INSTANCE_GET_CLASS ((c),                            \
                                    THRIFT_TYPE_LOOK_AHEAD_READER,  \
                                    ThriftLookAheadReaderClass))

typedef struct _ThriftLookAheadReader ThriftLookAheadReader;

/*!
 * Thrift Look Ahead Reader object
 */
struct _ThriftLookAheadReader
{
  GObject parent;

  /* protected */
  ThriftTransport *transport;

  /* private */
  gboolean hasData;
  guint8 data;
};

typedef struct _ThriftLookAheadReaderClass ThriftLookAheadReaderClass;

/*!
 * Thrift Look Ahead Reader
 */
struct _ThriftLookAheadReaderClass
{
  GObjectClass parent;

  /* vtable */
  gint8 (*read) (ThriftLookAheadReader *reader, GError **error);
  gint8 (*peek) (ThriftLookAheadReader *reader, GError **error);
};

/* used by THRIFT_TYPE_LOOK_AHEAD_READER */
GType thrift_look_ahead_reader_get_type (void);

gint8 thrift_look_ahead_reader_read (ThriftLookAheadReader *reader, GError **error);

gint8 thrift_look_ahead_reader_peek (ThriftLookAheadReader *reader, GError **error);

gint32 thrift_look_ahead_reader_read_syntax_char (ThriftLookAheadReader *reader, gint8 ch, GError **error);

G_END_DECLS

#endif
