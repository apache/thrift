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
#ifndef _THRIFT_ZLIB_TRANSPORT_H
#define _THRIFT_ZLIB_TRANSPORT_H

#include <glib.h>
#include <glib-object.h>
#include <zlib.h>

#include "thrift_transport.h"

G_BEGIN_DECLS

/*! \file thrift_zlib_transport.h
 *  \brief Class for Thrift file descriptor transports.
 */

/* type macros */
#define THRIFT_TYPE_ZLIB_TRANSPORT (thrift_zlib_transport_get_type ())
#define THRIFT_ZLIB_TRANSPORT(obj) \
  (G_TYPE_CHECK_INSTANCE_CAST ((obj), THRIFT_TYPE_ZLIB_TRANSPORT, \
                               ThriftZlibTransport))
#define THRIFT_IS_ZLIB_TRANSPORT(obj) \
  (G_TYPE_CHECK_CLASS_TYPE ((obj), THRIFT_TYPE_ZLIB_TRANSPORT))
#define THRIFT_ZLIB_TRANSPORT_CLASS(c) \
  (G_TYPE_CHECK_CLASS_CAST ((c), THRIFT_TYPE_ZLIB_TRANSPORT, \
                            ThriftZlibTransportClass))
#define THRIFT_IS_ZLIB_TRANSPORT_CLASS(c) \
  (G_TYPE_CHECK_CLASS_TYPE ((c), THRIFT_TYPE_ZLIB_TRANSPORT))
#define THRIFT_ZLIB_TRANSPORT_GET_CLASS(obj) \
  (G_TYPE_INSTANCE_GET_CLASS ((obj), THRIFT_TYPE_ZLIB_TRANSPORT, \
                              ThriftZlibTransportClass))

typedef struct _ThriftZlibTransport ThriftZlibTransport;\

struct _ThriftZlibTransport
{
  ThriftTransport parent;

  /* protected */
  ThriftTransport *transport;
  gint urbuf_size;
  gint crbuf_size;
  gint uwbuf_size;
  gint cwbuf_size;
  gint comp_level;
  ThriftConfiguration *configuration;
  glong remainingMessageSize_;
  glong knowMessageSize_;

  /* private */
  gint urpos;
  gint uwpos;
  gboolean input_ended;  /* TRUE iff zlib has reached the end of the input stream*/
  gboolean output_finished;  /* TRUE iff we have finished the ouput stream*/
  guint8* urbuf;
  guint8* crbuf;
  guint8* uwbuf;
  guint8* cwbuf;
  struct z_stream_s* rstream;
  struct z_stream_s* wstream;
};

typedef struct _ThriftZlibTransportClass ThriftZlibTransportClass;

/*!
 * Thrift Transport class
 */
struct _ThriftZlibTransportClass
{
  ThriftTransportClass parent;
};

/* used by THRIFT_TYPE_ZLIB_TRANSPORT */
GType thrift_zlib_transport_get_type (void);

gboolean
thrift_zlib_transport_verify_checksum(ThriftTransport *transport, GError **error);

gboolean
thrift_zlib_transport_finish(ThriftTransport *transport, GError **error);

G_END_DECLS

#endif /* _THRIFT_ZLIB_TRANSPORT_H */

