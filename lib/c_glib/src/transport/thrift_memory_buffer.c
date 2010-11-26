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

#include <assert.h>
#include <netdb.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <unistd.h>

#include "thrift.h"
#include "transport/thrift_transport.h"
#include "transport/thrift_memory_buffer.h"

/* object properties */
enum _ThriftMemoryBufferProperties
{
  PROP_0,
  PROP_THRIFT_MEMORY_BUFFER_BUFFER_SIZE,
};

G_DEFINE_TYPE(ThriftMemoryBuffer, thrift_memory_buffer, THRIFT_TYPE_TRANSPORT)

/* implements thrift_transport_is_open */
gboolean
thrift_memory_buffer_is_open (ThriftTransport *transport)
{
  THRIFT_UNUSED_VAR (transport);
  return TRUE;
}

/* implements thrift_transport_open */
gboolean
thrift_memory_buffer_open (ThriftTransport *transport, GError **error)
{
  THRIFT_UNUSED_VAR (transport);
  THRIFT_UNUSED_VAR (error);
  return TRUE;
}

/* implements thrift_transport_close */
gboolean
thrift_memory_buffer_close (ThriftTransport *transport, GError **error)
{
  THRIFT_UNUSED_VAR (transport);
  THRIFT_UNUSED_VAR (error);
  return TRUE;
}

/* implements thrift_transport_read */
gint32
thrift_memory_buffer_read (ThriftTransport *transport, gpointer buf,
                           guint32 len, GError **error)
{
  THRIFT_UNUSED_VAR (error);
  ThriftMemoryBuffer *t = THRIFT_MEMORY_BUFFER (transport);
  guint32 give = len; 

  /* if the requested bytes are more than what we have available,
   * just give all that we have the buffer */
  if (t->buf->len < len)
  {
    give = t->buf->len;
  }

  memcpy (buf, t->buf->data, give);
  g_byte_array_remove_range (t->buf, 0, give);

  return give;
}

/* implements thrift_transport_read_end
 * called when read is complete.  nothing to do on our end. */
gboolean
thrift_memory_buffer_read_end (ThriftTransport *transport, GError **error)
{
  /* satisfy -Wall */
  THRIFT_UNUSED_VAR (transport);
  THRIFT_UNUSED_VAR (error);
  return TRUE;
}

/* implements thrift_transport_write */
gboolean
thrift_memory_buffer_write (ThriftTransport *transport,
                            const gpointer buf,     
                            const guint32 len, GError **error)
{
  THRIFT_UNUSED_VAR (error);

  ThriftMemoryBuffer *t = THRIFT_MEMORY_BUFFER (transport);

  /* return an exception if the buffer doesn't have enough space. */
  if (len > t->buf_size - t->buf->len)
  {
    g_set_error (error, THRIFT_TRANSPORT_ERROR, THRIFT_TRANSPORT_ERROR_SEND,
                 "unable to write %d bytes to buffer of length %d",
                 len, t->buf_size);
    return FALSE;
  } else {
    t->buf = g_byte_array_append (t->buf, buf, len);
    return TRUE;
  }
}

/* implements thrift_transport_write_end
 * called when write is complete.  nothing to do on our end. */
gboolean
thrift_memory_buffer_write_end (ThriftTransport *transport, GError **error)
{
  /* satisfy -Wall */
  THRIFT_UNUSED_VAR (transport);
  THRIFT_UNUSED_VAR (error);
  return TRUE;
}

/* implements thrift_transport_flush */
gboolean
thrift_memory_buffer_flush (ThriftTransport *transport, GError **error)
{
  THRIFT_UNUSED_VAR (transport);
  THRIFT_UNUSED_VAR (error);

  return TRUE;
}

/* initializes the instance */
static void
thrift_memory_buffer_init (ThriftMemoryBuffer *transport)
{
  transport->buf = g_byte_array_new ();
}

/* destructor */
static void
thrift_memory_buffer_finalize (GObject *object)
{
  ThriftMemoryBuffer *transport = THRIFT_MEMORY_BUFFER (object);

  if (transport->buf != NULL)
  {
    g_byte_array_free (transport->buf, TRUE);
  }
  transport->buf = NULL;
}

/* property accessor */
void
thrift_memory_buffer_get_property (GObject *object, guint property_id,
                                   GValue *value, GParamSpec *pspec)
{
  THRIFT_UNUSED_VAR (pspec);
  ThriftMemoryBuffer *transport = THRIFT_MEMORY_BUFFER (object);

  switch (property_id)
  {
    case PROP_THRIFT_MEMORY_BUFFER_BUFFER_SIZE:
      g_value_set_uint (value, transport->buf_size);
      break;
  }
}

/* property mutator */
void
thrift_memory_buffer_set_property (GObject *object, guint property_id,
                                   const GValue *value, GParamSpec *pspec)
{
  THRIFT_UNUSED_VAR (pspec);
  ThriftMemoryBuffer *transport = THRIFT_MEMORY_BUFFER (object);

  switch (property_id)
  {
    case PROP_THRIFT_MEMORY_BUFFER_BUFFER_SIZE:
      transport->buf_size = g_value_get_uint (value);
      break;
  }
}

/* initializes the class */
static void
thrift_memory_buffer_class_init (ThriftMemoryBufferClass *cls)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (cls);
  GParamSpec *param_spec = NULL;

  /* setup accessors and mutators */
  gobject_class->get_property = thrift_memory_buffer_get_property;
  gobject_class->set_property = thrift_memory_buffer_set_property;

  param_spec = g_param_spec_uint ("buf_size",
                                  "buffer size (construct)",
                                  "Set the read buffer size",
                                  0, /* min */
                                  1048576, /* max, 1024*1024 */
                                  512, /* default value */
                                  G_PARAM_CONSTRUCT_ONLY |
                                  G_PARAM_READWRITE);
  g_object_class_install_property (gobject_class,
                                   PROP_THRIFT_MEMORY_BUFFER_BUFFER_SIZE,
                                   param_spec);

  ThriftTransportClass *ttc = THRIFT_TRANSPORT_CLASS (cls);

  gobject_class->finalize = thrift_memory_buffer_finalize;
  ttc->is_open = thrift_memory_buffer_is_open;
  ttc->open = thrift_memory_buffer_open;
  ttc->close = thrift_memory_buffer_close;
  ttc->read = thrift_memory_buffer_read;
  ttc->read_end = thrift_memory_buffer_read_end;
  ttc->write = thrift_memory_buffer_write;
  ttc->write_end = thrift_memory_buffer_write_end;
  ttc->flush = thrift_memory_buffer_flush;
}
