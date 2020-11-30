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

#include <errno.h>
#include <netdb.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>

#include <thrift/c_glib/thrift.h>
#include <thrift/c_glib/transport/thrift_transport.h>
#include <thrift/c_glib/transport/thrift_zlib_transport.h>

#define DEFAULT_URBUF_SIZE      128
#define DEFAULT_CRBUF_SIZE      1024
#define DEFAULT_UWBUF_SIZE      128
#define DEFAULT_CWBUF_SIZE      1024
#define MIN_DIRECT_DEFLATE_SIZE 32

/* object properties */
enum _ThriftZlibTransportProperties
{
  PROP_0,
  PROP_THRIFT_ZLIB_TRANSPORT_TRANSPORT,
  PROP_THRIFT_ZLIB_TRANSPORT_URBUF_SIZE,
  PROP_THRIFT_ZLIB_TRANSPORT_CRBUF_SIZE,
  PROP_THRIFT_ZLIB_TRANSPORT_UWBUF_SIZE,
  PROP_THRIFT_ZLIB_TRANSPORT_CWBUF_SIZE,
  PROP_THRIFT_ZLIB_TRANSPORT_COMP_LEVEL,
  PROP_THRIFT_ZLIB_TRANSPORT_CONFIGURATION,
  PROP_THRIFT_ZLIB_TRANSPORT_REMAINING_MESSAGE_SIZE,
  PROP_THRIFT_ZLIB_TRANSPORT_KNOW_MESSAGE_SIZE
};

G_DEFINE_TYPE (ThriftZlibTransport, thrift_zlib_transport, THRIFT_TYPE_TRANSPORT)

/*! READING STRATEGY
 * We have two buffers for reading: one containing the compressed data (crbuf)
 * and one containing the uncompressed data (urbuf).  When read is called,
 * we repeat the following steps until we have satisfied the request:
 *  - Copy data from urbuf into the caller's buffer.
 *  - If we had enough, return.
 *  - If urbuf is empty, read some data into it from the underlying transport.
 *  - Inflate data from crbuf into urbuf.
 *
 *  In standalone object, we set input_end to true when inflate returns
 *  Z_STREAM_END.  This allows to make sure that a checksum was verified.
 */
int
thrift_zlib_transport_read_avail (ThriftTransport *transport)
{
  ThriftZlibTransport *t = THRIFT_ZLIB_TRANSPORT (transport);
  return t->urbuf_size - t->rstream->avail_out - t->urpos;
}

/* overrides thrift_transport_is_open */
gboolean
thrift_zlib_transport_is_open (ThriftTransport *transport)
{
  ThriftZlibTransport *t = THRIFT_ZLIB_TRANSPORT (transport);
  return (thrift_zlib_transport_read_avail (transport) > 0) || \
         (t->rstream->avail_in > 0) || THRIFT_TRANSPORT_GET_CLASS (t->transport)->is_open (t->transport);
}

/* overrides thrift_transport_peek */
gboolean
thrift_zlib_transport_peek (ThriftTransport *transport, GError **error)
{
  ThriftZlibTransport *t = THRIFT_ZLIB_TRANSPORT (transport);
  return (thrift_zlib_transport_read_avail (transport) > 0) || \
         (t->rstream->avail_in > 0) || THRIFT_TRANSPORT_GET_CLASS (t->transport)->peek (t->transport, error);
}

/* implements thrift_transport_open */
gboolean
thrift_zlib_transport_open (ThriftTransport *transport, GError **error)
{
  ThriftZlibTransport *t = THRIFT_ZLIB_TRANSPORT (transport);
  return THRIFT_TRANSPORT_GET_CLASS (t->transport)->open (t->transport, error);
}

/* implements thrift_transport_close */
gboolean
thrift_zlib_transport_close (ThriftTransport *transport, GError **error)
{
  ThriftZlibTransport *t = THRIFT_ZLIB_TRANSPORT (transport);
  return THRIFT_TRANSPORT_GET_CLASS (t->transport)->close (t->transport, error);
}

gint32
thrift_zlib_transport_read_from_zlib(ThriftTransport *transport, GError **error)
{
  ThriftZlibTransport *t = THRIFT_ZLIB_TRANSPORT (transport);
  gint32 got = 0;
  int zlib_rv = Z_OK;

  if (t->input_ended) {
    /* If input end return error */
    return -1;           
  }

  /* If we don't have any more compressed data available,
   * read some from the underlying transport.
   */
    got = THRIFT_TRANSPORT_GET_CLASS(t->transport)->read (t->transport, t->crbuf, 1, error);
    if (got < 0) {
      return -1;
    }
    t->rstream->next_in = t->crbuf;
    t->rstream->avail_in = got;

    /* We have some compressed data now.  Uncompress it. */
    zlib_rv = inflate (t->rstream, Z_SYNC_FLUSH);
    if (zlib_rv == Z_STREAM_END) {
      t->input_ended = TRUE;
      inflateEnd(t->rstream);
      return 0;
    } else { 
     if (zlib_rv != Z_OK) {
        g_set_error (error, THRIFT_TRANSPORT_ERROR, THRIFT_TRANSPORT_ERROR_RECEIVE,
                     "zlib error: %d (status = %s)", zlib_rv, t->rstream->msg);
        /* It must to return error */
        return -1;
     } else {
       return 1;
     }
   }
  
  /* return 1 to continue to read */
  return 1;
}

/* implements thrift_transport_read */
gint32
thrift_zlib_transport_read_slow (ThriftTransport *transport, gpointer buf,
                            GError **error)
{
  ThriftZlibTransport *t = THRIFT_ZLIB_TRANSPORT (transport);
  gint *buf_tmp = buf;
  gint32 need = 1;
  gint give;
  gint32 ret = 0;

  while(TRUE)
  {
    if ((guint32)thrift_zlib_transport_read_avail (transport) < 1) {
      give = thrift_zlib_transport_read_avail (transport);
    } else {
      give = need;
    }
    memcpy (buf_tmp, t->urbuf+t->urpos, give);
    if (give > need) {
      need = 0;
    } else {
      need -= give;
    }
    buf_tmp += give;
    t->urpos += give;

    /* If they were satisfied, we are done. */
    if (need == 0) {
      return 1;
    }
    
    /* If we will need to read from the underlying transport to get more data,
     * but we already have some data available, return it now.  Reading from
     * the underlying transport may block, and read() is only allowed to block
     * when no data is available.
     */
    if (need < 1 && t->rstream->avail_in == 0) {
      return give;
    }

    /* If we get to this point, we need to get some more data. */

    /* If zlib has reported the end of a stream, we can't really do any more. */
    if (t->input_ended) {
      return 1;
    }

    /* The uncompressed read buffer is empty, so reset the stream fields. */
    t->rstream->next_out = t->urbuf;
    t->rstream->avail_out = t->urbuf_size;
    t->urpos = 0;

    /* Call inflate() to uncompress some more data. */
    if ((ret = thrift_zlib_transport_read_from_zlib(transport, error)) == 0) {
      /* no data available from underlying transport */
      return 1;
    } else {
      if (ret < 0) {
        return -1;
      }
    }
  }
  /* Okay.  The read buffer should have whatever we can give it now. */
  /* Loop back to the start and try to give some more. */
}

gint32
thrift_zlib_transport_read (ThriftTransport *transport, gpointer buf,
                             guint32 len, GError **error)
{
  ThriftZlibTransport *t = THRIFT_ZLIB_TRANSPORT (transport);
  ThriftTransportClass *ttc = THRIFT_TRANSPORT_GET_CLASS (transport);
  guint32 i;
  gint32 ret;

  if (!ttc->checkReadBytesAvailable (transport, len, error)){
    return -1;
  }

  for (i=0; i < len; i=i+ret) {
    if ((ret = thrift_zlib_transport_read_slow (transport, ((char*)buf)+i, error)) < 0) {
      return ret;
    }
    if (t->input_ended)
      break;
  }

  return len;
}

/* implements thrift_transport_read_end 
 * called when read is complete. nothing to do on our end. */
gboolean
thrift_zlib_transport_read_end (ThriftTransport *transport, GError **error)
{
  /* satisfy -Wall */
  THRIFT_UNUSED_VAR (error);
  THRIFT_UNUSED_VAR (transport);

  return TRUE;
}

gboolean
thrift_zlib_transport_flush_to_zlib (ThriftTransport *transport, const gint8* buf,
                                     gint len, gint flush, GError **error)
{
  ThriftZlibTransport *t = THRIFT_ZLIB_TRANSPORT (transport);
  t->wstream->next_in = (guchar*)buf;
  t->wstream->avail_in = len;

  while (TRUE) {
    if ((flush == Z_NO_FLUSH || flush == Z_BLOCK) && t->wstream->avail_in  == 0) {
      break;
    }

    /* If our output buffer is full, flush to the underlying transport. */
    if (t->wstream->avail_out == 0) {
      THRIFT_TRANSPORT_GET_CLASS (t->transport)->write (t->transport,
                                                        t->cwbuf, t->cwbuf_size, error);
      t->wstream->next_out = t->cwbuf;
      t->wstream->avail_out = t->cwbuf_size;
      break;
    }

    int zlib_rv = deflate(t->wstream, flush);

    if (flush == Z_FINISH && zlib_rv == Z_STREAM_END) {
      if (t->wstream->avail_in != 0) {
        g_set_error (error, THRIFT_TRANSPORT_ERROR, THRIFT_TRANSPORT_ERROR_SEND,
                     "wstream->avail_in != 0");
        return FALSE;
      }
      deflateEnd(t->wstream);
      t->output_finished = TRUE;
      break;
    }

    if (zlib_rv != Z_OK) {
      g_set_error (error, THRIFT_TRANSPORT_ERROR, THRIFT_TRANSPORT_ERROR_SEND,
                   "zlib error: %d (status = %s)", zlib_rv, t->wstream->msg);
      return FALSE;
    }

    if ((flush == Z_SYNC_FLUSH || flush == Z_FULL_FLUSH) && t->wstream->avail_in ==0
        && t->wstream->avail_out != 0) {
      break;
    }
  }
  return TRUE;
}

/* implements thrift_transport_write 
 * WRITING STRATEGY
 * We buffer up small writes before sending them to zlib, so our logic is:
 * - Is the write big?
 *   - Send the buffer to zlib.
 *   - Send this data to zlib.
 * - Is the write small?
 *   - Is there insufficient space in the buffer for it?
 *     - Send the buffer to zlib.
 *   - Copy the data to the buffer. 
 *
 * We have two buffers for writing also: the uncompressed buffer (mentioned
 * above) and the compressed buffer.  When sending data to zlib we loop over
 * the following until the source (uncompressed buffer or big write) is empty:
 * - Is there no more space in the compressed buffer?
 *   - Write the compressed buffer to the underkying transport.
 * - Deflate from the source into the compressed buffer. */
gboolean
thrift_zlib_transport_write (ThriftTransport *transport,
                             const gpointer buf,
                             const guint32 len, GError **error)
{
  ThriftZlibTransport *t = THRIFT_ZLIB_TRANSPORT (transport);
  
  if (t->output_finished) { 
    g_set_error (error, THRIFT_TRANSPORT_ERROR, THRIFT_TRANSPORT_ERROR_SEND,
		 "write() called after write_end(): %s",
		 strerror(errno));
    return FALSE;
  }

  /* zlib's "deflate" function has enough logic in it that I think
   * we're better off (performance-wise) buffering up small writes. */
  if (len > MIN_DIRECT_DEFLATE_SIZE) {
    if (!thrift_zlib_transport_flush_to_zlib (transport, (gint8*)t->uwbuf, t->uwpos, Z_NO_FLUSH, error)) {
      return FALSE;
    }
    t->uwpos = 0;
    if (!thrift_zlib_transport_flush_to_zlib (transport, buf, len, Z_NO_FLUSH, error)) {
      return FALSE;
    }
    return TRUE;
  } else if (len > 0) {
    if ((guint32)(t->uwbuf_size - t->uwpos) < len) {
      if (!thrift_zlib_transport_flush_to_zlib (transport, (gint8*)t->uwbuf, t->uwpos, Z_NO_FLUSH, error)) {
        return FALSE;
      }
      t->uwpos = 0;
    }
    memcpy (t->uwbuf + t->uwpos, buf, len);
    t->uwpos += len;
    return TRUE;
  }
  return FALSE;
}

gboolean
thrift_zlib_transport_flush_to_transport (ThriftTransport *transport, gint flush, GError **error)
{
  ThriftZlibTransport *t = THRIFT_ZLIB_TRANSPORT (transport);

  /* write pending data in uwbuf to zlib */
  if (!thrift_zlib_transport_flush_to_zlib (transport, (gint8*)t->uwbuf, t->uwpos, flush, error)) {
    return FALSE;
  }
  t->uwpos = 0;

  /* write all available data from zlib to the transport */
  if (!THRIFT_TRANSPORT_GET_CLASS (t->transport)->write (t->transport,
                                                    t->cwbuf, (t->cwbuf_size - t->wstream->avail_out),
                                                    error)) {
    return FALSE;
  }

  t->wstream->next_out = t->cwbuf;
  t->wstream->avail_out = t->cwbuf_size;

  /* flush the transport */
  if (!THRIFT_TRANSPORT_GET_CLASS (t->transport)->flush(t->transport, error)) {
    return FALSE;
  }
  return TRUE;
}

/* implements thrift_transport_write_end
 * called when write is complete.  nothing to do on our end. */
gboolean
thrift_zlib_transport_write_end (ThriftTransport *transport, GError **error)
{
  THRIFT_UNUSED_VAR (error);
  THRIFT_UNUSED_VAR (transport);

  return TRUE;
}

/* implements thrift_transport_flush */
gboolean
thrift_zlib_transport_flush (ThriftTransport *transport, GError **error)
{
  ThriftZlibTransport *t = THRIFT_ZLIB_TRANSPORT (transport);
  ThriftTransportClass *ttc = THRIFT_TRANSPORT_GET_CLASS (transport);

  if (t->output_finished) {
    return FALSE;
  }

  thrift_zlib_transport_flush_to_zlib (transport, (gint8*)t->uwbuf, t->uwpos, Z_NO_FLUSH, error);
  t->uwpos = 0;

  if (t->wstream->avail_out < 6) {
    if (!THRIFT_TRANSPORT_GET_CLASS (t->transport)->write(t->transport,
                                                     t->cwbuf, t->cwbuf_size - t->wstream->avail_out,
                                                     error)) {
      return FALSE;
    }
    t->wstream->next_out = t->cwbuf;
    t->wstream->avail_out = t->cwbuf_size;
  }

   if (!thrift_zlib_transport_flush_to_transport (transport, Z_FULL_FLUSH, error)) {
    return FALSE;
  }

  if (!ttc->resetConsumedMessageSize (transport, -1, error)) {
    return FALSE;
  }
  return TRUE; 
}

gboolean
thrift_zlib_transport_verify_checksum(ThriftTransport *transport, GError **error)
{
  ThriftZlibTransport *t = THRIFT_ZLIB_TRANSPORT (transport);
  
  /* If zlib has already reported the end of the stream,
   * it has verified the checksum. */
  if (t->input_ended) {
    return TRUE;
  }

  /* This should only be called when reading is complete.
   * If the caller still has unread data, throw an exception. */
  if (thrift_zlib_transport_read_avail (transport) > 0) {
    g_set_error (error, THRIFT_TRANSPORT_ERROR, THRIFT_TRANSPORT_ERROR_RECEIVE,
                 "thrift_zlib_transport_verify_checksum() called bufore end of zlib stream.");
    return FALSE;
  }

  /* Reset the rsteam fields, in case avail_out is 0.
   * (Since thrift_zlib_transport_read_avail() is 0, we know there is no unread data in urbuf) */
  t->rstream->next_out = t->urbuf;
  t->rstream->avail_out = t->urbuf_size;
  t->urpos = 0;

  /* Call inflate()
   * This will set the error if the checksum is bad. */
  gboolean performed_inflate = thrift_zlib_transport_read_from_zlib (transport, error);
  if (!performed_inflate) {
    g_set_error (error, THRIFT_TRANSPORT_ERROR, THRIFT_TRANSPORT_ERROR_RECEIVE,
                 "checksum not available yet in thrift_zlib_transport_verify_checksum ()");
    return FALSE;
  }

  /* If input_ended is TRUE now, the checksum has been verified */
  if (t->input_ended) {
    return TRUE;
  }

  /* The caller invoked us before the actual end of the data stream */
  if (t->rstream->avail_out < (guint)t->urbuf_size) {
    g_set_error (error, THRIFT_TRANSPORT_ERROR, THRIFT_TRANSPORT_ERROR_RECEIVE,
                 "rstream->avail_out >= urbuf_size");
    return FALSE;
  }

  g_set_error (error, THRIFT_TRANSPORT_ERROR, THRIFT_TRANSPORT_ERROR_RECEIVE,
               "thrift_zlib_transport_verify_checksum() called bufore end of zlib stream.");
  return FALSE;
}

gboolean
thrift_zlib_transport_finish(ThriftTransport *transport, GError **error)
{
  ThriftZlibTransport *t = THRIFT_ZLIB_TRANSPORT (transport);
  
  if (t->output_finished) {
    g_set_error (error, THRIFT_TRANSPORT_ERROR, THRIFT_TRANSPORT_ERROR_SEND,
		 "finish() called more than once");
    return FALSE;
  }

   if (!thrift_zlib_transport_flush_to_transport (transport, Z_FINISH, error)) {
    return FALSE;
  }

  return TRUE;
}

/* initializes the instance */
static void
thrift_zlib_transport_init (ThriftZlibTransport *transport)
{
  transport->transport = NULL;
  transport->urpos = 0;
  transport->uwpos = 0;
  transport->input_ended = FALSE;
  transport->output_finished = FALSE;

  transport->rstream = g_new0 (struct z_stream_s, 1);
  transport->wstream = g_new0 (struct z_stream_s, 1);

  transport->rstream->zalloc = Z_NULL;
  transport->wstream->zalloc = Z_NULL;
  transport->rstream->zfree = Z_NULL;
  transport->wstream->zfree = Z_NULL;
  transport->rstream->opaque = Z_NULL;
  transport->wstream->opaque = Z_NULL;

  transport->rstream->avail_in = 0;
  transport->wstream->avail_in = 0;
}

/* destructor */
static void
thrift_zlib_transport_finalize (GObject *object)
{
  ThriftZlibTransport *t = THRIFT_ZLIB_TRANSPORT (object);
  inflateEnd (t->rstream);
  deflateEnd (t->wstream);

  if (t->urbuf != NULL) {
    g_free (t->urbuf);
  }
  if (t->crbuf != NULL) {
    g_free (t->crbuf);
  }
  if (t->uwbuf != NULL) {
    g_free (t->uwbuf);
  }
  if (t->cwbuf != NULL) {
    g_free (t->cwbuf);
  }
  if (t->rstream != NULL) {
    g_free (t->rstream);
  }
  if (t->wstream != NULL) {
    g_free (t->wstream);
  }
}

/* property accessor */
void
thrift_zlib_transport_get_property (GObject *object, guint property_id,
                                    GValue *value, GParamSpec *pspec)
{
  ThriftZlibTransport *transport = NULL;
  ThriftTransport *tt = NULL;

  THRIFT_UNUSED_VAR (pspec);

  transport = THRIFT_ZLIB_TRANSPORT (object);
  tt = THRIFT_TRANSPORT (object);

  switch (property_id) {
    case PROP_THRIFT_ZLIB_TRANSPORT_TRANSPORT:
      g_value_set_object (value, transport->transport);
      break;
    case PROP_THRIFT_ZLIB_TRANSPORT_URBUF_SIZE:
      g_value_set_int (value, transport->urbuf_size);
      break;
    case PROP_THRIFT_ZLIB_TRANSPORT_CRBUF_SIZE:
      g_value_set_int (value, transport->crbuf_size);
      break;
    case PROP_THRIFT_ZLIB_TRANSPORT_UWBUF_SIZE:
      g_value_set_int (value, transport->uwbuf_size);
      break;
    case PROP_THRIFT_ZLIB_TRANSPORT_CWBUF_SIZE:
      g_value_set_int (value, transport->cwbuf_size);
      break;
    case PROP_THRIFT_ZLIB_TRANSPORT_COMP_LEVEL:
      g_value_set_int (value, transport->comp_level);
      break;
    case PROP_THRIFT_ZLIB_TRANSPORT_CONFIGURATION:
      g_value_set_object (value, tt->configuration);
      break;
    case PROP_THRIFT_ZLIB_TRANSPORT_REMAINING_MESSAGE_SIZE:
      g_value_set_long (value, tt->remainingMessageSize_);
      break;
    case PROP_THRIFT_ZLIB_TRANSPORT_KNOW_MESSAGE_SIZE:
      g_value_set_long (value, tt->knowMessageSize_);
      break;
    default:
      break;
  }
}

/* property mutator */
void
thrift_zlib_transport_set_property (GObject *object, guint property_id,
                                    const GValue *value, GParamSpec *pspec)
{
  ThriftZlibTransport *transport = NULL;
  ThriftTransport *tt = NULL;

  THRIFT_UNUSED_VAR (pspec);

  transport = THRIFT_ZLIB_TRANSPORT (object);
  tt = THRIFT_TRANSPORT (object);

  switch (property_id) {
    case PROP_THRIFT_ZLIB_TRANSPORT_TRANSPORT:
      transport->transport = g_value_get_object (value);
      break;
    case PROP_THRIFT_ZLIB_TRANSPORT_URBUF_SIZE:
      transport->urbuf_size = g_value_get_int (value);
      transport->urbuf = g_new0 (guint8, transport->urbuf_size);
      transport->rstream->next_out = transport->urbuf;
      transport->rstream->avail_out = transport->urbuf_size;
      break;
    case PROP_THRIFT_ZLIB_TRANSPORT_CRBUF_SIZE:
      transport->crbuf_size = g_value_get_int (value);
      transport->crbuf = g_new0 (guint8, transport->crbuf_size);
      transport->rstream->next_in = transport->crbuf;
      break;
    case PROP_THRIFT_ZLIB_TRANSPORT_UWBUF_SIZE:
      transport->uwbuf_size = g_value_get_int (value);
      transport->uwbuf = g_new0 (guint8, transport->uwbuf_size);
      transport->wstream->next_in = transport->uwbuf;
      break;
    case PROP_THRIFT_ZLIB_TRANSPORT_CWBUF_SIZE:
      transport->cwbuf_size = g_value_get_int (value);
      transport->cwbuf = g_new0 (guint8, transport->cwbuf_size);
      transport->wstream->next_out = transport->cwbuf;
      transport->wstream->avail_out = transport->cwbuf_size;
      break;
    case PROP_THRIFT_ZLIB_TRANSPORT_COMP_LEVEL:
      transport->comp_level = g_value_get_int (value);
      if(inflateInit(transport->rstream) != Z_OK) {
         printf("inflate_init fail \n");
         return;
      }
      if(deflateInit (transport->wstream, transport->comp_level) != Z_OK) {
	 printf("deflate init fail\n");
         return;
      }
      break;
    case PROP_THRIFT_ZLIB_TRANSPORT_CONFIGURATION:
      tt->configuration = g_value_dup_object (value);
      break;
    case PROP_THRIFT_ZLIB_TRANSPORT_REMAINING_MESSAGE_SIZE:
      tt->remainingMessageSize_ = g_value_get_long (value);
      break;
    case PROP_THRIFT_ZLIB_TRANSPORT_KNOW_MESSAGE_SIZE:
      tt->knowMessageSize_ = g_value_get_long (value);
      break;
    default:
      break;
  }
}

/* initialize the class */
static void
thrift_zlib_transport_class_init (ThriftZlibTransportClass *cls)
{
  ThriftTransportClass *ttc;
  GObjectClass *gobject_class;
  GParamSpec *param_spec;

  ttc = THRIFT_TRANSPORT_CLASS (cls);
  gobject_class = G_OBJECT_CLASS (cls);
  param_spec = NULL;

  /* setup accessors and mutators */
  gobject_class->get_property = thrift_zlib_transport_get_property;
  gobject_class->set_property = thrift_zlib_transport_set_property;
  
  param_spec = g_param_spec_object ("transport", "transport (construct)",
                                    "Thrift transport",
                                    THRIFT_TYPE_TRANSPORT,
                                    G_PARAM_CONSTRUCT_ONLY |
                                    G_PARAM_READWRITE);
  g_object_class_install_property (gobject_class,
                                   PROP_THRIFT_ZLIB_TRANSPORT_TRANSPORT,
                                   param_spec);

  param_spec = g_param_spec_int ("urbuf_size", "urbuf_size (construct)",
                                 "Uncompressed buffer size for reading",
                                 0, /* min */
                                 G_MAXINT, /* max */
                                 DEFAULT_URBUF_SIZE, /* default value */
                                 G_PARAM_CONSTRUCT_ONLY |
                                 G_PARAM_READWRITE);
  g_object_class_install_property (gobject_class,
                                   PROP_THRIFT_ZLIB_TRANSPORT_URBUF_SIZE,
                                   param_spec);

  param_spec = g_param_spec_int ("crbuf_size", "crbuf_size (construct)",
                                 "Compressed buffer size for reading",
                                 0, /* min */
                                 G_MAXINT, /* max */
                                 DEFAULT_CRBUF_SIZE, /* default value */
                                 G_PARAM_CONSTRUCT_ONLY |
                                 G_PARAM_READWRITE);
  g_object_class_install_property (gobject_class,
                                   PROP_THRIFT_ZLIB_TRANSPORT_CRBUF_SIZE,
                                   param_spec);

  param_spec = g_param_spec_int ("uwbuf_size", "uwbuf_size (construct)",
                                 "Uncompressed buffer size for writing",
                                 MIN_DIRECT_DEFLATE_SIZE, /* min */
                                 G_MAXINT, /* max */
                                 DEFAULT_UWBUF_SIZE, /* default value */
                                 G_PARAM_CONSTRUCT_ONLY |
                                 G_PARAM_READWRITE);
  g_object_class_install_property (gobject_class,
                                   PROP_THRIFT_ZLIB_TRANSPORT_UWBUF_SIZE,
                                   param_spec);

  param_spec = g_param_spec_int ("cwbuf_size", "cwbuf_size (construct)",
                                 "Compressed buffer size of writing",
                                 0,  /* min */
                                 G_MAXINT,  /* max */
                                 DEFAULT_CWBUF_SIZE, /* default value */
                                 G_PARAM_CONSTRUCT_ONLY |
                                 G_PARAM_READWRITE);
  g_object_class_install_property (gobject_class,
                                   PROP_THRIFT_ZLIB_TRANSPORT_CWBUF_SIZE,
                                   param_spec);

  param_spec = g_param_spec_int ("comp_level", "comp_level (construct)",
                                 "Compression level (0=none[fast], 6=default, 9=max[slow])",
                                 Z_DEFAULT_COMPRESSION, /* min */
                                 Z_BEST_COMPRESSION, /* max */
                                 Z_DEFAULT_COMPRESSION, /* default value */
                                 G_PARAM_CONSTRUCT_ONLY |
                                 G_PARAM_READWRITE);
  g_object_class_install_property (gobject_class,
                                   PROP_THRIFT_ZLIB_TRANSPORT_COMP_LEVEL,
                                   param_spec);

  param_spec = g_param_spec_object ("configuration", "configuration (construct)",
                                    "Thrift Configuration",
                                    THRIFT_TYPE_CONFIGURATION,
                                    G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
  g_object_class_install_property (gobject_class,
                                   PROP_THRIFT_ZLIB_TRANSPORT_CONFIGURATION,
                                   param_spec);

  param_spec = g_param_spec_long ("remainingmessagesize", "remainingmessagesize (construct)",
                                  "Set the size of the remaining message",
                                  0, /* min */
                                  G_MAXINT32, /* max */
                                  DEFAULT_MAX_MESSAGE_SIZE, /* default by construct */
                                  G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
  g_object_class_install_property (gobject_class,
                                   PROP_THRIFT_ZLIB_TRANSPORT_REMAINING_MESSAGE_SIZE,
                                   param_spec);

  param_spec = g_param_spec_long ("knowmessagesize", "knowmessagesize (construct)",
                                  "Set the size of the know message",
                                  G_MININT, /* min */
                                  G_MAXINT32, /* max */
                                  DEFAULT_MAX_MESSAGE_SIZE, /* default by construct */
                                  G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
  g_object_class_install_property (gobject_class,
                                   PROP_THRIFT_ZLIB_TRANSPORT_KNOW_MESSAGE_SIZE,
                                   param_spec);

  gobject_class->finalize = thrift_zlib_transport_finalize;
  ttc->is_open = thrift_zlib_transport_is_open;
  ttc->peek = thrift_zlib_transport_peek;
  ttc->open = thrift_zlib_transport_open;
  ttc->close = thrift_zlib_transport_close;
  ttc->read = thrift_zlib_transport_read;
  ttc->read_end = thrift_zlib_transport_read_end;
  ttc->write = thrift_zlib_transport_write;
  ttc->write_end = thrift_zlib_transport_write_end;
  ttc->flush = thrift_zlib_transport_flush;
}
