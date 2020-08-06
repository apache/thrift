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
#include <unistd.h>
#include <fcntl.h>
#include <string.h>

#include <glib.h>
#include <glib/gstdio.h>

#include <thrift/c_glib/thrift.h>
#include <thrift/c_glib/thrift_configuration.h>
#include <thrift/c_glib/transport/thrift_transport.h>
#include <thrift/c_glib/transport/thrift_fd_transport.h>

/* object properties */
enum _ThriftFDTransportProperties
{
  PROP_0,
  PROP_THRIFT_FD_TRANSPORT_FD,
  PROP_THRIFT_FD_TRANSPORT_CONFIGURATION,
  PROP_THRIFT_FD_TRANSPORT_REMAINING_MESSAGE_SIZE,
  PROP_THRIFT_FD_TRANSPORT_KNOW_MESSAGE_SIZE,
};

G_DEFINE_TYPE (ThriftFDTransport, thrift_fd_transport, THRIFT_TYPE_TRANSPORT)

/* implements thrift_transport_is_open */
gboolean
thrift_fd_transport_is_open (ThriftTransport *transport)
{
  ThriftFDTransport *t;
  t = THRIFT_FD_TRANSPORT (transport);
  return t->fd >= 0 && ! (fcntl (t->fd, F_GETFL) == -1 && errno == EBADF);
}

/* implements thrift_transport_open */
gboolean
thrift_fd_transport_open (ThriftTransport *transport, GError **error)
{
  THRIFT_UNUSED_VAR (error);
  return thrift_fd_transport_is_open (transport);
}

/* implements thrift_transport_close */
gboolean
thrift_fd_transport_close (ThriftTransport *transport, GError **error)
{
  ThriftFDTransport *t;
  t = THRIFT_FD_TRANSPORT (transport);

#if GLIB_CHECK_VERSION (2, 36, 0)
  return g_close (t->fd, error);
#else
  if (close (t->fd) == 0) {
    g_clear_error (error);
    return TRUE;
  } else {
    g_set_error (error,
                 THRIFT_TRANSPORT_ERROR,
                 THRIFT_TRANSPORT_ERROR_CLOSE,
                 strerror (errno));
    return FALSE;
  }
#endif
}

/* implements thrift_transport_read */
gint32
thrift_fd_transport_read (ThriftTransport *transport, gpointer buf,
                          guint32 len, GError **error)
{
  ThriftFDTransport *t;
  ssize_t n;

  t = THRIFT_FD_TRANSPORT (transport);
  ThriftTransportClass *ttc = THRIFT_TRANSPORT_GET_CLASS (transport);
  if(!ttc->checkReadBytesAvailable(transport, len, error))
  {
    return -1;
  }

  n = read (t->fd, (guint8 *) buf, len);
  if (n == -1) {
    g_set_error (error,
                 THRIFT_TRANSPORT_ERROR,
                 THRIFT_TRANSPORT_ERROR_RECEIVE,
                 "Failed to read from fd: %s",
                 strerror (errno));
    return -1;
  }
  return n;
}

/* implements thrift_transport_read_end
 * called when write is complete.  nothing to do on our end. */
gboolean
thrift_fd_transport_read_end (ThriftTransport *transport, GError **error)
{
  /* satisfy -Wall */
  THRIFT_UNUSED_VAR (transport);
  THRIFT_UNUSED_VAR (error);
  return TRUE;
}

/* implements thrift_transport_write */
gboolean
thrift_fd_transport_write (ThriftTransport *transport,
                           const gpointer buf,
                           const guint32 len, GError **error)
{
  ThriftFDTransport *t;
  guint8 *_buf;
  guint32 _len;
  ssize_t n;

  t = THRIFT_FD_TRANSPORT (transport);
  _buf = (guint8 *) buf;
  _len = len;
  while (_len > 0) {
    n = write (t->fd, _buf, _len);
    if (n == -1) {
      g_set_error (error,
                   THRIFT_TRANSPORT_ERROR,
                   THRIFT_TRANSPORT_ERROR_SEND,
                   "Failed to write from fd: %s",
                   strerror (errno));
      return FALSE;
    } else {
      _buf += n;
      _len -= n;
    }
  }
  return TRUE;
}

/* implements thrift_transport_write_end
 * called when write is complete.  nothing to do on our end. */
gboolean
thrift_fd_transport_write_end (ThriftTransport *transport, GError **error)
{
  THRIFT_UNUSED_VAR (transport);
  THRIFT_UNUSED_VAR (error);
  return TRUE;
}

/* implements thrift_transport_flush */
gboolean
thrift_fd_transport_flush (ThriftTransport *transport, GError **error)
{
  ThriftFDTransport *t;
  t = THRIFT_FD_TRANSPORT (transport);
  ThriftTransportClass *ttc = THRIFT_TRANSPORT_GET_CLASS (transport);
  if(!ttc->resetConsumedMessageSize (transport, -1, error))
  {
    return FALSE;
  }

  if (fsync (t->fd) == -1) {
    g_set_error (error,
                 THRIFT_TRANSPORT_ERROR,
                 THRIFT_TRANSPORT_ERROR_UNKNOWN,
                 "Failed to flush fd: %s",
                 strerror (errno));
    return FALSE;
  } else {
    return TRUE;
  }
}

/* initializes the instance */
static void
thrift_fd_transport_init (ThriftFDTransport *transport)
{
  transport->fd = -1;
}

/* destructor */
static void
thrift_fd_transport_finalize (GObject *object)
{
  THRIFT_UNUSED_VAR (object);
}

/* property accessor */
void
thrift_fd_transport_get_property (GObject *object, guint property_id,
                                  GValue *value, GParamSpec *pspec)
{
  ThriftFDTransport *t;

  THRIFT_UNUSED_VAR (pspec);

  t = THRIFT_FD_TRANSPORT (object);

  ThriftTransport *tt = THRIFT_TRANSPORT (object);

  switch (property_id) {
    case PROP_THRIFT_FD_TRANSPORT_FD:
      g_value_set_int (value, t->fd);
      break;
    case PROP_THRIFT_FD_TRANSPORT_CONFIGURATION:
      g_value_set_object (value, tt->configuration);
      break;
    case PROP_THRIFT_FD_TRANSPORT_REMAINING_MESSAGE_SIZE:
      g_value_set_long (value, tt->remainingMessageSize_);
      break;
    case PROP_THRIFT_FD_TRANSPORT_KNOW_MESSAGE_SIZE:
      g_value_set_long (value, tt->knowMessageSize_);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
  }
}

/* property mutator */
void
thrift_fd_transport_set_property (GObject *object, guint property_id,
                                  const GValue *value, GParamSpec *pspec)
{
  ThriftFDTransport *t;

  THRIFT_UNUSED_VAR (pspec);

  t = THRIFT_FD_TRANSPORT (object);

  ThriftTransport *tt = THRIFT_TRANSPORT (object);

  switch (property_id) {
    case PROP_THRIFT_FD_TRANSPORT_FD:
      t->fd = g_value_get_int (value);
      break;
    case PROP_THRIFT_FD_TRANSPORT_CONFIGURATION:
      tt->configuration = g_value_dup_object (value);
      break;
    case PROP_THRIFT_FD_TRANSPORT_REMAINING_MESSAGE_SIZE:
      tt->remainingMessageSize_ = g_value_get_long (value);
      break;
    case PROP_THRIFT_FD_TRANSPORT_KNOW_MESSAGE_SIZE:
      tt->knowMessageSize_ = g_value_get_long (value);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
  }
}

/* initializes the class */
static void
thrift_fd_transport_class_init (ThriftFDTransportClass *cls)
{
  ThriftTransportClass *ttc;
  GObjectClass *gobject_class;
  GParamSpec *param_spec;

  ttc = THRIFT_TRANSPORT_CLASS (cls);
  gobject_class = G_OBJECT_CLASS (cls);
  param_spec = NULL;

  /* setup accessors and mutators */
  gobject_class->get_property = thrift_fd_transport_get_property;
  gobject_class->set_property = thrift_fd_transport_set_property;

  param_spec = g_param_spec_int ("fd",
                                 "file descriptor (construct)",
                                 "Set the file descriptor",
                                 INT_MIN, /* min */
                                 INT_MAX, /* max, 1024*1024 */
                                 -1, /* default value */
                                 G_PARAM_CONSTRUCT_ONLY |
                                 G_PARAM_READWRITE);
  g_object_class_install_property (gobject_class,
                                   PROP_THRIFT_FD_TRANSPORT_FD,
                                   param_spec);

  param_spec = g_param_spec_object ("configuration",
                                    "configuration (construct)",
                                    "thrift configuration",
                                    THRIFT_TYPE_CONFIGURATION,
                                    G_PARAM_CONSTRUCT_ONLY |
                                    G_PARAM_READWRITE);
  g_object_class_install_property (gobject_class,
                                   PROP_THRIFT_FD_TRANSPORT_CONFIGURATION,
                                   param_spec);

  param_spec = g_param_spec_long ("remainingmessagesize",
                                  "remainingmessagesize (construct)",
                                  "Set the size of the remaining message",
                                  0, /* min */
                                  G_MAXINT32, /* max */
                                  DEFAULT_MAX_MESSAGE_SIZE, /* default by construct */
                                  G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
  g_object_class_install_property (gobject_class,
                                   PROP_THRIFT_FD_TRANSPORT_REMAINING_MESSAGE_SIZE,
                                   param_spec);

  param_spec = g_param_spec_long ("knowmessagesize",
                                  "knowmessagesize (construct)",
                                  "Set the size of the know message",
                                  0, /* min */
                                  G_MAXINT32, /* max */
                                  DEFAULT_MAX_MESSAGE_SIZE, /* default by construct */
                                  G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
  g_object_class_install_property (gobject_class,
                                   PROP_THRIFT_FD_TRANSPORT_KNOW_MESSAGE_SIZE,
                                   param_spec);

  gobject_class->finalize = thrift_fd_transport_finalize;
  ttc->is_open = thrift_fd_transport_is_open;
  ttc->open = thrift_fd_transport_open;
  ttc->close = thrift_fd_transport_close;
  ttc->read = thrift_fd_transport_read;
  ttc->read_end = thrift_fd_transport_read_end;
  ttc->write = thrift_fd_transport_write;
  ttc->write_end = thrift_fd_transport_write_end;
  ttc->flush = thrift_fd_transport_flush;
}
