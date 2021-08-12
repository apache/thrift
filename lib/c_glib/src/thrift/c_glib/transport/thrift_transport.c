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
#include <glib.h>
#include <thrift/c_glib/thrift.h>
#include <thrift/c_glib/transport/thrift_transport.h>
#include <thrift/c_glib/thrift_configuration.h>

/* object properties */
enum _ThriftTransportProperties
{
  PROP_0,
  PROP_THRIFT_TRANSPORT_CONFIGURATION,
  PROP_THRIFT_TRANSPORT_REMAINING_MESSAGE_SIZE,
  PROP_THRIFT_TRANSPORT_KNOW_MESSAGE_SIZE
};

/* define the GError domain string */
#define THRIFT_TRANSPORT_ERROR_DOMAIN "thrift-transport-error-quark"

G_DEFINE_ABSTRACT_TYPE(ThriftTransport, thrift_transport, G_TYPE_OBJECT)

gboolean 
thrift_transport_is_open (ThriftTransport *transport)
{
  return THRIFT_TRANSPORT_GET_CLASS (transport)->is_open (transport);
}

gboolean
thrift_transport_peek (ThriftTransport *transport, GError **error)
{
  return THRIFT_TRANSPORT_GET_CLASS (transport)->peek (transport, error);
}

gboolean
thrift_transport_open (ThriftTransport *transport, GError **error)
{
  return THRIFT_TRANSPORT_GET_CLASS (transport)->open (transport, error);
}

gboolean
thrift_transport_close (ThriftTransport *transport, GError **error)
{
  return THRIFT_TRANSPORT_GET_CLASS (transport)->close (transport, error);
}

gint32
thrift_transport_read (ThriftTransport *transport, gpointer buf,
                       guint32 len, GError **error)
{
  return THRIFT_TRANSPORT_GET_CLASS (transport)->read (transport, buf,
                                                       len, error);
}

gboolean
thrift_transport_read_end (ThriftTransport *transport, GError **error)
{
  return THRIFT_TRANSPORT_GET_CLASS (transport)->read_end (transport,
                                                           error);
}

gboolean
thrift_transport_write (ThriftTransport *transport, const gpointer buf,
                        const guint32 len, GError **error)
{
  return THRIFT_TRANSPORT_GET_CLASS (transport)->write (transport, buf,
                                                        len, error);
}

gboolean
thrift_transport_write_end (ThriftTransport *transport, GError **error)
{
  return THRIFT_TRANSPORT_GET_CLASS (transport)->write_end (transport,
                                                            error);
}

gboolean
thrift_transport_flush (ThriftTransport *transport, GError **error)
{
  return THRIFT_TRANSPORT_GET_CLASS (transport)->flush (transport, error);
}

gint32
thrift_transport_read_all (ThriftTransport *transport, gpointer buf,
                           guint32 len, GError **error)
{
  return THRIFT_TRANSPORT_GET_CLASS (transport)->read_all (transport, buf,
                                                           len, error);
}

/* by default, peek returns true if and only if the transport is open */
static gboolean
thrift_transport_real_peek (ThriftTransport *transport, GError **error)
{
  THRIFT_UNUSED_VAR (error);

  return THRIFT_TRANSPORT_GET_CLASS (transport)->is_open (transport);
}

static gint32
thrift_transport_real_read_all (ThriftTransport *transport, gpointer buf,
                                guint32 len, GError **error)
{
  ThriftTransportClass *ttc;
  guint32 have;
  gint32 ret;
  gint8 *bytes;

  THRIFT_UNUSED_VAR (error);

  ttc = THRIFT_TRANSPORT_GET_CLASS (transport);
  have = 0;
  ret = 0;
  bytes = (gint8*) buf;

  while (have < len) {
    if ((ret = ttc->read (transport, (gpointer) (bytes + have), len - have,
                          error)) < 0) {
      return ret;
    }
    have += ret;
  }

  return have;
}

gboolean
thrift_transport_updateKnownMessageSize(ThriftTransport *transport, glong size, GError **error)
{
  gboolean boolean = TRUE;
  ThriftTransport *tt = THRIFT_TRANSPORT (transport);
  ThriftTransportClass *ttc = THRIFT_TRANSPORT_GET_CLASS (transport);
  glong consumed = tt->knowMessageSize_ - tt->remainingMessageSize_;
  if(!ttc->resetConsumedMessageSize (transport, size, error))
  {
    boolean = FALSE;
  }
  if(!ttc->countConsumedMessageBytes (transport, consumed, error))
  {
    boolean = FALSE;
  }
  return boolean;
}

gboolean
thrift_transport_checkReadBytesAvailable(ThriftTransport *transport, glong numBytes, GError **error)
{
  gboolean boolean = TRUE;
  ThriftTransport *tt = THRIFT_TRANSPORT (transport);
  if(tt->remainingMessageSize_ < numBytes)
  {
    g_set_error(error,
                THRIFT_TRANSPORT_ERROR,
 		THRIFT_TRANSPORT_ERROR_MAX_MESSAGE_SIZE_REACHED,
		"MaxMessageSize reached");
    boolean = FALSE;
  }

  return boolean;
}

gboolean
thrift_transport_resetConsumedMessageSize(ThriftTransport *transport, glong newSize, GError **error)
{
  ThriftTransport *tt = THRIFT_TRANSPORT (transport);
  if(newSize < 0)
  {
    if(tt->configuration != NULL)
    {
      tt->knowMessageSize_ = tt->configuration->maxMessageSize_;
      tt->remainingMessageSize_ = tt->configuration->maxMessageSize_;
    }
    else
    {
      tt->knowMessageSize_ = DEFAULT_MAX_MESSAGE_SIZE;
      tt->remainingMessageSize_ = DEFAULT_MAX_MESSAGE_SIZE;
    }
    return TRUE;
  }
  /* update only: message size can shrink, but not grow */
  if(newSize > tt->knowMessageSize_)
  {
    g_set_error(error,
                THRIFT_TRANSPORT_ERROR,
                THRIFT_TRANSPORT_ERROR_MAX_MESSAGE_SIZE_REACHED,
                "MaxMessageSize reached");
    return FALSE;
  }
 
  tt->knowMessageSize_ = newSize;
  tt->remainingMessageSize_ = newSize;

  return TRUE;  
}

gboolean
thrift_transport_countConsumedMessageBytes(ThriftTransport *transport, glong numBytes, GError **error)
{
  ThriftTransport *tt = THRIFT_TRANSPORT (transport);
  if(tt->remainingMessageSize_ > numBytes)
  {
    tt->remainingMessageSize_ -= numBytes;
  }
  else
  {
    tt->remainingMessageSize_ = 0;
    g_set_error(error,
                THRIFT_TRANSPORT_ERROR,
                THRIFT_TRANSPORT_ERROR_MAX_MESSAGE_SIZE_REACHED,
                "MaxMessageSize reached");
    return FALSE;
  }

  return TRUE;
}

/* property accesor */
void
thrift_transport_get_property(GObject *object, guint property_id,
		              GValue *value, GParamSpec *pspec)
{
  ThriftTransport *transport = THRIFT_TRANSPORT (object);

  THRIFT_UNUSED_VAR (pspec);

  switch (property_id)
  {
    case PROP_THRIFT_TRANSPORT_CONFIGURATION:
         g_value_set_object (value, transport->configuration);
         break;
    case PROP_THRIFT_TRANSPORT_REMAINING_MESSAGE_SIZE:
         g_value_set_long (value, transport->remainingMessageSize_);
         break;
    case PROP_THRIFT_TRANSPORT_KNOW_MESSAGE_SIZE:
         g_value_set_long (value, transport->knowMessageSize_);
         break; 
  }
}

/* property mutator */
void
thrift_transport_set_property (GObject *object, guint property_id,
		               const GValue *value, GParamSpec *pspec)
{
  ThriftTransport *transport = THRIFT_TRANSPORT (object);

  THRIFT_UNUSED_VAR (pspec);

  switch (property_id)
  {
    case PROP_THRIFT_TRANSPORT_CONFIGURATION:
         transport->configuration = g_value_get_object (value);
         break;
    case PROP_THRIFT_TRANSPORT_REMAINING_MESSAGE_SIZE:
         transport->remainingMessageSize_ = g_value_get_long (value);
         break;
    case PROP_THRIFT_TRANSPORT_KNOW_MESSAGE_SIZE:
         transport->knowMessageSize_ = g_value_get_long (value);
         break;
  }
}

/* define the GError domain for Thrift transports */
GQuark
thrift_transport_error_quark (void)
{
  return g_quark_from_static_string (THRIFT_TRANSPORT_ERROR_DOMAIN);
}

static void
thrift_transport_dispose (GObject *gobject)
{
  ThriftTransport *self = THRIFT_TRANSPORT (gobject);

  if(self->configuration != NULL)
    g_clear_object (&self->configuration);

  /* Always chain up to the parent class; there is no need to check if
   * the parent class implements the dispose() virtual function: it is
   * always guaranteed to do so
   */
  G_OBJECT_CLASS (thrift_transport_parent_class)->dispose (gobject);
}

/* class initializer for ThriftTransport */
static void
thrift_transport_class_init (ThriftTransportClass *cls)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (cls);
  GParamSpec *param_spec = NULL;
  
  /* setup accessors and mutators */
  gobject_class->get_property = thrift_transport_get_property;
  gobject_class->set_property = thrift_transport_set_property;
  gobject_class->dispose = thrift_transport_dispose;
  
  param_spec = g_param_spec_object ("configuration",
                                    "configuration (construct)",
                                    "Thrift Configuration",
                                    THRIFT_TYPE_CONFIGURATION,
                                    G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
  g_object_class_install_property (gobject_class,
                                   PROP_THRIFT_TRANSPORT_CONFIGURATION,
                                   param_spec);

  param_spec = g_param_spec_long ("remainingmessagesize",
                                  "remainingmessagesize (construct)",
                                  "Set the remaining message size",
                                  0, /* min */
                                  G_MAXINT32, /* max */
                                  DEFAULT_MAX_MESSAGE_SIZE, /* default by construct */
                                  G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
  g_object_class_install_property (gobject_class,
                                   PROP_THRIFT_TRANSPORT_REMAINING_MESSAGE_SIZE,
                                   param_spec);

  param_spec = g_param_spec_long ("knowmessagesize",
                                  "knowmessagesize (construct)",
                                  "Set the known size of the message",
                                  0, /* min */
                                  G_MAXINT32, /* max */
                                  DEFAULT_MAX_MESSAGE_SIZE, /* default by construct */
                                  G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
  g_object_class_install_property (gobject_class,
                                   PROP_THRIFT_TRANSPORT_KNOW_MESSAGE_SIZE,
                                   param_spec);


  /* set these as virtual methods to be implemented by a subclass */
  cls->is_open = thrift_transport_is_open;
  cls->open = thrift_transport_open;
  cls->close = thrift_transport_close;
  cls->read = thrift_transport_read;
  cls->read_end = thrift_transport_read_end;
  cls->write = thrift_transport_write;
  cls->write_end = thrift_transport_write_end;
  cls->flush = thrift_transport_flush;

  /* provide a default implementation for the peek and read_all methods */
  cls->peek = thrift_transport_real_peek;
  cls->read_all = thrift_transport_real_read_all;

  cls->updateKnownMessageSize = thrift_transport_updateKnownMessageSize;
  cls->checkReadBytesAvailable = thrift_transport_checkReadBytesAvailable;
  cls->resetConsumedMessageSize = thrift_transport_resetConsumedMessageSize;
  cls->countConsumedMessageBytes = thrift_transport_countConsumedMessageBytes;
}

static void
thrift_transport_init (ThriftTransport *transport)
{
  THRIFT_UNUSED_VAR (transport);
}
