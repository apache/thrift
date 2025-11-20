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

#include <thrift/c_glib/thrift.h>
#include <thrift/c_glib/transport/thrift_transport.h>
#include <thrift/c_glib/transport/thrift_server_transport.h>

/* object properties */
enum _ThriftServerTransportProperties
{
    PROP_0,
    PROP_THRIFT_SERVER_TRANSPORT_CONFIGURATION,
    PROP_THRIFT_SERVER_TRANSPORT_REMAINING_MESSAGE_SIZE,
    PROP_THRIFT_SERVER_TRANSPORT_KNOW_MESSAGE_SIZE
};

G_DEFINE_ABSTRACT_TYPE(ThriftServerTransport, thrift_server_transport, G_TYPE_OBJECT)

gboolean
thrift_server_transport_updateKnownMessageSize(ThriftServerTransport *transport, glong size, GError **error)
{
  gboolean boolean = TRUE;
  ThriftServerTransport *tst = THRIFT_SERVER_TRANSPORT (transport);
  ThriftServerTransportClass *tstc = THRIFT_SERVER_TRANSPORT_GET_CLASS (transport);
  glong consumed = tst->knowMessageSize_ - tst->remainingMessageSize_;
  if(!tstc->resetConsumedMessageSize (transport, size, error))
  {
    boolean = FALSE;
  }
  if(!tstc->countConsumedMessageBytes (transport, consumed, error))
  {
    boolean = FALSE;
  }
  return boolean;
}

gboolean
thrift_server_transport_checkReadBytesAvailable(ThriftServerTransport *transport, glong numBytes, GError **error)
{
  gboolean boolean = TRUE;
  ThriftServerTransport *tst = THRIFT_SERVER_TRANSPORT (transport);
  if(tst->remainingMessageSize_ < numBytes)
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
thrift_server_transport_resetConsumedMessageSize(ThriftServerTransport *transport, glong newSize, GError **error)
{
  ThriftServerTransport *tst = THRIFT_SERVER_TRANSPORT (transport);
  if(newSize < 0)
  {
    if(tst->configuration != NULL)
    {
      tst->knowMessageSize_ = tst->configuration->maxMessageSize_;
      tst->remainingMessageSize_ = tst->configuration->maxMessageSize_;
    }
    else
    {
      tst->knowMessageSize_ = DEFAULT_MAX_MESSAGE_SIZE;
      tst->remainingMessageSize_ = DEFAULT_MAX_MESSAGE_SIZE;
    }
    return TRUE;
  }
  /* update only: message size can shrink, but not grow */
  if(newSize > tst->knowMessageSize_)
  {
    g_set_error(error,
                THRIFT_TRANSPORT_ERROR,
                THRIFT_TRANSPORT_ERROR_MAX_MESSAGE_SIZE_REACHED,
                "MaxMessageSize reached");
    return FALSE;
  }
 
  tst->knowMessageSize_ = newSize;
  tst->remainingMessageSize_ = newSize;

  return TRUE;  
}

gboolean
thrift_server_transport_countConsumedMessageBytes(ThriftServerTransport *transport, glong numBytes, GError **error)
{
    ThriftServerTransport *tst = THRIFT_SERVER_TRANSPORT (transport);
    if(tst->remainingMessageSize_ > numBytes)
    {
        tst->remainingMessageSize_ -= numBytes;
    }
    else
    {
        tst->remainingMessageSize_ = 0;
        if(*error == NULL)
	{
	    g_set_error(error,
		        THRIFT_TRANSPORT_ERROR,
 		        THRIFT_TRANSPORT_ERROR_MAX_MESSAGE_SIZE_REACHED,
		        "MaxMessageSize reached");
	}
	return FALSE;
    }

    return TRUE;
}

/* property accesor */
void
thrift_server_transport_get_property(GObject *object, guint property_id,
		                     GValue *value, GParamSpec *pspec)
{
    ThriftServerTransport *transport = THRIFT_SERVER_TRANSPORT (object);

    THRIFT_UNUSED_VAR (pspec);

    switch (property_id)
    {
        case PROP_THRIFT_SERVER_TRANSPORT_CONFIGURATION:
          g_value_set_object (value, transport->configuration);
	  break;
	case PROP_THRIFT_SERVER_TRANSPORT_REMAINING_MESSAGE_SIZE:
	  g_value_set_long (value, transport->remainingMessageSize_);
	  break;
	case PROP_THRIFT_SERVER_TRANSPORT_KNOW_MESSAGE_SIZE:
          g_value_set_long (value, transport->knowMessageSize_);
	  break; 
    }
}

/* property mutator */
void
thrift_server_transport_set_property (GObject *object, guint property_id,
		                      const GValue *value, GParamSpec *pspec)
{
    ThriftServerTransport *transport = THRIFT_SERVER_TRANSPORT (object);

    THRIFT_UNUSED_VAR (pspec);

    switch (property_id)
    {
      case PROP_THRIFT_SERVER_TRANSPORT_CONFIGURATION:
           transport->configuration = g_value_dup_object (value);
           break;
      case PROP_THRIFT_SERVER_TRANSPORT_REMAINING_MESSAGE_SIZE:
           transport->remainingMessageSize_ = g_value_get_long (value);
           break;
      case PROP_THRIFT_SERVER_TRANSPORT_KNOW_MESSAGE_SIZE:
           transport->knowMessageSize_ = g_value_get_long (value);
           break;
    }
}

/* base initializer for the server transport interface */
static void
thrift_server_transport_class_init (ThriftServerTransportClass *c)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (c);
  ThriftServerTransportClass *tstc = THRIFT_SERVER_TRANSPORT_CLASS (c);
  GParamSpec *param_spec = NULL;
  
  /* setup accessors and mutators */
  gobject_class->get_property = thrift_server_transport_get_property;
  gobject_class->set_property = thrift_server_transport_set_property;
  
  param_spec = g_param_spec_object ("configuration",
                                    "configuration (construct)",
                                    "Thrift Configuration",
                                    THRIFT_TYPE_CONFIGURATION,
                                    G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
  g_object_class_install_property (gobject_class,
                                   PROP_THRIFT_SERVER_TRANSPORT_CONFIGURATION,
                                   param_spec);

  param_spec = g_param_spec_long ("remainingmessagesize",
                                  "remainingmessagesize (construct)",
                                  "Set the remaining message size",
                                  0, /* min */
                                  G_MAXINT32, /* max */
                                  DEFAULT_MAX_MESSAGE_SIZE, /* default by construct */
                                  G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
  g_object_class_install_property (gobject_class,
                                   PROP_THRIFT_SERVER_TRANSPORT_REMAINING_MESSAGE_SIZE,
                                   param_spec);

  param_spec = g_param_spec_long ("knowmessagesize",
                                  "knowmessagesize (construct)",
                                  "Set the known size of the message",
                                  0, /* min */
                                  G_MAXINT32, /* max */
                                  DEFAULT_MAX_MESSAGE_SIZE, /* default by construct */
                                  G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
  g_object_class_install_property (gobject_class,
                                   PROP_THRIFT_SERVER_TRANSPORT_KNOW_MESSAGE_SIZE,
                                   param_spec);

  c->listen = thrift_server_transport_listen;
  c->accept = thrift_server_transport_accept;
  c->close = thrift_server_transport_close;
  tstc->updateKnownMessageSize = thrift_server_transport_updateKnownMessageSize;
  tstc->checkReadBytesAvailable = thrift_server_transport_checkReadBytesAvailable;
  tstc->resetConsumedMessageSize = thrift_server_transport_resetConsumedMessageSize;
  tstc->countConsumedMessageBytes = thrift_server_transport_countConsumedMessageBytes;
}

static void
thrift_server_transport_init (ThriftServerTransport *transport)
{
  THRIFT_UNUSED_VAR (transport);
}

gboolean
thrift_server_transport_listen (ThriftServerTransport *transport,
                                GError **error)
{
  return THRIFT_SERVER_TRANSPORT_GET_CLASS (transport)->listen (transport,
                                                                error);
}

ThriftTransport *
thrift_server_transport_accept (ThriftServerTransport *transport,
                                GError **error)
{
  return THRIFT_SERVER_TRANSPORT_GET_CLASS (transport)->accept (transport,
                                                                error);
}

gboolean
thrift_server_transport_close (ThriftServerTransport *transport, GError **error)
{
  return THRIFT_SERVER_TRANSPORT_GET_CLASS (transport)->close (transport,
                                                               error);
}
