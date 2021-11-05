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
#include <string.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/un.h>
#include <netinet/in.h>

#include <thrift/c_glib/thrift.h>
#include <thrift/c_glib/transport/thrift_socket.h>
#include <thrift/c_glib/transport/thrift_transport.h>
#include <thrift/c_glib/transport/thrift_server_transport.h>
#include <thrift/c_glib/transport/thrift_server_socket.h>

/* object properties */
enum _ThriftServerSocketProperties
{
  PROP_0,
  PROP_THRIFT_SERVER_SOCKET_PORT,
  PROP_THRIFT_SERVER_SOCKET_PATH,
  PROP_THRIFT_SERVER_SOCKET_BACKLOG,
  PROP_THRIFT_SERVER_SOCKET_CONFIGURATION,
  PROP_THRIFT_SERVER_SOCKET_REMAINING_MESSAGE_SIZE,
  PROP_THRIFT_SERVER_SOCKET_KNOW_MESSAGE_SIZE
};

/* define the GError domain string */
#define THRIFT_SERVER_SOCKET_ERROR_DOMAIN "thrift-server-socket-error-quark"

G_DEFINE_TYPE(ThriftServerSocket, thrift_server_socket, THRIFT_TYPE_SERVER_TRANSPORT)

gboolean
thrift_server_socket_listen (ThriftServerTransport *transport, GError **error)
{
  int enabled = 1; /* for setsockopt() */
  ThriftServerSocket *tsocket = THRIFT_SERVER_SOCKET (transport);

  const int socket_domain = tsocket->path ? PF_UNIX : AF_INET;

  /* create a socket */
  if ((tsocket->sd = socket (socket_domain, SOCK_STREAM, 0)) == -1)
  {
    g_set_error (error, THRIFT_SERVER_SOCKET_ERROR,
                 THRIFT_SERVER_SOCKET_ERROR_SOCKET,
                 "failed to create socket - %s", strerror (errno));
    return FALSE;
  }

  if (setsockopt(tsocket->sd, SOL_SOCKET, SO_REUSEADDR, &enabled,
                 sizeof(enabled)) == -1)
  {
    g_set_error (error, THRIFT_SERVER_SOCKET_ERROR,
                 THRIFT_SERVER_SOCKET_ERROR_SETSOCKOPT,
                 "unable to set SO_REUSEADDR - %s", strerror(errno));
    close (tsocket->sd);
    tsocket->sd = THRIFT_INVALID_SOCKET;
    return FALSE;
  }

  /* bind to the socket */
  if (tsocket->path)
  {
    /* create a socket structure */
    struct sockaddr_un pin;
    memset (&pin, 0, sizeof(pin));
    pin.sun_family = AF_UNIX;
    memcpy(pin.sun_path, tsocket->path, strlen(tsocket->path) + 1);

    if (bind(tsocket->sd, (struct sockaddr *) &pin, sizeof(pin)) == -1)
    {
      g_set_error (error, THRIFT_SERVER_SOCKET_ERROR,
                   THRIFT_SERVER_SOCKET_ERROR_BIND,
                   "failed to bind to path %s: - %s",
                   tsocket->path, strerror(errno));
      close (tsocket->sd);
      tsocket->sd = THRIFT_INVALID_SOCKET;
      return FALSE;
    }
  }
  else
  {
    /* create a address structure */
    struct sockaddr_in pin;
    memset (&pin, 0, sizeof(pin));
    pin.sin_family = AF_INET;
    pin.sin_addr.s_addr = INADDR_ANY;
    pin.sin_port = htons(tsocket->port);

    if (bind(tsocket->sd, (struct sockaddr *) &pin, sizeof(pin)) == -1)
    {
      g_set_error (error, THRIFT_SERVER_SOCKET_ERROR,
                   THRIFT_SERVER_SOCKET_ERROR_BIND,
                   "failed to bind to port %d - %s",
                   tsocket->port, strerror(errno));
      close (tsocket->sd);
      tsocket->sd = THRIFT_INVALID_SOCKET;
      return FALSE;
    }
  }

  if (listen(tsocket->sd, tsocket->backlog) == -1)
  {
    if (tsocket->path)
    {
      g_set_error (error, THRIFT_SERVER_SOCKET_ERROR,
                   THRIFT_SERVER_SOCKET_ERROR_BIND,
                   "failed to listen to path %s: - %s",
                   tsocket->path, strerror(errno));
    }
    else
    {
      g_set_error (error, THRIFT_SERVER_SOCKET_ERROR,
                   THRIFT_SERVER_SOCKET_ERROR_LISTEN,
                   "failed to listen to port %d - %s",
                   tsocket->port, strerror(errno));
    }
    close (tsocket->sd);
    tsocket->sd = THRIFT_INVALID_SOCKET;
    return FALSE;
  }

  return TRUE;
}

ThriftTransport *
thrift_server_socket_accept (ThriftServerTransport *transport, GError **error)
{
  int sd = THRIFT_INVALID_SOCKET;
  guint addrlen = 0;
  struct sockaddr_in address;
  ThriftSocket *socket = NULL;

  ThriftServerSocket *tsocket = THRIFT_SERVER_SOCKET (transport);
  ThriftServerTransport *tst = THRIFT_SERVER_TRANSPORT (transport);

  if ((sd = accept(tsocket->sd, (struct sockaddr *) &address, &addrlen)) == -1)
  {
    g_set_error (error, THRIFT_SERVER_SOCKET_ERROR,
                 THRIFT_SERVER_SOCKET_ERROR_ACCEPT,
                 "failed to accept connection - %s",
                 strerror(errno));
    return NULL;
  }

  if(tst->configuration != NULL)
  {
    socket = g_object_new (THRIFT_TYPE_SOCKET, "configuration", tst->configuration, 
		           "remainingmessagesize", tst->configuration->maxMessageSize_, 
		           "knowmessagesize", tst->configuration->maxMessageSize_, NULL);
  }
  else
  {
    socket = g_object_new (THRIFT_TYPE_SOCKET, NULL);
  }
  socket->sd = sd;

  return THRIFT_TRANSPORT(socket);
}

gboolean
thrift_server_socket_close (ThriftServerTransport *transport, GError **error)
{
  ThriftServerSocket *tsocket = THRIFT_SERVER_SOCKET (transport);

  if (close (tsocket->sd) == -1)
  {
    g_set_error (error, THRIFT_SERVER_SOCKET_ERROR,
                 THRIFT_SERVER_SOCKET_ERROR_CLOSE,
                 "unable to close socket - %s", strerror(errno));
    return FALSE;
  }
  tsocket->sd = THRIFT_INVALID_SOCKET;

  return TRUE;
}

/* define the GError domain for this implementation */
GQuark
thrift_server_socket_error_quark (void)
{
  return g_quark_from_static_string(THRIFT_SERVER_SOCKET_ERROR_DOMAIN);
}

/* initializes the instance */
static void
thrift_server_socket_init (ThriftServerSocket *socket)
{
  socket->sd = THRIFT_INVALID_SOCKET;
}

/* destructor */
static void
thrift_server_socket_finalize (GObject *object)
{
  ThriftServerSocket *socket = THRIFT_SERVER_SOCKET (object);

  if (socket->sd != THRIFT_INVALID_SOCKET)
  {
    close (socket->sd);
  }
  socket->sd = THRIFT_INVALID_SOCKET;
}

/* property accessor */
void
thrift_server_socket_get_property (GObject *object, guint property_id,
                                   GValue *value, GParamSpec *pspec)
{
  ThriftServerSocket *socket = THRIFT_SERVER_SOCKET (object);
  ThriftServerTransport *transport = THRIFT_SERVER_TRANSPORT (object);

  switch (property_id)
  {
    case PROP_THRIFT_SERVER_SOCKET_PORT:
      g_value_set_uint (value, socket->port);
      break;
    case PROP_THRIFT_SERVER_SOCKET_PATH:
      g_value_set_string (value, socket->path);
      break;
    case PROP_THRIFT_SERVER_SOCKET_BACKLOG:
      g_value_set_uint (value, socket->backlog);
      break;
    case PROP_THRIFT_SERVER_SOCKET_CONFIGURATION:
      g_value_set_object (value, transport->configuration);
      break;
    case PROP_THRIFT_SERVER_SOCKET_REMAINING_MESSAGE_SIZE:
      g_value_set_long (value, transport->remainingMessageSize_);
      break;
    case PROP_THRIFT_SERVER_SOCKET_KNOW_MESSAGE_SIZE:
      g_value_set_long (value, transport->knowMessageSize_);
      break; 
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
  }
}

/* property mutator */
void
thrift_server_socket_set_property (GObject *object, guint property_id,
                                   const GValue *value, GParamSpec *pspec)
{
  ThriftServerSocket *socket = THRIFT_SERVER_SOCKET (object);
  ThriftServerTransport *transport = THRIFT_SERVER_TRANSPORT (object);

  switch (property_id)
  {
    case PROP_THRIFT_SERVER_SOCKET_PORT:
      socket->port = g_value_get_uint (value);
      break;
    case PROP_THRIFT_SERVER_SOCKET_PATH:
      if (socket->path) {
        g_free(socket->path);
      }
      socket->path = g_strdup (g_value_get_string (value));
      break;
    case PROP_THRIFT_SERVER_SOCKET_BACKLOG:
      socket->backlog = g_value_get_uint (value);
      break;
    case PROP_THRIFT_SERVER_SOCKET_CONFIGURATION:
      transport->configuration = g_value_dup_object (value);
      break;
    case PROP_THRIFT_SERVER_SOCKET_REMAINING_MESSAGE_SIZE:
      transport->remainingMessageSize_ = g_value_get_long (value);
      break;
    case PROP_THRIFT_SERVER_SOCKET_KNOW_MESSAGE_SIZE:
      transport->knowMessageSize_ = g_value_get_long (value);
      break;
    default:
      G_OBJECT_WARN_INVALID_PROPERTY_ID (object, property_id, pspec);
      break;
  }
}

/* initializes the class */
static void
thrift_server_socket_class_init (ThriftServerSocketClass *cls)
{
  ThriftServerTransportClass *tstc = THRIFT_SERVER_TRANSPORT_CLASS (cls);
  GObjectClass *gobject_class = G_OBJECT_CLASS (cls);
  GParamSpec *param_spec = NULL;

  /* setup accessors and mutators */
  gobject_class->get_property = thrift_server_socket_get_property;
  gobject_class->set_property = thrift_server_socket_set_property;

  param_spec = g_param_spec_uint ("port",
                                  "port (construct)",
                                  "Set the port to listen to",
                                  0, /* min */
                                  65535, /* max */
                                  9090, /* default by convention */
                                  G_PARAM_CONSTRUCT_ONLY |
                                  G_PARAM_READWRITE);
  g_object_class_install_property (gobject_class,
                                   PROP_THRIFT_SERVER_SOCKET_PORT,
                                   param_spec);

  param_spec = g_param_spec_string ("path",
                                    "path (construct)",
                                    "Set the path to listen to",
                                    NULL, /* default value */
                                    G_PARAM_CONSTRUCT_ONLY |
                                    G_PARAM_READWRITE);
  g_object_class_install_property (gobject_class,
                                   PROP_THRIFT_SERVER_SOCKET_PATH,
                                   param_spec);

  param_spec = g_param_spec_uint ("backlog",
                                  "backlog (construct)",
                                  "Set the accept backlog",
                                  0, /* max */
                                  65534, /* max */
                                  1024, /* default */
                                  G_PARAM_CONSTRUCT_ONLY |
                                  G_PARAM_READWRITE);
  g_object_class_install_property (gobject_class,
                                   PROP_THRIFT_SERVER_SOCKET_BACKLOG,
                                   param_spec);

  param_spec = g_param_spec_object ("configuration",
                                    "configuration (construct)",
                                    "Thtift Configuration",
                                    THRIFT_TYPE_CONFIGURATION,
                                    G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
  g_object_class_install_property (gobject_class,
                                   PROP_THRIFT_SERVER_SOCKET_CONFIGURATION,
                                   param_spec);

  param_spec = g_param_spec_long ("remainingmessagesize",
                                  "remainingmessagesize (construct)",
                                  "Set the remaining message size",
                                  0, /* min */
                                  G_MAXINT32, /* max */
                                  DEFAULT_MAX_MESSAGE_SIZE, /* default by construct */
                                  G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
  g_object_class_install_property (gobject_class,
                                   PROP_THRIFT_SERVER_SOCKET_REMAINING_MESSAGE_SIZE,
                                   param_spec);

  param_spec = g_param_spec_long ("knowmessagesize",
                                  "knowmessagesize (construct)",
                                  "Set the known size of the message",
                                  0, /* min */
                                  G_MAXINT32, /* max */
                                  DEFAULT_MAX_MESSAGE_SIZE, /* default by construct */
                                  G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY);
  g_object_class_install_property (gobject_class,
                                   PROP_THRIFT_SERVER_SOCKET_KNOW_MESSAGE_SIZE,
                                   param_spec);
  gobject_class->finalize = thrift_server_socket_finalize;

  tstc->listen = thrift_server_socket_listen;
  tstc->accept = thrift_server_socket_accept;
  tstc->close = thrift_server_socket_close;
}

