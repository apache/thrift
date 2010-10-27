#include <errno.h>
#include <netdb.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#include "thrift.h"
#include "transport/thrift_transport.h"
#include "transport/thrift_socket.h"

/* object properties */
enum _ThriftSocketProperties
{
  PROP_0,
  PROP_THRIFT_SOCKET_HOSTNAME,
  PROP_THRIFT_SOCKET_PORT
};

/* for errors coming from socket() and connect() */
extern int errno;

/* forward declarations */
static void thrift_socket_instance_init (ThriftSocket *self);
static void thrift_socket_class_init (ThriftSocketClass *cls);

gboolean thrift_socket_is_open (ThriftTransport *transport);
gboolean thrift_socket_open (ThriftTransport *transport, GError **error);
gboolean thrift_socket_close (ThriftTransport *transport, GError **error);
gint32 thrift_socket_read (ThriftTransport *transport, gpointer buf,
                           guint32 len, GError **error);
gboolean thrift_socket_read_end (ThriftTransport *transport, GError **error);
gboolean thrift_socket_write (ThriftTransport *transport, const gpointer buf,
                              const guint32 len, GError **error);
gboolean thrift_socket_write_end (ThriftTransport *transport, GError **error);
gboolean thrift_socket_flush (ThriftTransport *transport, GError **error);

GType
thrift_socket_get_type (void)
{
  static GType type = 0;

  if (type == 0)
  {
    static const GTypeInfo info =
    {
      sizeof (ThriftSocketClass),
      NULL, /* base_init */
      NULL, /* base_finalize */
      (GClassInitFunc) thrift_socket_class_init,
      NULL, /* class finalize */
      NULL, /* class data */
      sizeof (ThriftSocket),
      0, /* n_preallocs */
      (GInstanceInitFunc) thrift_socket_instance_init,
      NULL, /* value_table */
    };

    type = g_type_register_static (THRIFT_TYPE_TRANSPORT,
                                   "ThriftSocket", &info, 0);
  }

  return type;
}

/* initializes the instance */
static void
thrift_socket_instance_init (ThriftSocket *socket)
{
  socket->sd = 0;
}

/* destructor */
static void
thrift_socket_finalize (GObject *object)
{
  ThriftSocket *socket = THRIFT_SOCKET (object);

  if (socket->hostname != NULL)
  {
    g_free (socket->hostname);
  }
  socket->hostname = NULL;

  if (socket->sd != 0)
  {
    close (socket->sd);
  }
  socket->sd = 0;
}

/* property accessor */
void
thrift_socket_get_property (GObject *object, guint property_id,
                            GValue *value, GParamSpec *pspec)
{
  THRIFT_UNUSED_VAR (pspec);
  ThriftSocket *socket = THRIFT_SOCKET (object);

  switch (property_id)
  {
    case PROP_THRIFT_SOCKET_HOSTNAME:
      g_value_set_string (value, socket->hostname);
      break;
    case PROP_THRIFT_SOCKET_PORT:
      g_value_set_uint (value, socket->port);
      break;
  }
}

/* property mutator */
void
thrift_socket_set_property (GObject *object, guint property_id,
                            const GValue *value, GParamSpec *pspec)
{
  THRIFT_UNUSED_VAR (pspec);
  ThriftSocket *socket = THRIFT_SOCKET (object);

  switch (property_id)
  {
    case PROP_THRIFT_SOCKET_HOSTNAME:
      socket->hostname = g_strdup (g_value_get_string (value));
      break;
    case PROP_THRIFT_SOCKET_PORT:
      socket->port = g_value_get_uint (value);
      break;
  }
}

/* initializes the class */
static void
thrift_socket_class_init (ThriftSocketClass *cls)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (cls);
  GParamSpec *param_spec = NULL;

  /* setup accessors and mutators */
  gobject_class->get_property = thrift_socket_get_property;
  gobject_class->set_property = thrift_socket_set_property;

  param_spec = g_param_spec_string ("hostname",
                                    "hostname (construct)",
                                    "Set the hostname of the remote host",
                                    "localhost", /* default value */
                                    G_PARAM_CONSTRUCT_ONLY |
                                    G_PARAM_READWRITE);
  g_object_class_install_property (gobject_class, PROP_THRIFT_SOCKET_HOSTNAME, 
                                   param_spec);

  param_spec = g_param_spec_uint ("port",
                                  "port (construct)",
                                  "Set the port of the remote host",
                                  0, /* min */
                                  65534, /* max */
                                  9090, /* default by convention */
                                  G_PARAM_CONSTRUCT_ONLY |
                                  G_PARAM_READWRITE);
  g_object_class_install_property (gobject_class, PROP_THRIFT_SOCKET_PORT, 
                                   param_spec);

  ThriftTransportClass *ttc = THRIFT_TRANSPORT_CLASS (cls);

  gobject_class->finalize = thrift_socket_finalize;
  ttc->is_open = thrift_socket_is_open;
  ttc->open = thrift_socket_open;
  ttc->close = thrift_socket_close;
  ttc->read = thrift_socket_read;
  ttc->read_end = thrift_socket_read_end;
  ttc->write = thrift_socket_write;
  ttc->write_end = thrift_socket_write_end;
  ttc->flush = thrift_socket_flush;
}

/* implements thrift_transport_is_open */
gboolean
thrift_socket_is_open (ThriftTransport *transport)
{
  ThriftSocket *socket = THRIFT_SOCKET (transport);
  return socket->sd != 0;
}

/* implements thrift_transport_open */
gboolean
thrift_socket_open (ThriftTransport *transport, GError **error)
{
  struct hostent *hp = NULL;
  struct sockaddr_in pin;

  ThriftSocket *tsocket = THRIFT_SOCKET (transport);
  g_return_val_if_fail (tsocket->sd == 0, FALSE);

  /* lookup the destination host */
  if ((hp = gethostbyname (tsocket->hostname)) == NULL)
  {
    /* host lookup failed, bail out with an error */
    g_set_error (error, THRIFT_TRANSPORT_ERROR, THRIFT_TRANSPORT_ERROR_HOST,
                 "host lookup failed for %s:%d - %s",
                 tsocket->hostname, tsocket->port,
                 hstrerror (h_errno));
    return FALSE;
  }

  /* create a socket structure */
  memset (&pin, 0, sizeof(pin));
  pin.sin_family = AF_INET;
  pin.sin_addr.s_addr = ((struct in_addr *) (hp->h_addr))->s_addr;
  pin.sin_port = htons (tsocket->port); 

  /* create the socket */
  if ((tsocket->sd = socket (AF_INET, SOCK_STREAM, 0)) == -1)
  {
    g_set_error (error, THRIFT_TRANSPORT_ERROR, THRIFT_TRANSPORT_ERROR_SOCKET,
                 "failed to create socket for host %s:%d - %s",
                 tsocket->hostname, tsocket->port,
                 strerror(errno));
    return FALSE;
  }

  /* open a connection */
  if (connect (tsocket->sd, (struct sockaddr *) &pin, sizeof(pin)) == -1)
  {
    g_set_error (error, THRIFT_TRANSPORT_ERROR, THRIFT_TRANSPORT_ERROR_CONNECT,
                 "failed to connect to host %s:%d - %s",
                 tsocket->hostname, tsocket->port, strerror(errno));
    return FALSE;
  }

  return TRUE;
}

/* implements thrift_transport_close */
gboolean
thrift_socket_close (ThriftTransport *transport, GError **error)
{
  ThriftSocket *socket = THRIFT_SOCKET (transport);

  if (close (socket->sd) == -1)
  {
    g_set_error (error, THRIFT_TRANSPORT_ERROR, THRIFT_TRANSPORT_ERROR_CLOSE,
                 "unable to close socket - %s",
                 strerror(errno));
    return FALSE;
  }

  socket->sd = 0;
  return TRUE;
}

/* implements thrift_transport_read */
gint32
thrift_socket_read (ThriftTransport *transport, gpointer buf,
                    guint32 len, GError **error)
{
  gint ret = 0;
  guint got = 0;

  ThriftSocket *socket = THRIFT_SOCKET (transport);

  while (got < len)
  {
    ret = recv (socket->sd, buf, len, 0);
    if (ret < 0)
    {
      g_set_error (error, THRIFT_TRANSPORT_ERROR,
                   THRIFT_TRANSPORT_ERROR_RECEIVE,
                   "failed to read %d bytes - %s", len, strerror(errno));
      return -1;
    }
    got += ret;
  }

  return got;
}

/* implements thrift_transport_read_end
 * called when write is complete.  nothing to do on our end. */
gboolean
thrift_socket_read_end (ThriftTransport *transport, GError **error)
{
  /* satisfy -Wall */
  THRIFT_UNUSED_VAR (transport);
  THRIFT_UNUSED_VAR (error);
  return TRUE;
}

/* implements thrift_transport_write */
gboolean
thrift_socket_write (ThriftTransport *transport, const gpointer buf,     
                     const guint32 len, GError **error)
{
  gint ret = 0;
  guint sent = 0;

  ThriftSocket *socket = THRIFT_SOCKET (transport);
  g_return_val_if_fail (socket->sd != 0, FALSE);

  while (sent < len)
  {
    ret = send (socket->sd, buf + sent, len - sent, 0);
    if (ret < 0)
    {
      g_set_error (error, THRIFT_TRANSPORT_ERROR,
                   THRIFT_TRANSPORT_ERROR_SEND,
                   "failed to send %d bytes - %s", len, strerror(errno));
      return FALSE;
    }
    sent += ret;
  }

  return TRUE;
}

/* implements thrift_transport_write_end
 * called when write is complete.  nothing to do on our end. */
gboolean
thrift_socket_write_end (ThriftTransport *transport, GError **error)
{
  /* satisfy -Wall */
  THRIFT_UNUSED_VAR (transport);
  THRIFT_UNUSED_VAR (error);
  return TRUE;
}

/* implements thrift_transport_flush
 * flush pending data.  since we are not buffered, this is a no-op */
gboolean
thrift_socket_flush (ThriftTransport *transport, GError **error)
{
  /* satisfy -Wall */
  THRIFT_UNUSED_VAR (transport);
  THRIFT_UNUSED_VAR (error);
  return TRUE;
}


