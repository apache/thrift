#include "thrift.h"
#include "transport/thrift_transport.h"
#include "transport/thrift_server_transport.h"

/* forward declarations */
static void thrift_server_transport_class_init (ThriftServerTransportClass *c);

/* define ThriftTransportClass type */
GType
thrift_server_transport_get_type (void)
{
  static GType type = 0;

  if (type == 0)
  {
    static const GTypeInfo info =
    {
      sizeof (ThriftServerTransportClass),
      NULL, /* base_init */
      NULL, /* base_finalize */
      (GClassInitFunc) thrift_server_transport_class_init, 
      NULL, /* class_finalize */
      NULL, /* class_data */
      sizeof (ThriftServerTransport),
      0, /* n_preallocs */
      NULL, /* instance_init */
      NULL, /* value_table */
    };

    type = g_type_register_static (G_TYPE_OBJECT,
                                   "ThriftServerTransport",
                                   &info, G_TYPE_FLAG_ABSTRACT);
  }

  return type;
}

/* base initializer for the server transport interface */
static void
thrift_server_transport_class_init (ThriftServerTransportClass *c)
{
  c->listen = thrift_server_transport_listen;
  c->accept = thrift_server_transport_accept;
  c->close = thrift_server_transport_close;
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


