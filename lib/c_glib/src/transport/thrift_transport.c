#include "thrift.h"
#include "transport/thrift_transport.h"

/* define the GError domain string */
#define THRIFT_TRANSPORT_ERROR_DOMAIN "thrift-transport-error-quark"

/* forward declarations */
static void thrift_transport_class_init (ThriftTransportClass *cls);

/* define ThriftTransportInterface's type */
GType
thrift_transport_get_type (void)
{
  static GType type = 0;

  if (type == 0)
  {
    static const GTypeInfo info =
    {
      sizeof (ThriftTransportClass),
      NULL, /* base_init */
      NULL, /* base_finalize */
      (GClassInitFunc) thrift_transport_class_init,
      NULL, /* class_finalize */
      NULL, /* class_data */
      sizeof (ThriftTransport),
      0, /* n_preallocs */
      NULL, /* instance_init */
      NULL, /* value_table */
    };

    type = g_type_register_static (G_TYPE_OBJECT, "ThriftTransport",
                                   &info, G_TYPE_FLAG_ABSTRACT);
  }

  return type;
}

/* class initializer for ThriftTransport */
static void
thrift_transport_class_init (ThriftTransportClass *cls)
{
  /* set these as virtual methods to be implemented by a subclass */
  cls->is_open = thrift_transport_is_open;
  cls->open = thrift_transport_open;
  cls->close = thrift_transport_close;
  cls->read = thrift_transport_read;
  cls->read_end = thrift_transport_read_end;
  cls->write = thrift_transport_write;
  cls->write_end = thrift_transport_write_end;
  cls->flush = thrift_transport_flush;
}

gboolean 
thrift_transport_is_open (ThriftTransport *transport)
{
  return THRIFT_TRANSPORT_GET_CLASS (transport)->is_open (transport);
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

/* define the GError domain for Thrift transports */
GQuark
thrift_transport_error_quark (void)
{
  return g_quark_from_static_string (THRIFT_TRANSPORT_ERROR_DOMAIN);
}

