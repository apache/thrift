#include "thrift.h"
#include "transport/thrift_transport_factory.h"

/* forward declaration s*/
static void thrift_transport_factory_class_init (ThriftTransportFactoryClass *cls);
ThriftTransport *thrift_transport_factory_get_transport (ThriftTransportFactory *factory, ThriftTransport *transport);

GType
thrift_transport_factory_get_type (void)
{
  static GType type = 0;

  if (type == 0)
  {
    static const GTypeInfo info = {
      sizeof (ThriftTransportFactoryClass),
      NULL, /* base_init */
      NULL, /* base_finalize */
      (GClassInitFunc) thrift_transport_factory_class_init,
      NULL, /* class_finalize */
      NULL, /* class_data */
      sizeof (ThriftTransportFactory),
      0, /* n_preallocs */
      NULL, /* instance_init */
      NULL, /* value_table */
    };

    type = g_type_register_static (G_TYPE_OBJECT, "ThriftTransportFactory",
                                   &info, 0);
  }

  return type;
}

static void
thrift_transport_factory_class_init (ThriftTransportFactoryClass *cls)
{
  cls->get_transport = thrift_transport_factory_get_transport;
}

/* builds a transport from the base transport. */
ThriftTransport *
thrift_transport_factory_get_transport (ThriftTransportFactory *factory,
                                        ThriftTransport *transport)
{
  THRIFT_UNUSED_VAR (factory);
  return transport;
}



