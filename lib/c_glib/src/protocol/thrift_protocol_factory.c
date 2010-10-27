#include "thrift.h"
#include "protocol/thrift_protocol_factory.h"

/* forward declarations */
static void thrift_protocol_factory_class_init (ThriftProtocolFactoryClass *cls);
ThriftProtocol *thrift_protocol_factory_get_protocol(ThriftProtocolFactory *factory, ThriftTransport *transport);


/* define ThriftProtocolFactoryInterface's type */
GType
thrift_protocol_factory_get_type (void)
{
  static GType type = 0;

  if (type == 0)
  {
    static const GTypeInfo info = {
      sizeof (ThriftProtocolFactoryClass),
      NULL, /* base_init */
      NULL, /* base_finalize */
      (GClassInitFunc) thrift_protocol_factory_class_init,
      NULL, /* class_finalize */
      NULL, /* class_data */
      sizeof (ThriftProtocolFactory),
      0, /* n_preallocs */
      NULL, /* instance_init */
      NULL, /* value_table */
    };

    type = g_type_register_static (G_TYPE_OBJECT, "ThriftProtocolFactory",
                                   &info, G_TYPE_FLAG_ABSTRACT);
  }

  return type;
}

static void
thrift_protocol_factory_class_init (ThriftProtocolFactoryClass *cls)
{
  cls->get_protocol = thrift_protocol_factory_get_protocol;
}

ThriftProtocol *
thrift_protocol_factory_get_protocol(ThriftProtocolFactory *factory,
                                     ThriftTransport *transport)
{
  return THRIFT_PROTOCOL_FACTORY_GET_CLASS (factory)->get_protocol (factory,
                                                                    transport);
}

