
#include "thrift.h"
#include "protocol/thrift_binary_protocol.h"
#include "protocol/thrift_binary_protocol_factory.h"

/* forward declarations */
static void thrift_binary_protocol_factory_class_init (ThriftProtocolFactoryClass *cls);

ThriftProtocol *thrift_binary_protocol_factory_get_protocol (ThriftProtocolFactory *factory, ThriftTransport *transport);

GType
thrift_binary_protocol_factory_get_type (void)
{
  static GType type = 0;

  if (type == 0)
  {
    static const GTypeInfo info =
    {
      sizeof (ThriftBinaryProtocolFactoryClass),
      NULL, /* base_init */
      NULL, /* base_finalize */
      (GClassInitFunc) thrift_binary_protocol_factory_class_init,
      NULL, /* class_finalize */
      NULL, /* class_data */
      sizeof (ThriftBinaryProtocolFactory),
      0, /* n_preallocs */
      NULL, /* instance_init */
      NULL, /* value_table */
    };

    type = g_type_register_static (THRIFT_TYPE_PROTOCOL_FACTORY,
                                   "ThriftBinaryProtocolFactoryType",
                                   &info, 0);
  }

  return type;
}

static void
thrift_binary_protocol_factory_class_init (ThriftProtocolFactoryClass *cls)
{
  cls->get_protocol = thrift_binary_protocol_factory_get_protocol;
}

ThriftProtocol *
thrift_binary_protocol_factory_get_protocol (ThriftProtocolFactory *factory,
                                             ThriftTransport *transport)
{
  THRIFT_UNUSED_VAR (factory);

  ThriftBinaryProtocol *tb = g_object_new (THRIFT_TYPE_BINARY_PROTOCOL,
                                           "transport", transport, NULL);

  return THRIFT_PROTOCOL (tb);
}


