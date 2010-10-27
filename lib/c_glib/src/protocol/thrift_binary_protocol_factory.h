#ifndef _THRIFT_BINARY_PROTOCOL_FACTORY_H
#define _THRIFT_BINARY_PROTOCOL_FACTORY_H

#include <glib-object.h>

#include "protocol/thrift_protocol_factory.h"

/* type macros */
#define THRIFT_TYPE_BINARY_PROTOCOL_FACTORY \
            (thrift_binary_protocol_factory_get_type ())
#define THRIFT_BINARY_PROTOCOL_FACTORY(obj) \
            (G_TYPE_CHECK_INSTANCE_CAST ((obj), \
                                         THRIFT_TYPE_BINARY_PROTOCOL_FACTORY, \
                                         ThriftBinaryProtocolFactory))
#define THRIFT_IS_BINARY_PROTOCOL_FACTORY(obj) \
            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), \
                                         THRIFT_TYPE_BINARY_PROTOCOL_FACTORY))
#define THRIFT_BINARY_PROTOCOL_FACTORY_CLASS(c) \
            (G_TYPE_CHECK_CLASS_CAST ((c), \
                                      THRIFT_TYPE_BINARY_PROTOCOL_FACTORY, \
                                      ThriftBinaryProtocolFactoryClass))
#define THRIFT_IS_BINARY_PROTOCOL_FACTORY_CLASS(c) \
            (G_TYPE_CHECK_CLASS_TYPE ((c), \
                                      THRIFT_TYPE_BINARY_PROTOCOL_FACTORY))
#define THRIFT_BINARY_PROTOCOL_FACTORY_GET_CLASS(obj) \
            (G_TYPE_INSTANCE_GET_CLASS ((obj), \
                                        THRIFT_TYPE_BINARY_PROTOCOL_FACTORY, \
                                        ThriftBinaryProtocolFactoryClass))

struct _ThriftBinaryProtocolFactory
{
  ThriftProtocolFactory parent;
};
typedef struct _ThriftBinaryProtocolFactory ThriftBinaryProtocolFactory;

struct _ThriftBinaryProtocolFactoryClass
{
  ThriftProtocolFactoryClass parent;
};
typedef struct _ThriftBinaryProtocolFactoryClass 
               ThriftBinaryProtocolFactoryClass;

/* used by THRIFT_TYPE_BINARY_PROTOCOL_FACTORY */
GType thrift_binary_protocol_factory_get_type (void);

#endif /* _THRIFT_BINARY_PROTOCOL_FACTORY_H */
