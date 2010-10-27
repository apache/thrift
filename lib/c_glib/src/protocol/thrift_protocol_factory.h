#ifndef _THRIFT_PROTOCOL_FACTORY_H
#define _THRIFT_PROTOCOL_FACTORY_H

#include <glib-object.h>

#include "transport/thrift_transport.h"
#include "protocol/thrift_protocol.h"

/*! \file thrift_protocol_factory.h
 *  \brief Abstract class for Thrift protocol factory implementations.
 */

/* type macros */
#define THRIFT_TYPE_PROTOCOL_FACTORY (thrift_protocol_factory_get_type ())
#define THRIFT_PROTOCOL_FACTORY(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), \
                                          THRIFT_TYPE_PROTOCOL_FACTORY, \
                                          ThriftProtocolFactory))
#define THRIFT_IS_PROTOCOL_FACTORY(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), \
                                             THRIFT_TYPE_PROTOCOL_FACTORY))
#define THRIFT_PROTOCOL_FACTORY_CLASS(c) (G_TYPE_CHECK_CLASS_CAST ((c), \
                                              THRIFT_TYPE_PROTOCOL_FACTORY, \
                                              ThriftProtocolFactoryClass))
#define THRIFT_IS_PROTOCOL_FACTORY_CLASS(c) (G_TYPE_CHECK_CLASS_TYPE ((c), \
                                                 THRIFT_TYPE_PROTOCOL_FACTORY))
#define THRIFT_PROTOCOL_FACTORY_GET_CLASS(obj) \
            (G_TYPE_INSTANCE_GET_CLASS ((obj), \
                                        THRIFT_TYPE_PROTOCOL_FACTORY, \
                                        ThriftProtocolFactoryClass))

/*!
 * Thrift Protocol Factory object
 */
struct _ThriftProtocolFactory
{
  GObject parent;
};
typedef struct _ThriftProtocolFactory ThriftProtocolFactory;

/*!
 * Thrift Protocol Factory class
 */
struct _ThriftProtocolFactoryClass
{
  GObjectClass parent;

  ThriftProtocol *(*get_protocol) (ThriftProtocolFactory *factory,
                                   ThriftTransport *transport);
};
typedef struct _ThriftProtocolFactoryClass ThriftProtocolFactoryClass;

/* used by THRIFT_TYPE_PROTOCOL_FACTORY */
GType thrift_protocol_factory_get_type (void);

/* virtual public methods */
ThriftProtocol *thrift_protocol_factory_get_protocol(ThriftProtocolFactory *factory, ThriftTransport *transport);

#endif /* _THRIFT_PROTOCOL_FACTORY_H */
