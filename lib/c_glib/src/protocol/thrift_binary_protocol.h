#ifndef _THRIFT_BINARY_PROTOCOL_H
#define _THRIFT_BINARY_PROTOCOL_H

#include <glib-object.h>

#include "protocol/thrift_protocol.h"
#include "transport/thrift_transport.h"

/*! \file thrift_binary_protocol.h
 *  \brief Binary protocol implementation of a Thrift protocol.  Implements the
 *         ThriftProtocol interface.
 */

/* type macros */
#define THRIFT_TYPE_BINARY_PROTOCOL (thrift_binary_protocol_get_type ())
#define THRIFT_BINARY_PROTOCOL(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), \
                                         THRIFT_TYPE_BINARY_PROTOCOL, \
                                         ThriftBinaryProtocol))
#define THRIFT_IS_BINARY_PROTOCOL(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), \
                                            THRIFT_TYPE_BINARY_PROTOCOL))
#define THRIFT_BINARY_PROTOCOL_CLASS(c) (G_TYPE_CHECK_CLASS_CAST ((c), \
                                             THRIFT_TYPE_BINARY_PROTOCOL, \
                                             ThriftBinaryProtocolClass))
#define THRIFT_IS_BINARY_PROTOCOL_CLASS(c) (G_TYPE_CHECK_CLASS_TYPE ((c), \
                                                THRIFT_TYPE_BINARY_PROTOCOL))
#define THRIFT_BINARY_PROTOCOL_GET_CLASS(obj) \
            (G_TYPE_INSTANCE_GET_CLASS ((obj), THRIFT_TYPE_BINARY_PROTOCOL, \
                                        ThriftBinaryProtocolClass))

/* version numbers */
#define THRIFT_BINARY_PROTOCOL_VERSION_1 0x80010000
#define THRIFT_BINARY_PROTOCOL_VERSION_MASK 0xffff0000

/*!
 * Thrift Binary Protocol instance.
 */
struct _ThriftBinaryProtocol
{
  ThriftProtocol parent;
};
typedef struct _ThriftBinaryProtocol ThriftBinaryProtocol;

/*!
 * Thrift Binary Protocol class.
 */
struct _ThriftBinaryProtocolClass
{
  ThriftProtocolClass parent;
};
typedef struct _ThriftBinaryProtocolClass ThriftBinaryProtocolClass;

/* used by THRIFT_TYPE_BINARY_PROTOCOL */
GType thrift_binary_protocol_get_type (void);

#endif /* _THRIFT_BINARY_PROTOCOL_H */
