#ifndef _THRIFT_SERVER_H
#define _THRIFT_SERVER_H

#include <glib-object.h>

#include "processor/thrift_processor.h"
#include "transport/thrift_server_transport.h"
#include "transport/thrift_transport_factory.h"
#include "protocol/thrift_protocol_factory.h"

/*! \file thrift_server.h
 *  \brief Abstract class for Thrift servers.
 */

/* type macros */	
#define THRIFT_TYPE_SERVER (thrift_server_get_type ())
#define THRIFT_SERVER(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), \
                                THRIFT_TYPE_SERVER, ThriftServer))
#define THRIFT_IS_SERVER(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), \
                                   THRIFT_TYPE_SERVER))
#define THRIFT_SERVER_CLASS(c) (G_TYPE_CHECK_CLASS_CAST ((c), \
                                    THRIFT_TYPE_SERVER, \
                                    ThriftServerClass))
#define THRIFT_IS_SERVER_CLASS(c) (G_TYPE_CHECK_CLASS_TYPE ((c), \
                                       THRIFT_TYPE_SERVER))
#define THRIFT_SERVER_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), \
                                          THRIFT_TYPE_SERVER, \
                                          ThriftServerClass))

/*!
 * Thrift Server object
 */
struct _ThriftServer
{
  GObject parent;

  /* protected */
  ThriftProcessor *processor;
  ThriftServerTransport *server_transport;
  ThriftTransportFactory *input_transport_factory;
  ThriftTransportFactory *output_transport_factory;
  ThriftProtocolFactory *input_protocol_factory;
  ThriftProtocolFactory *output_protocol_factory;
};
typedef struct _ThriftServer ThriftServer;

/*!
 * Thrift Server class
 */
struct _ThriftServerClass
{
  GObjectClass parent;

  /* vtable */
  void (*serve) (ThriftServer *server);
  void (*stop) (ThriftServer *server);
};
typedef struct _ThriftServerClass ThriftServerClass;

/* used by THRIFT_TYPE_SERVER */
GType thrift_server_get_type (void); 

/*!
 * Processes the request.
 * \public \memberof ThriftServerClass
 */
void thrift_server_serve (ThriftServer *server);

/*!
 * Stop handling requests.
 */
void thrift_server_stop (ThriftServer *server);

#endif /* _THRIFT_SERVER_H */

