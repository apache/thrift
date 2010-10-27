#ifndef _THRIFT_SERVER_TRANSPORT_H
#define _THRIFT_SERVER_TRANSPORT_H

#include <glib-object.h>

#include "thrift_transport.h"

/*! \file thrift_server_transport.h
 *  \brief Abstract class for Thrift server transports.
 */

/* type macros */	
#define THRIFT_TYPE_SERVER_TRANSPORT (thrift_server_transport_get_type ())
#define THRIFT_SERVER_TRANSPORT(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), \
                                          THRIFT_TYPE_SERVER_TRANSPORT, \
                                          ThriftServerTransport))
#define THRIFT_IS_SERVER_TRANSPORT(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), \
                                             THRIFT_TYPE_SERVER_TRANSPORT))
#define THRIFT_SERVER_TRANSPORT_CLASS(c) (G_TYPE_CHECK_CLASS_CAST ((c), \
                                              THRIFT_TYPE_SERVER_TRANSPORT, \
                                              ThriftServerTransportClass))
#define THRIFT_IS_SERVER_TRANSPORT_CLASS(c) (G_TYPE_CHECK_CLASS_TYPE ((c), \
                                                 THRIFT_TYPE_SERVER_TRANSPORT))
#define THRIFT_SERVER_TRANSPORT_GET_CLASS(obj) \
            (G_TYPE_INSTANCE_GET_CLASS ((obj), THRIFT_TYPE_SERVER_TRANSPORT, \
                                        ThriftServerTransportClass))

struct _ThriftServerTransport
{
  GObject parent;
};
typedef struct _ThriftServerTransport ThriftServerTransport;

/*!
 * Thrift Transport class
 */
struct _ThriftServerTransportClass
{
  GObjectClass parent;

  /* vtable */
  gboolean (*listen) (ThriftServerTransport *transport, GError **error);
  ThriftTransport *(*accept) (ThriftServerTransport *transport, GError **error);
  gboolean (*close) (ThriftServerTransport *transport, GError **error);
};
typedef struct _ThriftServerTransportClass ThriftServerTransportClass;

/* used by THRIFT_TYPE_SERVER_TRANSPORT */
GType thrift_server_transport_get_type (void); 

/*!
 * Listen for new connections.
 * \public \memberof ThriftServerTransportClass
 */
gboolean thrift_server_transport_listen (ThriftServerTransport *transport,
                                         GError **error);

/*!
 * Accept a connection.
 * \public \memberof ThriftServerTransportClass
 */
ThriftTransport *thrift_server_transport_accept 
    (ThriftServerTransport *transport, GError **error);

/*!
 * Close the transport.
 * \public \memberof ThriftServerTransportClass
 */
gboolean thrift_server_transport_close (ThriftServerTransport *transport,
                                        GError **error);

#endif /* _THRIFT_SERVER_TRANSPORT_H */
