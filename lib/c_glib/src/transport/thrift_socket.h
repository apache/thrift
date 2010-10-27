#ifndef _THRIFT_SOCKET_H
#define _THRIFT_SOCKET_H

#include <glib-object.h>

#include "transport/thrift_transport.h"

/*! \file thrift_socket.h
 *  \brief Socket implementation of a Thrift transport.  Subclasses the
 *         ThriftTransport class.
 */

/* type macros */
#define THRIFT_TYPE_SOCKET (thrift_socket_get_type ())
#define THRIFT_SOCKET(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), \
                                THRIFT_TYPE_SOCKET, ThriftSocket))
#define THRIFT_IS_SOCKET(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), \
                                   THRIFT_TYPE_SOCKET))
#define THRIFT_SOCKET_CLASS(c) (G_TYPE_CHECK_CLASS_CAST ((c), \
                                    THRIFT_TYPE_SOCKET, ThriftSocketClass))
#define THRIFT_IS_SOCKET_CLASS(c) (G_TYPE_CHECK_CLASS_TYPE ((c), \
                                       THRIFT_TYPE_SOCKET))
#define THRIFT_SOCKET_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), \
                                          THRIFT_TYPE_SOCKET, \
                                          ThriftSocketClass))

/*!
 * Thrift Socket instance.
 */
struct _ThriftSocket
{
  ThriftTransport parent;

  /* private */
  gchar *hostname;
  gshort port;
  int sd;
  guint8 *buf;
  guint32 buf_size;
  guint32 buf_len;
};
typedef struct _ThriftSocket ThriftSocket;

/*!
 * Thrift Socket class.
 */
struct _ThriftSocketClass
{
  ThriftTransportClass parent;
};
typedef struct _ThriftSocketClass ThriftSocketClass;

/* used by THRIFT_TYPE_SOCKET */
GType thrift_socket_get_type (void);

#endif
