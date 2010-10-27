#ifndef _THRIFT_SERVER_SOCKET_H
#define _THRIFT_SERVER_SOCKET_H

#include <glib-object.h>

#include "thrift_server_transport.h"

/*! \file thrift_server_socket.h
 *  \brief Socket implementation of a Thrift server transport.  Implements the
 *         ThriftServerTransport class.
 */

/* type macros */
#define THRIFT_TYPE_SERVER_SOCKET (thrift_server_socket_get_type ())
#define THRIFT_SERVER_SOCKET(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), \
                                       THRIFT_TYPE_SERVER_SOCKET, \
                                       ThriftServerSocket))
#define THRIFT_IS_SERVER_SOCKET(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), \
                                          THRIFT_TYPE_SERVER_SOCKET))
#define THRIFT_SERVER_SOCKET_CLASS(c) (G_TYPE_CHECK_CLASS_CAST ((c), \
                                           THRIFT_TYPE_SERVER_SOCKET, \
                                           ThriftServerSocketClass))
#define THRIFT_IS_SERVER_SOCKET_CLASS(c) (G_TYPE_CHECK_CLASS_TYPE ((c), \
                                              THRIFT_TYPE_SERVER_SOCKET))
#define THRIFT_SERVER_SOCKET_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), \
                                                 THRIFT_TYPE_SERVER_SOCKET, \
                                                 ThriftServerSocketClass))

/*!
 * Thrift ServerSocket instance.
 */
struct _ThriftServerSocket
{
  ThriftServerTransport parent;

  /* private */
  gshort port;
  gshort backlog;
  int sd;
  guint8 *buf;
  guint32 buf_size;
  guint32 buf_len;
};
typedef struct _ThriftServerSocket ThriftServerSocket;

/*!
 * Thrift ServerSocket class.
 */
struct _ThriftServerSocketClass
{
  ThriftServerTransportClass parent;
};
typedef struct _ThriftServerSocketClass ThriftServerSocketClass;

/* used by THRIFT_TYPE_SERVER_SOCKET */
GType thrift_server_socket_get_type (void);

/* define error/exception types */
typedef enum
{
  THRIFT_SERVER_SOCKET_ERROR_SOCKET,
  THRIFT_SERVER_SOCKET_ERROR_SETSOCKOPT,
  THRIFT_SERVER_SOCKET_ERROR_BIND,
  THRIFT_SERVER_SOCKET_ERROR_LISTEN,
  THRIFT_SERVER_SOCKET_ERROR_ACCEPT,
  THRIFT_SERVER_SOCKET_ERROR_CLOSE
} ThriftServerSocketError;

/* define a error domain for GError to use */
GQuark thrift_server_socket_error_quark (void);
#define THRIFT_SERVER_SOCKET_ERROR (thrift_server_socket_error_quark ())

#endif
