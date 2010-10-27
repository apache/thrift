#ifndef _THRIFT_SIMPLE_SERVER_H
#define _THRIFT_SIMPLE_SERVER_H

#include <glib-object.h>

#include "server/thrift_server.h"

/*! \file thrift_simple_server.h
 *  \brief A simple Thrift server, single-threaded.
 */

/* type macros */
#define THRIFT_TYPE_SIMPLE_SERVER (thrift_simple_server_get_type ())
#define THRIFT_SIMPLE_SERVER(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), \
                                       THRIFT_TYPE_SIMPLE_SERVER, \
                                       ThriftSimpleServer))
#define THRIFT_IS_SIMPLE_SERVER(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), \
                                          THRIFT_TYPE_SIMPLE_SERVER))
#define THRIFT_SIMPLE_SERVER_CLASS(c) (G_TYPE_CHECK_CLASS_CAST ((c) \
                                           THRIFT_TYPE_SIMPLE_SERVER, \
                                           ThriftSimpleServerClass))
#define THRIFT_IS_SIMPLE_SERVER_CLASS(c) (G_TYPE_CHECK_CLASS_TYPE ((c), \
                                              THRIFT_TYPE_SIMPLE_SERVER))
#define THRIFT_SIMPLE_SERVER_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), \
                                                 THRIFT_TYPE_SIMPLE_SERVER, \
                                                 ThriftSimpleServerClass))

/**
 * Thrift Simple Server instance.
 */
struct _ThriftSimpleServer
{
  ThriftServer parent;

  /* private */
  volatile gboolean running;
};
typedef struct _ThriftSimpleServer ThriftSimpleServer;

/**
 * Thrift Simple Server class.
 */
struct _ThriftSimpleServerClass
{
  ThriftServerClass parent;
};
typedef struct _ThriftSimpleServerClass ThriftSimpleServerClass;

/* used by THRIFT_TYPE_SIMPLE_SERVER */
GType thrift_simple_server_get_type (void);

#endif /* _THRIFT_SIMPLE_SERVER_H */

