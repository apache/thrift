#ifndef _THRIFT_BUFFERED_TRANSPORT_H
#define _THRIFT_BUFFERED_TRANSPORT_H

#include <glib.h>
#include <glib-object.h>

#include "transport/thrift_transport.h"

/*! \file thrift_buffered_transport.h
 *  \brief Implementation of a Thrift buffered transport.  Subclasses 
 *         the ThriftTransport class.
 */

/* type macros */
#define THRIFT_TYPE_BUFFERED_TRANSPORT (thrift_buffered_transport_get_type ())
#define THRIFT_BUFFERED_TRANSPORT(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), \
                                        THRIFT_TYPE_BUFFERED_TRANSPORT, \
                                        ThriftBufferedTransport))
#define THRIFT_IS_BUFFERED_TRANSPORT(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), \
                                           THRIFT_TYPE_BUFFERED_TRANSPORT))
#define THRIFT_BUFFERED_TRANSPORT_CLASS(c) (G_TYPE_CHECK_CLASS_CAST ((c), \
                                            THRIFT_TYPE_BUFFERED_TRANSPORT, \
                                            ThriftBufferedTransportClass))
#define THRIFT_IS_BUFFERED_TRANSPORT_CLASS(c) (G_TYPE_CHECK_CLASS_TYPE ((c), \
                                               THRIFT_TYPE_BUFFERED_TRANSPORT)
#define THRIFT_BUFFERED_TRANSPORT_GET_CLASS(obj) \
            (G_TYPE_INSTANCE_GET_CLASS ((obj), \
             THRIFT_TYPE_BUFFERED_TRANSPORT, \
             ThriftBufferedTransportClass))

/*!
 * ThriftBufferedTransport  instance.
 */
struct _ThriftBufferedTransport
{
  ThriftTransport parent;

  /* protected */
  ThriftTransport *transport;

  /* private */
  GByteArray *r_buf;
  GByteArray *w_buf;
  guint32 r_buf_size;
  guint32 w_buf_size;
};
typedef struct _ThriftBufferedTransport ThriftBufferedTransport;

/*!
 * ThriftBufferedTransport class.
 */
struct _ThriftBufferedTransportClass
{
  ThriftTransportClass parent;
};
typedef struct _ThriftBufferedTransportClass ThriftBufferedTransportClass;

/* used by THRIFT_TYPE_BUFFERED_TRANSPORT */
GType thrift_buffered_transport_get_type (void);

#endif
