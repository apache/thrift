#ifndef _THRIFT_FRAMED_TRANSPORT_H
#define _THRIFT_FRAMED_TRANSPORT_H

#include <glib.h>
#include <glib-object.h>

#include "transport/thrift_transport.h"

/*! \file thrift_framed_transport.h
 *  \brief Implementation of a Thrift framed transport.  Subclasses 
 *         the ThriftTransport class.
 */

/* type macros */
#define THRIFT_TYPE_FRAMED_TRANSPORT (thrift_framed_transport_get_type ())
#define THRIFT_FRAMED_TRANSPORT(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), \
                                      THRIFT_TYPE_FRAMED_TRANSPORT, \
                                      ThriftFramedTransport))
#define THRIFT_IS_FRAMED_TRANSPORT(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), \
                                         THRIFT_TYPE_FRAMED_TRANSPORT))
#define THRIFT_FRAMED_TRANSPORT_CLASS(c) (G_TYPE_CHECK_CLASS_CAST ((c), \
                                          THRIFT_TYPE_FRAMED_TRANSPORT, \
                                          ThriftFramedTransportClass))
#define THRIFT_IS_FRAMED_TRANSPORT_CLASS(c) (G_TYPE_CHECK_CLASS_TYPE ((c), \
                                             THRIFT_TYPE_FRAMED_TRANSPORT)
#define THRIFT_FRAMED_TRANSPORT_GET_CLASS(obj) \
            (G_TYPE_INSTANCE_GET_CLASS ((obj), \
             THRIFT_TYPE_FRAMED_TRANSPORT, \
             ThriftFramedTransportClass))

/*!
 * ThriftFramedTransport instance.
 */
struct _ThriftFramedTransport
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
typedef struct _ThriftFramedTransport ThriftFramedTransport;

/*!
 * ThriftFramedTransport class.
 */
struct _ThriftFramedTransportClass
{
  ThriftTransportClass parent;
};
typedef struct _ThriftFramedTransportClass ThriftFramedTransportClass;

/* used by THRIFT_TYPE_FRAMED_TRANSPORT */
GType thrift_framed_transport_get_type (void);

#endif
