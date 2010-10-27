#ifndef _THRIFT_MEMORY_BUFFER_H
#define _THRIFT_MEMORY_BUFFER_H

#include <glib.h>
#include <glib-object.h>

#include "transport/thrift_transport.h"

/*! \file thrift_memory_buffer.h
 *  \brief Implementation of a Thrift memory buffer transport. 
 */

/* type macros */
#define THRIFT_TYPE_MEMORY_BUFFER (thrift_memory_buffer_get_type ())
#define THRIFT_MEMORY_BUFFER(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), \
                                   THRIFT_TYPE_MEMORY_BUFFER, \
                                   ThriftMemoryBuffer))
#define THRIFT_IS_MEMORY_BUFFER(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), \
                                      THRIFT_TYPE_MEMORY_BUFFER))
#define THRIFT_MEMORY_BUFFER_CLASS(c) (G_TYPE_CHECK_CLASS_CAST ((c), \
                                       THRIFT_TYPE_MEMORY_BUFFER, \
                                       ThriftMemoryBufferClass))
#define THRIFT_IS_MEMORY_BUFFER_CLASS(c) (G_TYPE_CHECK_CLASS_TYPE ((c), \
                                          THRIFT_TYPE_MEMORY_BUFFER)
#define THRIFT_MEMORY_BUFFER_GET_CLASS(obj) \
            (G_TYPE_INSTANCE_GET_CLASS ((obj), \
             THRIFT_TYPE_MEMORY_BUFFER, \
             ThriftMemoryBufferClass))

/*!
 * ThriftMemoryBuffer instance.
 */
struct _ThriftMemoryBuffer
{
  ThriftTransport parent;

  /* private */
  GByteArray *buf;
  guint32 buf_size;
};
typedef struct _ThriftMemoryBuffer ThriftMemoryBuffer;

/*!
 * ThriftMemoryBuffer class.
 */
struct _ThriftMemoryBufferClass
{
  ThriftTransportClass parent;
};
typedef struct _ThriftMemoryBufferClass ThriftMemoryBufferClass;

/* used by THRIFT_TYPE_MEMORY_BUFFER */
GType thrift_memory_buffer_get_type (void);

#endif
