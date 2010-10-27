#ifndef _THRIFT_PROCESSOR_H
#define _THRIFT_PROCESSOR_H

#include <glib-object.h>

#include "protocol/thrift_protocol.h"

/*! \file thrift_processor.h
 *  \brief Abstract class for Thrift processors.
 */

/* type macros */	
#define THRIFT_TYPE_PROCESSOR (thrift_processor_get_type ())
#define THRIFT_PROCESSOR(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), \
                                   THRIFT_TYPE_PROCESSOR, ThriftProcessor))
#define THRIFT_IS_PROCESSOR(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), \
                                      THRIFT_TYPE_PROCESSOR))
#define THRIFT_PROCESSOR_CLASS(c) (G_TYPE_CHECK_CLASS_CAST ((c), \
                                       THRIFT_TYPE_PROCESSOR, \
                                       ThriftProcessorClass))
#define THRIFT_IS_PROCESSOR_CLASS(c) (G_TYPE_CHECK_CLASS_TYPE ((c), \
                                          THRIFT_TYPE_PROCESSOR))
#define THRIFT_PROCESSOR_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), \
                                             THRIFT_TYPE_PROCESSOR, \
                                             ThriftProcessorClass))

/*!
 * Thrift Processorobject
 */
struct _ThriftProcessor
{
  GObject parent;
};
typedef struct _ThriftProcessor ThriftProcessor;

/*!
 * Thrift Processor class
 */
struct _ThriftProcessorClass
{
  GObjectClass parent;

  /* vtable */
  gboolean (*process) (ThriftProcessor *processor, ThriftProtocol *in,
                       ThriftProtocol *out);
};
typedef struct _ThriftProcessorClass ThriftProcessorClass;

/* used by THRIFT_TYPE_PROCESSOR */
GType thrift_processor_get_type (void); 

/*!
 * Processes the request.
 * \public \memberof ThriftProcessorClass
 */
gboolean thrift_processor_process (ThriftProcessor *processor,
                                   ThriftProtocol *in, ThriftProtocol *out);

#endif /* _THRIFT_PROCESSOR_H */
