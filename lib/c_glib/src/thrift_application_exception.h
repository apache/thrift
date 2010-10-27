#ifndef _THRIFT_APPLICATION_EXCEPTION_H
#define _THRIFT_APPLICATION_EXCEPTION_H

#include <glib-object.h>
#include "thrift_struct.h"

/*! \file thrift_application_exception.h
 *  \brief C Implementation of a TApplicationException.
 */

/* type macros */
#define THRIFT_TYPE_APPLICATION_EXCEPTION \
            (thrift_application_exception_get_type ())
#define THRIFT_APPLICATION_EXCEPTION(obj) \
            (G_TYPE_CHECK_INSTANCE_CAST ((obj), \
                                         THRIFT_TYPE_APPLICATION_EXCEPTION, \
                                         ThriftApplicationException))
#define THRIFT_IS_APPLICATION_EXCEPTION(obj) \
            (G_TYPE_CHECK_INSTANCE_TYPE ((obj), \
                                         THRIFT_TYPE_APPLICATION_EXCEPTION))
#define THRIFT_APPLICATION_EXCEPTION_CLASS(c) \
            (G_TYPE_CHECK_CLASS_CAST ((c), \
                                      THRIFT_TYPE_APPLICATION_EXCEPTION, \
                                      ThriftApplicationExceptionClass))
#define THRIFT_IS_APPLICATION_EXCEPTION_CLASS(c) \
            (G_TYPE_CHECK_CLASS_TYPE ((c), THRIFT_TYPE_APPLICATION_EXCEPTION))
#define THRIFT_APPLICATION_EXCEPTION_GET_CLASS(obj) \
            (G_TYPE_INSTANCE_GET_CLASS ((obj), \
                                        THRIFT_TYPE_APPLICATION_EXCEPTION, \
                                        ThriftApplicationExceptionClass))

struct _ThriftApplicationException
{
  ThriftStruct parent;

  /* public */
  gint32 type;
  gboolean __isset_type;
  gchar *message;
  gboolean __isset_message;
};
typedef struct _ThriftApplicationException ThriftApplicationException;

struct _ThriftApplicationExceptionClass
{
  ThriftStructClass parent;
};
typedef struct _ThriftApplicationExceptionClass ThriftApplicationExceptionClass;

GType thrift_application_exception_get_type (void);

/* gerror codes */
typedef enum
{
  THRIFT_APPLICATION_EXCEPTION_ERROR_UNKNOWN,
  THRIFT_APPLICATION_EXCEPTION_ERROR_UNKNOWN_METHOD,
  THRIFT_APPLICATION_EXCEPTION_ERROR_INVALID_MESSAGE_TYPE,
  THRIFT_APPLICATION_EXCEPTION_ERROR_WRONG_METHOD_NAME,
  THRIFT_APPLICATION_EXCEPTION_ERROR_BAD_SEQUENCE_ID,
  THRIFT_APPLICATION_EXCEPTION_ERROR_MISSING_RESULT
} ThriftApplicationExceptionError;

/* define error domain for GError */
GQuark thrift_application_exception_error_quark (void);
#define THRIFT_APPLICATION_EXCEPTION_ERROR (thrift_application_exception_error_quark ())

#endif /* _THRIFT_APPLICATION_EXCEPTION_H */
