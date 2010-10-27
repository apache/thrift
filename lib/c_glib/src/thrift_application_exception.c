#include "thrift_application_exception.h"
#include "protocol/thrift_protocol.h"

/* forward declarations */
void thrift_application_exception_instance_init (ThriftApplicationException *object);
void thrift_application_exception_class_init (ThriftStructClass *cls);
gint32 thrift_application_exception_read (ThriftStruct *object, ThriftProtocol *protocol, GError **error);
gint32 thrift_application_exception_write (ThriftStruct *object, ThriftProtocol *protocol, GError **error);

GType
thrift_application_exception_get_type (void)
{
  static GType type = 0;

  if (type == 0)
  {
    static const GTypeInfo type_info =
    {
      sizeof (ThriftApplicationExceptionClass),
      NULL, /* base_init */
      NULL, /* base_finalize */
      (GClassInitFunc) thrift_application_exception_class_init,
      NULL, /* class_finalize */
      NULL, /* class_data */
      sizeof (ThriftApplicationException),
      0, /* n_preallocs */
      (GInstanceInitFunc) thrift_application_exception_instance_init,
      NULL, /* value_table */
    };

    type = g_type_register_static (THRIFT_TYPE_STRUCT,
                                   "ThriftApplicationExceptionType",
                                   &type_info, 0);
  }
  return type;
}

void
thrift_application_exception_instance_init (ThriftApplicationException *object)
{
  object->type = 0;
  object->__isset_type = FALSE;
  object->message = NULL;
  object->__isset_message = FALSE;
}

void
thrift_application_exception_class_init (ThriftStructClass *cls)
{
  cls->read = thrift_application_exception_read;
  cls->write = thrift_application_exception_write;
}

gint32
thrift_application_exception_read (ThriftStruct *object,
                                   ThriftProtocol *protocol, GError **error)
{
  gint32 ret;
  gint32 xfer = 0;
  gchar *name;
  ThriftType ftype;
  gint16 fid;
  ThriftApplicationException *this = THRIFT_APPLICATION_EXCEPTION (object);

  /* read the struct begin marker */
  if ((ret = thrift_protocol_read_struct_begin (protocol, &name, error)) < 0)
  {
    if (name) g_free (name);
    return -1;
  }
  xfer += ret;
  if (name) g_free (name);

  while (1)
  {
    if ((ret = thrift_protocol_read_field_begin (protocol, &name, &ftype,
                                                 &fid, error)) < 0)
    {
      if (name) g_free (name);
      return -1;
    }
    xfer += ret;
    if (name) g_free (name);

    /* break if we get a STOP field */
    if (ftype == T_STOP)
    {
      break;
    }

    switch (fid)
    {
      case 1:
        if (ftype == T_STRING)
        {
          if ((ret = thrift_protocol_read_string (protocol, &this->message,
                                                  error)) < 0)
            return -1;
          xfer += ret;
          this->__isset_message = TRUE;
        } else {
          if ((ret = thrift_protocol_skip (protocol, ftype, error)) < 0)
            return -1;
          xfer += ret;
        }
        break;
      case 2:
        if (ftype == T_I32)
        {
          if ((ret = thrift_protocol_read_i32 (protocol, &this->type,
                                               error)) < 0)
            return -1;
          xfer += ret;
          this->__isset_type = TRUE;
        } else {
          if ((ret = thrift_protocol_skip (protocol, ftype, error)) < 0)
            return -1;
          xfer += ret;
        }
        break;
      default:
        if ((ret = thrift_protocol_skip (protocol, ftype, error)) < 0)
          return -1;
        xfer += ret;
        break;
    }
    if ((ret = thrift_protocol_read_field_end (protocol, error)) < 0)
      return -1;
    xfer += ret;
  }

  if ((ret = thrift_protocol_read_struct_end (protocol, error)) < 0)
    return -1;
  xfer += ret;

  return xfer;
}

gint32
thrift_application_exception_write (ThriftStruct *object,
                                    ThriftProtocol *protocol, GError **error)
{
  gint32 ret;
  gint32 xfer = 0;

  ThriftApplicationException *this = THRIFT_APPLICATION_EXCEPTION (object);

  if ((ret = thrift_protocol_write_struct_begin (protocol,
                                                 "TApplicationException",
                                                 error)) < 0)
    return -1;
  xfer += ret;
  if ((ret = thrift_protocol_write_field_begin (protocol, "message",
                                                T_STRING, 1, error)) < 0)
    return -1;
  xfer += ret;
  if ((ret = thrift_protocol_write_string (protocol, this->message, error)) < 0)
    return -1;
  xfer += ret;
  if ((ret = thrift_protocol_write_field_end (protocol, error)) < 0)
    return -1;
  xfer += ret;
  if ((ret = thrift_protocol_write_field_begin (protocol, "type",
                                                T_I32, 2, error)) < 0)
    return -1;
  xfer += ret;
  if ((ret = thrift_protocol_write_i32 (protocol, this->type, error)) < 0)
    return -1;
  xfer += ret;
  if ((ret = thrift_protocol_write_field_end (protocol, error)) < 0)
    return -1;
  xfer += ret;
  if ((ret = thrift_protocol_write_field_stop (protocol, error)) < 0)
    return -1;
  xfer += ret;
  if ((ret = thrift_protocol_write_struct_end (protocol, error)) < 0)
    return -1;
  xfer += ret;

  return xfer;
}


/* GError domain */
#define THRIFT_APPLICATION_EXCEPTION_ERROR_DOMAIN "thrift-application-exception-error-quark"

GQuark
thrift_application_exception_error_quark (void)
{
  return g_quark_from_static_string (THRIFT_APPLICATION_EXCEPTION_ERROR_DOMAIN);
}

