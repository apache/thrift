#include "thrift_struct.h"

static void
thrift_struct_class_init (ThriftStructClass *cls)
{
  ThriftStructClass *c = THRIFT_STRUCT_CLASS (cls);
  c->read = thrift_struct_read;
  c->write = thrift_struct_write;
}

GType
thrift_struct_get_type (void)
{
  static GType type = 0;

  if (type == 0)
  {
    static const GTypeInfo type_info =
      {
        sizeof (ThriftStructClass),
        NULL, /* base_init */
        NULL, /* base finalize */
        (GClassInitFunc) thrift_struct_class_init,
        NULL, /* class finalize */
        NULL, /* class data */
        sizeof (ThriftStruct),
        0, /* n_preallocs */
        NULL, /* instance_init */
        NULL, /* value_table */
      };

    type = g_type_register_static (G_TYPE_OBJECT,
                                   "ThriftStructType",
                                   &type_info, G_TYPE_FLAG_ABSTRACT);
  }

  return type;
}

gint32
thrift_struct_read (ThriftStruct *object, ThriftProtocol *protocol,
                    GError **error)
{
  g_return_val_if_fail (THRIFT_IS_STRUCT (object), -1);
  return THRIFT_STRUCT_GET_CLASS (object)->read (object, protocol, error);
}

gint32
thrift_struct_write (ThriftStruct *object, ThriftProtocol *protocol,
                     GError **error)
{
  g_return_val_if_fail (THRIFT_IS_STRUCT (object), -1);
  return THRIFT_STRUCT_GET_CLASS (object)->write (object, protocol, error);
}

