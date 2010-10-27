#include <assert.h>
#include <glib-object.h>

#include "../src/thrift_struct.c"

/* tests to ensure we can extend a ThriftStruct */

struct _ThriftTestStruct
{
  ThriftStruct parent;
};
typedef struct _ThriftTestStruct ThriftTestStruct;

struct _ThriftTestStructClass
{
  ThriftStructClass parent;
};
typedef struct _ThriftTestStructClass ThriftTestStructClass;

GType thrift_test_struct_get_type (void);

#define THRIFT_TYPE_TEST_STRUCT (thrift_test_struct_get_type ())
#define THRIFT_TEST_STRUCT(obj) (G_TYPE_CHECK_INSTANCE_CAST ((obj), \
                                     THRIFT_TYPE_TEST_STRUCT, \
                                     ThriftTestStruct))
#define THRIFT_TEST_STRUCT_CLASS(c) (G_TYPE_CHECK_CLASS_CAST ((c), \
                                         THRIFT_TYPE_TEST_STRUCT, \
                                         ThriftTestStructClass))
#define THRIFT_IS_TEST_STRUCT(obj) (G_TYPE_CHECK_INSTANCE_TYPE ((obj), \
                                        THRIFT_TYPE_TEST_STRUCT))
#define THRIFT_IS_TEST_STRUCT_CLASS(c) (G_TYPE_CHECK_CLASS_TYPE ((c), \
                                            THRIFT_TYPE_TEST_STRUCT))
#define THRIFT_TEST_STRUCT_GET_CLASS(obj) (G_TYPE_INSTANCE_GET_CLASS ((obj), \
                                               THRIFT_TYPE_TEST_STRUCT, \
                                               ThriftTestStructClass))

/* test declarations */
gint32 thrift_test_struct_read (ThriftStruct *object, ThriftProtocol *protocol,
                                GError **error);
gint32 thrift_test_struct_write (ThriftStruct *object, ThriftProtocol *protocol,
                                 GError **error);

static void
thrift_test_struct_class_init (ThriftTestStructClass *cls)
{
  ThriftStructClass *ts_cls = THRIFT_STRUCT_CLASS (cls);
  ts_cls->read = thrift_test_struct_read;
  ts_cls->write = thrift_test_struct_write;
}

static void
thrift_test_struct_instance_init (ThriftTestStruct *s)
{
  (void) s;
}

GType
thrift_test_struct_get_type (void)
{
  static GType type = 0;

  if (type == 0)
  {
    static const GTypeInfo type_info =
    {
      sizeof (ThriftTestStructClass),
      NULL,
      NULL,
      (GClassInitFunc) thrift_test_struct_class_init,
      NULL,
      NULL,
      sizeof (ThriftTestStruct),
      0,
      (GInstanceInitFunc) thrift_test_struct_instance_init,
      NULL, 
    };

    type = g_type_register_static (THRIFT_TYPE_STRUCT,
                                   "ThriftTestStructType", &type_info, 0);
  }

  return type;
}

gint32
thrift_test_struct_read (ThriftStruct *object, ThriftProtocol *protocol,
                         GError **error)
{
  return 0;
}

gint32
thrift_test_struct_write (ThriftStruct *object, ThriftProtocol *protocol,
                          GError **error)
{
  return 0;
}


static void
test_initialize_object (void)
{
  ThriftTestStruct *t = NULL;

  t = g_object_new (THRIFT_TYPE_TEST_STRUCT, NULL);
  assert ( THRIFT_IS_STRUCT (t));
  thrift_struct_read (THRIFT_STRUCT (t), NULL, NULL);
  thrift_struct_write (THRIFT_STRUCT (t), NULL, NULL);
  thrift_test_struct_read (THRIFT_STRUCT (t), NULL, NULL);
  thrift_test_struct_write (THRIFT_STRUCT (t), NULL, NULL);
  g_object_unref (t);
}

int
main(void)
{
  g_type_init ();
  test_initialize_object ();

  return 0;
}
