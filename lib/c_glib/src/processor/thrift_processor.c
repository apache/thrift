#include "thrift.h"
#include "processor/thrift_processor.h"

/* forward declarations */
static void thrift_processor_class_init (ThriftProcessorClass *cls);

/* define ThriftProcessorClass's type */
GType
thrift_processor_get_type (void)
{
  static GType type = 0;

  if (type == 0)
  {
    static const GTypeInfo info =
    {
      sizeof (ThriftProcessorClass),
      NULL, /* base_init */
      NULL, /* base_finalize */
      (GClassInitFunc) thrift_processor_class_init,
      NULL, /* class_finalize */
      NULL, /* class_data */
      sizeof (ThriftProcessor),
      0, /* n_preallocs */
      NULL, /* instance_init */
      NULL, /* value_table */
    };

    type = g_type_register_static (G_TYPE_OBJECT, "ThriftProcessor",
                                   &info, G_TYPE_FLAG_ABSTRACT);
  }

  return type;
}

/* class initializer for ThriftProcessor */
static void
thrift_processor_class_init (ThriftProcessorClass *cls)
{
  /* set these as virtual methods to be implemented by a subclass */
  cls->process = thrift_processor_process;
}

gboolean 
thrift_processor_process (ThriftProcessor *processor, ThriftProtocol *in,
                          ThriftProtocol *out)
{
  return THRIFT_PROCESSOR_GET_CLASS (processor)->process (processor, in, out);
}

