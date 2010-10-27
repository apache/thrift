#include "thrift.h"
#include "thrift_server.h"

/* object properties */
enum _ThriftServerProperties
{
  PROP_0,
  PROP_THRIFT_SERVER_PROCESSOR,
  PROP_THRIFT_SERVER_SERVER_TRANSPORT,
  PROP_THRIFT_SERVER_INPUT_TRANSPORT_FACTORY,
  PROP_THRIFT_SERVER_OUTPUT_TRANSPORT_FACTORY,
  PROP_THRIFT_SERVER_INPUT_PROTOCOL_FACTORY,
  PROP_THRIFT_SERVER_OUTPUT_PROTOCOL_FACTORY
};

/* forward declarations */
static void thrift_server_instance_init (ThriftServer *server);
static void thrift_server_class_init (ThriftServerClass *cls);
void thrift_server_get_property (GObject *object, guint property_id,
                                 GValue *value, GParamSpec *pspec);
void thrift_server_set_property (GObject *object, guint property_id,
                                 const GValue *value, GParamSpec *pspec);


/* define ThriftServerClass's type */
GType
thrift_server_get_type (void)
{
  static GType type = 0;

  if (type == 0)
  {
    static const GTypeInfo info =
    {
      sizeof (ThriftServerClass),
      NULL, /* base_init */
      NULL, /* base_finalize */
      (GClassInitFunc) thrift_server_class_init,
      NULL, /* class_finalize */
      NULL, /* class_data */
      sizeof (ThriftServer),
      0, /* n_preallocs */
      (GInstanceInitFunc) thrift_server_instance_init,
      NULL, /* value_table */
    };

    type = g_type_register_static (G_TYPE_OBJECT, "ThriftServer",
                                   &info, G_TYPE_FLAG_ABSTRACT);
  }

  return type;
}

/* instance initializer for Thrift Server */
static void
thrift_server_instance_init (ThriftServer *server)
{
  server->processor = NULL;
  server->server_transport = NULL;
  server->input_transport_factory = NULL;
  server->output_transport_factory = NULL;
  server->input_protocol_factory = NULL;
  server->output_protocol_factory = NULL;
}

/* class initializer for ThriftServer
 * TODO: implement ServerEventHandler as a GClosure
 */
static void
thrift_server_class_init (ThriftServerClass *cls)
{
  GObjectClass *gobject_class = G_OBJECT_CLASS (cls);

  gobject_class->get_property = thrift_server_get_property;
  gobject_class->set_property = thrift_server_set_property;

  g_object_class_install_property (gobject_class,
      PROP_THRIFT_SERVER_PROCESSOR,
      g_param_spec_object ("processor", "Processor", "Thrift Processor",
                           THRIFT_TYPE_PROCESSOR,
                           G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
  g_object_class_install_property (gobject_class,
      PROP_THRIFT_SERVER_SERVER_TRANSPORT,
      g_param_spec_object ("server_transport", "Server Transport",
                           "Thrift Server Transport",
                           THRIFT_TYPE_SERVER_TRANSPORT,
                           G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
  g_object_class_install_property (gobject_class,
      PROP_THRIFT_SERVER_INPUT_TRANSPORT_FACTORY,
      g_param_spec_object ("input_transport_factory", "Input Transport Factory",
                           "Thrift Server Input Transport Factory",
                           THRIFT_TYPE_TRANSPORT_FACTORY,
                           G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
  g_object_class_install_property (gobject_class,
      PROP_THRIFT_SERVER_OUTPUT_TRANSPORT_FACTORY,
      g_param_spec_object ("output_transport_factory",
                           "Output Transport Factory",
                           "Thrift Server Output Transport Factory",
                           THRIFT_TYPE_TRANSPORT_FACTORY,
                           G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
  g_object_class_install_property (gobject_class,
      PROP_THRIFT_SERVER_INPUT_PROTOCOL_FACTORY,
      g_param_spec_object ("input_protocol_factory", "Input Protocol Factory",
                           "Thrift Server Input Protocol Factory",
                           THRIFT_TYPE_PROTOCOL_FACTORY,
                           G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));
  g_object_class_install_property (gobject_class,
      PROP_THRIFT_SERVER_OUTPUT_PROTOCOL_FACTORY,
      g_param_spec_object ("output_protocol_factory", "Output Protocol Factory",
                           "Thrift Server Output Protocol Factory",
                           THRIFT_TYPE_PROTOCOL_FACTORY,
                           G_PARAM_READWRITE | G_PARAM_CONSTRUCT_ONLY));

  /* set these as virtual methods to be implemented by a subclass */
  cls->serve = thrift_server_serve;
  cls->stop = thrift_server_stop;
}

void
thrift_server_get_property (GObject *object, guint property_id,
                            GValue *value, GParamSpec *pspec)
{
  ThriftServer *server = THRIFT_SERVER (object);

  THRIFT_UNUSED_VAR (pspec);

  switch (property_id)
  {
    case PROP_THRIFT_SERVER_PROCESSOR:
      g_value_set_object (value, server->processor);
      break;
    case PROP_THRIFT_SERVER_SERVER_TRANSPORT:
      g_value_set_object (value, server->server_transport);
      break;
    case PROP_THRIFT_SERVER_INPUT_TRANSPORT_FACTORY:
      g_value_set_object (value, server->input_transport_factory);
      break;
    case PROP_THRIFT_SERVER_OUTPUT_TRANSPORT_FACTORY:
      g_value_set_object (value, server->output_transport_factory);
      break;
    case PROP_THRIFT_SERVER_INPUT_PROTOCOL_FACTORY:
      g_value_set_object (value, server->input_protocol_factory);
      break;
    case PROP_THRIFT_SERVER_OUTPUT_PROTOCOL_FACTORY:
      g_value_set_object (value, server->output_protocol_factory);
      break;
  }
}

void
thrift_server_set_property (GObject *object, guint property_id,
                            const GValue *value, GParamSpec *pspec)
{
  ThriftServer *server = THRIFT_SERVER (object);

  THRIFT_UNUSED_VAR (pspec);

  switch (property_id)
  {
    case PROP_THRIFT_SERVER_PROCESSOR:
      server->processor = g_value_get_object (value);
      break;
    case PROP_THRIFT_SERVER_SERVER_TRANSPORT:
      server->server_transport = g_value_get_object (value);
      break;
    case PROP_THRIFT_SERVER_INPUT_TRANSPORT_FACTORY:
      server->input_transport_factory = g_value_get_object (value);
      break;
    case PROP_THRIFT_SERVER_OUTPUT_TRANSPORT_FACTORY:
      server->output_transport_factory = g_value_get_object (value);
      break;
    case PROP_THRIFT_SERVER_INPUT_PROTOCOL_FACTORY:
      server->input_protocol_factory = g_value_get_object (value);
      break;
    case PROP_THRIFT_SERVER_OUTPUT_PROTOCOL_FACTORY:
      server->output_protocol_factory = g_value_get_object (value);
      break;
  }
}

void
thrift_server_serve (ThriftServer *server)
{
  THRIFT_SERVER_GET_CLASS (server)->serve (server);
}

void
thrift_server_stop (ThriftServer *server)
{
  THRIFT_SERVER_GET_CLASS (server)->stop (server);
}

