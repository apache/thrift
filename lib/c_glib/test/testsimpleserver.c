#include <assert.h>
#include <glib.h>
#include <stdlib.h>
#include <unistd.h>
#include <sys/types.h>
#include <sys/wait.h>

#include "processor/thrift_processor.h"
#include "transport/thrift_server_socket.h"

#define TEST_PORT 51199

#include "server/thrift_simple_server.c"

/* create a rudimentary processor */
#define TEST_PROCESSOR_TYPE (test_processor_get_type ())

struct _TestProcessor
{
  ThriftProcessor parent;
};
typedef struct _TestProcessor TestProcessor;

struct _TestProcessorClass
{
  ThriftProcessorClass parent;
};
typedef struct _TestProcessorClass TestProcessorClass;

gboolean
test_processor_process (ThriftProcessor *processor, ThriftProtocol *in,
                        ThriftProtocol *out)
{
  return FALSE;
}

static void
test_processor_class_init (ThriftProcessorClass *proc)
{
  proc->process = test_processor_process;
}

GType
test_processor_get_type (void)
{
  static GType type = 0;

  if (type == 0)
  {
    static const GTypeInfo info =
    {
      sizeof (TestProcessorClass),
      NULL, /* base_init */
      NULL, /* base_finalize */
      (GClassInitFunc) test_processor_class_init,
      NULL, /* class_finalize */
      NULL, /* class_data */
      sizeof (TestProcessor),
      0, /* n_preallocs */
      NULL, /* instance_init */
      NULL, /* value_table */
    };

    type = g_type_register_static (THRIFT_TYPE_PROCESSOR,
                                   "TestProcessorType",
                                   &info, 0);
  }

  return type;
}

static void
test_server (void)
{
  int status;
  pid_t pid;
  TestProcessor *p = NULL;
  ThriftServerSocket *tss = NULL;
  ThriftSimpleServer *ss = NULL;

  p = g_object_new (TEST_PROCESSOR_TYPE, NULL);
  tss = g_object_new (THRIFT_TYPE_SERVER_SOCKET, "port", TEST_PORT, NULL);
  ss = g_object_new (THRIFT_TYPE_SIMPLE_SERVER, "processor", p,
                     "server_transport", THRIFT_SERVER_TRANSPORT (tss), NULL);

  /* run the server in a child process */
  pid = fork ();
  assert (pid >= 0);

  if (pid == 0)
  {
    THRIFT_SERVER_GET_CLASS (THRIFT_SERVER (ss))->serve (THRIFT_SERVER (ss));
    exit (0);
  } else {
    sleep (5);
    kill (pid, SIGINT);

    g_object_unref (ss);
    g_object_unref (tss);
    g_object_unref (p);
    assert (wait (&status) == pid);
    assert (status == SIGINT);
  }
}

int
main (void)
{
  g_type_init ();
  test_server ();

  return 0;
}
