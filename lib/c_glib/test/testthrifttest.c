#include <assert.h>
#include <netdb.h>

#include <thrift/c_glib/transport/thrift_server_transport.h>
#include <thrift/c_glib/transport/thrift_server_socket.h>

static const char TEST_ADDRESS[] = "localhost";
static const int TEST_PORT = 64444;

static void
test_thrift_server (void)
{
  ThriftServerSocket *tsocket = g_object_new (THRIFT_TYPE_SERVER_SOCKET,
                                              "port", TEST_PORT, NULL);

  g_object_unref (tsocket);
}

int
main(int argc, char *argv[])
{
#if (!GLIB_CHECK_VERSION (2, 36, 0))
  g_type_init();
#endif

  g_test_init (&argc, &argv, NULL);

  g_test_add_func ("/testthrift/Server", test_thrift_server);

  return g_test_run ();
}
